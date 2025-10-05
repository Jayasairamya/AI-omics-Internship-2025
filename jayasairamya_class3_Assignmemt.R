getwd()

# ctrl + entr


setwd("Path to your folder")

setwd("C:/Users/admin/Downloads/AI OMICS Internship 2025")

setwd("C:\\Users\\admin\\Downloads\\AI OMICS Internship 2025")


if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")

# Install Bioconductor packages
BiocManager::install(c("GEOquery","affy","arrayQualityMetrics"))

# Install CRAN packages for data manipulation
install.packages("dplyr")


# Load Required Libraries
library(GEOquery)             # Download GEO datasets (series matrix, raw CEL files)
library(affy)                 # Pre-processing of Affymetrix microarray data (RMA normalization)
library(arrayQualityMetrics)  # QC reports for microarray data
library(dplyr)                # Data manipulation


#### Download Series Matrix Files ####
# -------------------------------------

# Series matrix files are preprocessed text files containing 
# expression values, sample annotations, and probe information.
# Reason: Useful for quick exploratory analysis when raw CEL files are not needed.

gse_data <- getGEO("GSE15852", GSEMatrix = TRUE)


# Extract expression data matrix (genes/probes × samples)
# Rows corresponds to probes and columns corresponds to samples

expression_data <- exprs(gse_data$GSE15852_series_matrix.txt.gz)


# Extract feature (probe annotation) data
# Rows corresponds to probes and columns corresponds to samples

feature_data <-  fData(gse_data$GSE15852_series_matrix.txt.gz)


# Extract phenotype (sample metadata) data
# Rows corresponds to samples and columns corresponds to probes
phenotype_data <-  pData(gse_data$GSE15852_series_matrix.txt.gz)

# Check missing values in sample annotation
sum(is.na(phenotype_data$source_name_ch1)) 


#### Download Raw Data (CEL files) ####

# CEL files contain raw probe-level intensity values for affymetrix platform.
# Raw data required full preprocessing (e.g., RMA normalization, QC)

# CEL files are large, and downloads may fail even with a good connection. 
#It's recommended to download raw data directly from NCBI GEO

# skip this step if you already downloaded data from NCBI

# Fetch GEO supplementry files
getGEOSuppFiles("GSE15852", baseDir = "Raw_Data", makeDirectory = TRUE)
                
untar("Raw_Data/GSE15852/GSE15852_RAW.tar", exdir = "Raw_Data/CEL_Files")             
getwd()
list.files("Raw_Data")


# Read CEL files into R as an AffyBatch object
raw_data <- ReadAffy(celfile.path = "Raw_Data/CEL_Files")
raw_data# Displays basic information about the dataset
# Note down the annotation (e.g annotation=hgu133a) from this output.
# You will need this in the next step to select and install the correct
# annotation package (e.g.,hgu133a.db) for mapping probe IDs to genes.

# ---------------------------------------------------
#### Quality Control (QC) Before Pre-processing ####
# ---------------------------------------------------
# QC identifies outlier arrays, hybridization problems, or technical biases.
# arrayQualityMetrics: # This package generates automated QC reports for microarray data.
# It applies multiple complementary methods to detect technical issues:
#   - Boxplots and density plots: check distribution of intensities 
#   - MA-plots: visualize systematic biases between arrays 
#   - Heatmaps and distance matrices: identify clustering/outliers
#   - PCA: detect unusual variation/detecting outliers or batch effects
#
# The output is an interactive HTML report (index.html file) summarizing QC results

arrayQualityMetrics(expressionset = raw_data,
                    outdir = "Results/QC_Raw_Data",
                    force = TRUE,
                    do.logtransform = TRUE)



# -------------------------------------------------------
#### RMA (Robust Multi-array Average) Normalization ####
# -------------------------------------------------------

# RMA is a popular method for normalizing Affymetrix microarray data by:
# 1. Background correcting, 
# 2. normalizing probe intensities using quantile normalization and 
# 3. summarizing them into gene-level expression values using a robust median polish algorithm.

# This method reduces experimental variation across multiple arrays, 
# producing more symmetrical and reliable normalized expression data 
# compared to other approaches


normalized_data <- rma(raw_data)


# QC after data normalization 
arrayQualityMetrics(expressionset = normalized_data[,c(23,27,32,34,36,40,41,42,43,47,76,79,81,82,84,86)],
                    outdir = "Results/QC_Normalized_Data",
                    reporttitle = "QC Report for Normalized Data",
                    force = TRUE)


# Extract normalized expression values into a data frame
processed_data <- as.data.frame(exprs(normalized_data))
dim(processed_data)


# ---------------------------------------------------------------------------
#### Filter Low-Variance Transcripts (“soft” intensity based filtering) ####
# ---------------------------------------------------------------------------

# Filtering removes probes with low or uninformative expression signals.
# Reason: Reduces noise and improves statistical power in differential expression & Machine Learning.

# Calculate median intensity per probe across samples
row_median <- rowMedians(as.matrix(processed_data))

row_median


# Visualize distribution of probe median intensities

hist(row_median,
     breaks = 100,
     freq = FALSE,
     main = "Median Intensity Distribution")


# Set a threshold to remove low variance probes (dataset-specific, adjust accordingly)
threshold <- 9

abline(v = threshold, col = "black", lwd = 2) 


# Select probes above threshold

indx <- row_median > threshold 

filtered_data <- processed_data[indx, ] 

# Rename filtered expression data with sample metadata
colnames(filtered_data) <- rownames(phenotype_data)

# Overwrite processed data with filtered dataset

processed_data <- filtered_data 

# -----------------------------------
#### Phenotype Data Preparation ####
# -----------------------------------

# Phenotype data contains sample-level metadata such as condition, 
# tissue type, or disease status.
# Required to define experimental groups for statistical analysis.

class(phenotype_data$source_name_ch1) 

# Define experimental groups (normal vs cancer)

groups <- factor(phenotype_data$source_name_ch1,
                 levels = c("normal breast tissue","breast tumor tissue"),
                 label = c("normal", "cancer"))

class(groups)
levels(groups)
save.image("my_workspace.RData")
load("my_workspace.RData")
