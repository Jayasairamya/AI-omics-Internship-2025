# ===================================================================
# Assignment 2: Classification of Differentially Expressed Genes (DEGs)
# ===================================================================

# ----------------------------
# Step 1: Define classification function
# ----------------------------
classify_gene <- function(logFC, padj) {
  # Replace NA padj with 1
  if (is.na(padj)) {
    padj <- 1
  }
  
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# ----------------------------
# Step 2: Define input/output
# ----------------------------
input_dir <- "Raw_Data"
output_dir <- "Results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

# ----------------------------
# Step 3: Loop through datasets
# ----------------------------
deg_results <- list()   # store results inside R

for (file_name in files_to_process) {
  cat("\nProcessing:", file_name, "\n")
  
  # Import dataset
  data <- read.csv(file.path(input_dir, file_name), header = TRUE)
  
  # Replace missing padj values with 1
  data$padj[is.na(data$padj)] <- 1
  
  # Apply classification function row-wise
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  
  # Save results in R list
  deg_results[[file_name]] <- data
  
  # Save processed dataset into Results folder
  output_file <- file.path(output_dir, paste0("Processed_", file_name))
  write.csv(data, output_file, row.names = FALSE)
  cat("Results saved to:", output_file, "\n")
  
  # Print summary counts
  cat("Summary counts:\n")
  print(table(data$status))
}

# ----------------------------
# Step 4: Access results if needed
# ----------------------------
results_deg1 <- deg_results[[1]]
results_deg2 <- deg_results[[2]]



# List all files in Raw_Data folder
list.files("Raw_Data")

# Read the first input file
input1 <- read.csv("Raw_Data/DEGs_Data_1.csv", header = TRUE)

# Preview first 5 rows
head(input1)

# Check structure (columns and datatypes)
str(input1)

# See how many rows and columns
dim(input1)


# Check DEGs_Data_2.csv
input2 <- read.csv("Raw_Data/DEGs_Data_2.csv", header = TRUE)

# Preview first few rows
head(input2)

# Structure of file
str(input2)

# Rows and columns
dim(input2)





# List all processed files
list.files("Results")

# Read one processed output file
output1 <- read.csv("Results/Processed_DEGs_Data_1.csv", header = TRUE)

# Preview first few rows
head(output1)

# Check if the new "status" column exists
table(output1$status)



# List all processed files
list.files("Results")

# Read one processed output file
output1 <- read.csv("Results/Processed_DEGs_Data_2.csv", header = TRUE)

# Preview first few rows
head(output2)

# Check if the new "status" column exists
table(output2$status)

list.files("Results")


