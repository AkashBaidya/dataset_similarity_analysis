install.packages("mlbench")
library(mlbench)

# Load 10 example datasets
data_list <- list()
data_names <- listDatasets()[1:10]  # Select the first 10 datasets
for (name in data_names) {
  dataset <- get(name)
  data_list[[name]] <- dataset$data
}

# Calculate Euclidean distance between datasets
distance_matrix <- matrix(NA, nrow = length(data_list), ncol = length(data_list))
rownames(distance_matrix) <- data_names
colnames(distance_matrix) <- data_names

for (i in 1:length(data_list)) {
  for (j in 1:length(data_list)) {
    distance_matrix[i, j] <- sqrt(sum((data_list[[i]] - data_list[[j]])^2))
  }
}

# Print distance matrix
print(distance_matrix)
install.packages("mlbench")
library(mlbench)

install.packages("mlbench")
library(mlbench)

# Load example datasets from the mlbench package
data_list <- lapply(data_names, function(name) {
  dataset <- get(name, envir = .GlobalEnv)
  as.data.frame(dataset)
})

# Calculate Euclidean distance between datasets
distance_matrix <- matrix(NA, nrow = length(data_list), ncol = length(data_list))
rownames(distance_matrix) <- data_names
colnames(distance_matrix) <- data_names

for (i in 1:length(data_list)) {
  for (j in 1:length(data_list)) {
    distance_matrix[i, j] <- sqrt(sum((data_list[[i]] - data_list[[j]])^2))
  }
}

#


install.packages("openml")
library(OpenML)

classification_datasets <- listOMLDataSets(data.name  = "classification")

classification_datasets <- listOMLDataSets(tag = "Medicine")
classification_datasets <- listOMLDataSets(data.name ="Cancer")

saveOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")
dataset_info <- getOMLDataSet(data.id=45090)



# Install and load necessary packages
install.packages("OpenML")
library(OpenML)

# Install and load necessary packages
install.packages("OpenML")
library(OpenML)
# Install and load necessary packages
install.packages("OpenML")
library(OpenML)

# Define dataset IDs
dataset_ids <- c(
  3, 6, 12, 14, 16, 18, 21, 22, 23, 24, 26, 28, 30, 31, 32, 36, 38, 44, 46,
  57, 60, 179, 180, 181, 182, 184, 185, 273, 293, 300, 351, 354, 357, 389,
  390, 391, 392, 393, 395, 396, 398, 399, 401, 554, 679, 715, 718, 720, 722,
  723, 727, 728, 734, 735, 737, 740, 741, 743, 751, 752, 761, 772, 797, 799,
  803, 806, 807, 813, 816, 819, 821, 822, 823, 833, 837, 843, 845, 846, 847,
  849, 866, 871, 881, 897, 901, 903, 904, 910, 912, 913, 914, 917, 923, 930,
  934, 953, 958, 959, 962, 966, 971, 976, 977, 978, 979, 980, 991, 993, 995,
  1000, 1002, 1018, 1019, 1020, 1021, 1036, 1040, 1041, 1049, 1050, 1053,
  1056, 1067, 1068, 1069, 1111, 1112, 1114, 1116, 1119, 1120, 1128, 1130,
  1134, 1138, 1139, 1142, 1146, 1161, 1166
)

# Get tasks for supervised classification
tasks <- listOMLTasks(task.type = "Supervised Classification", status = "all")

# Filter tasks with holdout resampling strategy
tasks <- subset(tasks, estimation.procedure == "33% Holdout set")

# Initialize empty vector to store task IDs
task_ids <- c()

# Iterate over dataset IDs
for (did in dataset_ids) {
  tasks_ <- subset(tasks, data.id == did)
  if (nrow(tasks_) >= 1) {
    task_id <- min(tasks_$task.id)
    task_ids <- c(task_ids, task_id)
    print(task_id)

  } else {
    stop(paste("No task found for dataset ID:", did))
  }
}


# Check if the length of task IDs is equal to 140
if (length(task_ids) != 140) {
  stop("The number of task IDs is not equal to 140.")
}

# Sort task IDs
task_ids <- sort(task_ids)

# Print the resulting task IDs
print(task_ids)

length(task_ids)


install.packages("openml")
library(openml)

# Search for datasets in bioinformatics with numeric features
bioinformatics_datasets <- listOMLDataSets(tag = "bioinformatics")

# Print the datasets found
print(bioinformatics_datasets)


install.packages("openml")
library(openml)

install.packages("openml")
library(openml)

# Retrieve a list of datasets
all_datasets <- listOMLDataSets()

# Filter datasets with numeric features and potentially relevant to bioinformatics
bioinformatics_datasets <- all_datasets[grep("bioinformatics", all_datasets$name, ignore.case = TRUE),]
bioinformatics_numeric_datasets <- subset(bioinformatics_datasets, nrNumericFeatures > 0)

# Print the filtered datasets
print(bioinformatics_numeric_datasets)


install.packages("openml")
library(openml)

# Retrieve a list of datasets
all_datasets <- listOMLDataSets()

# Filter datasets with numeric features and potentially relevant to bioinformatics
bioinformatics_datasets <- all_datasets[grep("bioinformatics", all_datasets$name, ignore.case = TRUE),]
bioinformatics_numeric_datasets <- subset(bioinformatics_datasets)

# Print the filtered datasets
print(bioinformatics_numeric_datasets)


install.packages("openml")
library(openml)

# Retrieve a list of datasets
all_datasets <- listOMLDataSets()

# Filter datasets with numeric features
numeric_datasets <- lapply(all_datasets$data.list, function(dataset) {
  if ("numeric" %in% dataset$feature.types) {
    return(dataset)
  }
})
numeric_datasets <- Filter(Negate(is.null), numeric_datasets)

# Filter datasets potentially relevant to bioinformatics
bioinformatics_numeric_datasets <- lapply(numeric_datasets, function(dataset) {
  if (grepl("bioinformatics", dataset$name, ignore.case = TRUE)) {
    return(dataset)
  }
})
bioinformatics_numeric_datasets <- Filter(Negate(is.null), bioinformatics_numeric_datasets)

# Print the filtered datasets
print(bioinformatics_numeric_datasets)



install.packages("openml")
library(openml)

# Search for a dataset with the desired columns
target_dataset_id <- 1485  # You need to replace this with the ID of the dataset you found
target_dataset <- getOMLDataSet(target_dataset_id)

# Replicate the dataset to create 10 copies
number_of_copies <- 10
replicated_datasets <- list()
for (i in 1:number_of_copies) {
  replicated_datasets[[i]] <- target_dataset
}

# Optionally, you can rename the datasets if needed
for (i in 1:number_of_copies) {
  replicated_datasets[[i]]$name <- paste0(target_dataset$name, "_copy_", i)
}

# Print the replicated datasets
for (i in 1:number_of_copies) {
  print(replicated_datasets[[i]])
}

install.packages("GEOquery")
library(GEOquery)

gse <- getGEO("GSE10245")



columns <- colnames(exprs(gse[[1]]))
print(columns)

gse <- getGEO("GSE10245", GSEMatrix = TRUE)
# Extract the expression matrix
expression_data <- exprs(gse[[1]])

# Extract metadata
metadata <- pData(gse[[1]])

gse <- getGEO("GSE10245")



columns <- colnames(exprs(gse[[1]]))
print(columns)

gse <- getGEO("GSE10245", GSEMatrix = TRUE)
# Extract the expression matrix
expression_data <- exprs(gse[[1]])

# Extract metadata
metadata <- pData(gse[[1]])



library(GEOquery)
library(GEOquery)

# Function to check if datasets have the same column names
check_column_names <- function(datasets) {
  # Extract column names from the first dataset
  reference_colnames <- colnames(datasets[[1]])
  
  # Iterate over remaining datasets
  for (i in 2:length(datasets)) {
    current_colnames <- colnames(datasets[[i]])
    
    # Check if column names are identical
    if (!identical(reference_colnames, current_colnames)) {
      return(FALSE)  # Return FALSE if column names are different
    }
  }
  
  return(TRUE)  # Return TRUE if all column names are identical
}

# Function to load datasets and check column names
load_and_check_datasets <- function(dataset_accessions) {
  datasets <- list()
  
  # Load datasets
  for (accession in dataset_accessions) {
    dataset <- getGEO(accession, GSEMatrix = TRUE)[[1]]
    datasets[[accession]] <- dataset
  }
  
  # Check column names
  if (check_column_names(datasets)) {
    print("All datasets have the same column names.")
  } else {
    print("Datasets have different column names.")
  }
}

# List of dataset accessions to check
dataset_accessions <- c("GSE10245", "GSE62944", "GSE45827")  # Add more accessions as needed

# Load and check datasets
load_and_check_datasets(dataset_accessions)


data(airways)



library(GEOquery)

# Function to check if datasets have the same column names
check_column_names <- function(datasets) {
  # Extract column names from the first dataset
  reference_colnames <- colnames(datasets[[1]])
  
  # Iterate over remaining datasets
  for (i in 2:length(datasets)) {
    current_colnames <- colnames(datasets[[i]])
    
    # Check if column names are identical
    if (!identical(reference_colnames, current_colnames)) {
      return(FALSE)  # Return FALSE if column names are different
    }
  }
  
  return(TRUE)  # Return TRUE if all column names are identical
}

# Function to load datasets from GEO by accession
load_datasets <- function(dataset_accessions) {
  datasets <- list()
  
  for (accession in dataset_accessions) {
    dataset <- getGEO(accession, GSEMatrix = TRUE)[[1]]
    datasets[[accession]] <- dataset
  }
  
  return(datasets)
}

# Function to find datasets with identical column names
find_datasets_with_identical_columns <- function(search_term, num_datasets) {
  # Search GEO for datasets matching the search term
  search_results <- getGEO(search_terms = search_term, limit = num_datasets)
  
  # Load datasets
  dataset_accessions <- sapply(search_results, function(x) attr(x, "GEOAccNum"))
  datasets <- load_datasets(dataset_accessions)
  
  # Check column names
  identical_columns_datasets <- list()
  for (dataset_accession in dataset_accessions) {
    if (check_column_names(datasets[[dataset_accession]])) {
      identical_columns_datasets[[dataset_accession]] <- datasets[[dataset_accession]]
    }
  }
  
  return(identical_columns_datasets)
}

# Example usage:
search_term <- "breast cancer"
num_datasets <- 10
identical_columns_datasets <- find_datasets_with_identical_columns(search_term, num_datasets)

# Print the accessions of datasets with identical column names
print(names(identical_columns_datasets))


library(GEOquery)

# Function to check if datasets have the same column names
check_column_names <- function(datasets) {
  reference_colnames <- colnames(datasets[[1]])
  all(sapply(datasets, function(dataset) identical(reference_colnames, colnames(dataset))))
}

# Function to load datasets by accession numbers
load_datasets <- function(dataset_accessions) {
  datasets <- list()
  for (accession in dataset_accessions) {
    dataset <- getGEO(accession, GSEMatrix = TRUE)[[1]]
    datasets[[accession]] <- dataset
  }
  datasets
}

# Function to find datasets with identical column names
find_datasets_with_identical_columns <- function(dataset_accessions) {
  datasets <- load_datasets(dataset_accessions)
  identical_columns_datasets <- list()
  for (accession in dataset_accessions) {
    if (check_column_names(datasets)) {
      identical_columns_datasets[[accession]] <- datasets[[accession]]
    }
  }
  identical_columns_datasets
}

# Example usage:
dataset_accessions <- c("GSE10245", "GSE62944", "GSE45827")

identical_columns_datasets <- find_datasets_with_identical_columns(dataset_accessions)

# Print the accessions of datasets with identical column names
print(names(identical_columns_datasets))



# Set your API key
setOMLConfig(apikey ="c1994bdb7ecb3c6f3c8f3b35f4b47f1f")  # Replace 'your_api_key' with your actual API key

# Retrieve dataset
dataset_id <-45094
dataset <- getOMLDataSet(dataset_id)

# Load dataset into R
data <- read.table(dataset$data_file)

# Display the first few rows of the dataset
head(data)
# Install the OpenML package
install.packages("OpenML")

# Load the OpenML package
# Load the OpenML package
library(OpenML)

# Set your API key
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")

# Retrieve dataset information
dataset_info <- getOMLDataSet(data.id = 45090)

# Get columns information
columns <- dataset_info$data$
# Display columns information
print(columns)


