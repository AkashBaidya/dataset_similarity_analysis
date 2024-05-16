# install.packages("GEOquery")
library(GEOquery)
datasets <- c("GSE50948", "GSE30543", "GSE50697", "GSE46924", "GSE31192", "GSE35696",
              "GSE35511", "GSE28645", "GSE33658", "GSE22865", "GSE32474", "GSE16179",
              "GSE32161", "GSE24249", "GSE27444", "GSE29327", "GSE28448", "GSE27515",
              "GSE22600", "GSE26298", "GSE27473", "GSE21422", "GSE28274", "GSE26910",
              "GSE27567", "GSE27018", "GSE19777", "GSE22035", "GSE26459", "GSE22513",
              "GSE15481", "GSE13477", "GSE11506", "GSE11352", "GSE11324", "GSE8565",
              "GSE10270", "GSE9586", "GSE6800", "GSE8597")


# Download each dataset separately
geo_data_list <- lapply(datasets, function(dataset) getGEO(dataset,GSEMatrix = TRUE))
dim(pData(phenoData(geo_data_list[[1]][[1]])))

dim(pData(phenoData(geo_data_list[[1]])))

a<-apply((pData(phenoData(geo_data_list[[1]][[1]]))), 2, table)

# Extract phenotype data and store as tables
phenotype_tables <- lapply(geo_data_list, function(data) {
  pData_list <- pData(phenoData(data[[1]]))
  sapply(pData_list, table)
})



# Download the dataset
geo_data <- getGEO(datasets)

# Check if the dataset was downloaded successfully
if (length(geo_data) > 0) {
  cat("Dataset", dataset, "was downloaded successfully.\n")
} else {
  cat("Failed to download dataset", dataset, ".\n")
}
geo_data_list[1]



dataGSE50948 <- getGEO("GSE50948", GSEMatrix = TRUE)


dim(pData(phenoData(dataGSE50948[[1]])))

f<-apply((pData(phenoData(dataGSE50948[[1]]))), 2, table)
dim(pData(phenoData(dataGSE50948[[1]])))

a<-apply((pData(phenoData(dataGSE50948[[1]]))), 2, table)

dataGSE50948[[1]]$metadata

a<-phenoData(dataGSE50948[[1]])

# Load necessary library
library(GEOquery)

# Define the datasets
datasets <- c("GSE60518", "GSE56843", "GSE43346", "GSE44723", "GSE32474", "GSE21411", 
              "GSE19804", "GSE10245", "GSE13525", "GSE9586", "GSE9008", "GSE5579", 
              "GSE6013", "GSE6400", "GSE5059")

# Function to extract clinical variables from a GEO dataset
get_clinical_variables <- function(dataset) {
  # Download dataset
  geo_data <- getGEO(dataset, GSEMatrix = TRUE)
  
  # Extract clinical variables
  clinical_data <- pData(phenoData(geo_data[[1]]))
  clinical_variables <- colnames(clinical_data)
  
  return(clinical_variables)
}

# Apply the function to each dataset
clinical_variables_list <- lapply(datasets, get_clinical_variables)

# Calculate the frequency of each clinical variable across datasets
clinical_variables_freq <- table(unlist(clinical_variables_list))

# Filter for variables measured in the majority of datasets (e.g., in at least 50% of datasets)
majority_variables <- names(clinical_variables_freq[clinical_variables_freq >= length(datasets)/2])

# Print the identified clinical variables
print(majority_variables)

# Check if any clinical variables were identified
if (length(clinical_variables_freq) > 0) {
  # Check if each variable is measured in the majority of datasets
  measured_in_majority <- names(clinical_variables_freq) %in% majority_variables
  
  # Create a data frame to summarize the information
  summary_table <- data.frame(Variable = names(clinical_variables_freq),
                              Frequency = as.numeric(clinical_variables_freq),
                              Measured_in_Majority = measured_in_majority)
  
  # Print the summary table
  print(summary_table)
} else {
  print("No clinical variables found in the datasets.")
}

#####
# Function to retrieve variable meanings from the metadata
get_variable_meanings <- function(dataset) {
  # Download dataset
  geo_data <- getGEO(dataset, GSEMatrix = TRUE)
  
  # Extract metadata
  metadata <- pData(geo_data[[1]])
  
  # Return variable meanings from the metadata
  return(metadata$description)
}

# Check if any clinical variables were identified
if (length(clinical_variables_freq) > 0) {
  # Check if each variable is measured in the majority of datasets
  measured_in_majority <- names(clinical_variables_freq) %in% majority_variables
  
  # Create a data frame to summarize the information
  summary_table <- data.frame(Variable = names(clinical_variables_freq),
                              Frequency = as.numeric(clinical_variables_freq),
                              Measured_in_Majority = measured_in_majority,
                              Meaning = sapply(names(clinical_variables_freq), function(var) {
                                # Retrieve variable meanings from metadata
                                variable_meanings <- get_variable_meanings(datasets[1])
                                # Return the meaning of the current variable or "Meaning not available"
                                if (var <= length(variable_meanings)) {
                                  return(variable_meanings[var])
                                } else {
                                  return("Meaning not available")
                                }
                              }))
  
  # Print the summary table
  print(summary_table)
} else {
  print("No clinical variables found in the datasets.")
}


metadata <- pData(geo_data_list[[1]][[1]])


# Function to retrieve variable meanings from the metadata
get_variable_meanings <- function(dataset) {
  # Download dataset
  geo_data <- getGEO(dataset, GSEMatrix = TRUE)
  
  # Extract metadata
  metadata <- pData(geo_data[[1]])
  
  # Return variable meanings from the metadata, if available
  if ("description" %in% colnames(metadata)) {
    return(metadata$description)
  } else {
    return(NULL)
  }
}

# Check if any clinical variables were identified
if (length(clinical_variables_freq) > 0) {
  # Check if each variable is measured in the majority of datasets
  measured_in_majority <- names(clinical_variables_freq) %in% majority_variables
  
  # Create a data frame to summarize the information
  summary_table <- data.frame(Variable = names(clinical_variables_freq),
                              Frequency = as.numeric(clinical_variables_freq),
                              Measured_in_Majority = measured_in_majority,
                              Meaning = sapply(names(clinical_variables_freq), function(var) {
                                # Retrieve variable meanings from metadata
                                variable_meanings <- get_variable_meanings(datasets[1])
                                # Return the meaning of the current variable if available, otherwise "Meaning not available"
                                if (!is.null(variable_meanings) && var <= length(variable_meanings)) {
                                  return(variable_meanings[var])
                                } else {
                                  return("Meaning not available")
                                }
                              }))
  
  # Print the summary table
  print(summary_table)
} else {
  print("No clinical variables found in the datasets.")
}

# Install and load the package
# Install and load the package
# Install and load the package
# Install and load the package
install.packages("transport")
library(transport)

# Set seed for reproducibility
set.seed(123)

# Function to generate datasets with positive values
generate_dataset <- function(n_rows, n_cols) {
  matrix(runif(n_rows * n_cols), nrow = n_rows)
}


# Generate two datasets
dataset1 <- generate_dataset(100, 7)
dataset2 <- generate_dataset(100, 7)

# Compute Wasserstein distance
distance <- transport(as.vector(dataset1), as.vector(dataset2), "emd")

# Print the distance
print(distance)
library(waddr)
wasserstein_metric(dataset1,dataset2)


# Load necessary library
library(transport)

# Create two datasets with 7 numeric columns
set.seed(123) # Setting a seed for reproducibility
dataset1 <- as.data.frame(matrix(runif(35), ncol=7))
dataset2 <- as.data.frame(matrix(runif(35), ncol=7))

# Calculate the Wasserstein distance
# Assuming each row represents a distribution and we're comparing corresponding rows
distances <- numeric(nrow(dataset1))

for (i in 1:nrow(dataset1)) {
  distances[i] <- wasserstein(
    a=as.matrix(dataset1[i, ]),
    b=as.matrix(dataset2[i, ]),
    p=1
  )
}

# Output the Wasserstein distances
distances


# Load the transport package
library(transport)

# Create two datasets with 7 numeric columns (random data for illustration)
set.seed(123)
dataset1 <- as.data.frame(matrix(runif(35), ncol = 7))
dataset2 <- as.data.frame(matrix(runif(35), ncol = 7))

# Calculate the Wasserstein distance for each pair of rows
distances <- numeric(nrow(dataset1))

for (i in 1:nrow(dataset1)) {
  distances[i] <- wasserstein(
    a = as.matrix(dataset1[i, ]),
    b = as.matrix(dataset2[i, ]),
    p = 1
  )
}

# Print the Wasserstein distances
print(distances)


# Function to compute Euclidean distance between two datasets
euclidean_distance <- function(dataset1, dataset2) {
  sqrt(sum((dataset1 - dataset2)^2))
}

# Generate two datasets
dataset1 <- generate_dataset(100, 7)
dataset2 <- generate_dataset(100, 7)

# Compute Euclidean distance
distance <- euclidean_distance(dataset1, dataset2)

# Print the distance
print(distance)




# Function to compute Wasserstein distance between two datasets
wasserstein_distance <- function(dataset1, dataset2) {
  # Convert datasets to vectors
  dataset1_vector <- as.vector(dataset1)
  dataset2_vector <- as.vector(dataset2)
  
  # Compute Wasserstein distance
  distance <- transport(dataset1_vector, dataset2_vector, method = "emd")
  
  return(distance)
}

# Generate two datasets
dataset1 <- generate_dataset(100, 7)
dataset2 <- generate_dataset(100, 7)

# Compute Euclidean distance
distance <- euclidean_distance(dataset1, dataset2)

# Compute Wasserstein distance
distance <- transport(dataset1, dataset2,)

# Print the distance
print(distance)


# Function to compute KL divergence between two datasets
kl_divergence <- function(dataset1, dataset2) {
  # Check if datasets have the same dimensions
  if (!all(dim(dataset1) == dim(dataset2))) {
    stop("Datasets must have the same dimensions.")
  }
  
  # Compute KL divergence for each pair of corresponding vectors
  kl_values <- sapply(1:ncol(dataset1), function(i) {
    kl_div <- sum(dataset1[, i] * log(dataset1[, i] / dataset2[, i]), na.rm = TRUE)
    return(kl_div)
  })
  
  # Aggregate KL divergence values
  kl_div <- mean(kl_values)
  
  return(kl_div)
}

# Generate two datasets
dataset1 <- generate_dataset(100, 7)
dataset2 <- generate_dataset(100, 7)

# Compute KL divergence
kl_distance <- kl_divergence(dataset1, dataset2)

# Print the KL divergence
print(kl_distance)

# List of packages to install
packages <- c(
  "ball", "cramer", "crossmatch", "diproperm", "Ecume", "energy", 
  "FNN", "GSAR", "gTests", "gCat", "gTestsMulti", "hypoRF", 
  "kernlab", "kerTests", "KMD", "LPKsample", "multicross"
)

# Function to install packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install the packages
install_if_missing(packages)


# List of packages to load (excluding 'crossmatch' and 'multicross')
packages <- c(
  "ball", "cramer", "diproperm", "Ecume", "energy", 
  "FNN", "GSAR", "gTests", "gCat", "gTestsMulti", "hypoRF", 
  "kernlab", "kerTests", "KMD", "LPKsample"
)

# Function to load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      warning(paste("Package", pkg, "is not installed. Please install it first."))
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

divergence_KL<- KL.divergence(dataset1, dataset2, k = 1, algorithm=c("kd_tree", "cover_tree", "brute"))
set.seed(1000)
X<- rexp(10000, rate=0.2)
Y<- rexp(10000, rate=0.4)

KL.divergence(X, Y, k=5)
# Load the packages
load_packages(packages)

