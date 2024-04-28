install.packages("GEOquery")
library(GEOquery)
datasets <- c("GSE60518", "GSE56843", "GSE43346", "GSE44723", "GSE32474", "GSE21411", 
              "GSE19804", "GSE10245", "GSE13525", "GSE9586", "GSE9008", "GSE5579", 
              "GSE6013", "GSE6400", "GSE5059")


# Download each dataset separately
geo_data_list <- lapply(datasets, function(dataset) getGEO(dataset,GSEMatrix = TRUE))
dim(pData(phenoData(geo_data_list[[1]][[1]])))

dim(pData(phenoData(dataGSE50948[[1]])))

a<-apply((pData(phenoData(geo_data_list[[1]][[1]]))), 2, table)



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

