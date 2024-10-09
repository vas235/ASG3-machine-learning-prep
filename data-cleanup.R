library(data.table)
library(haven)
library(stringr)
library(jsonlite)
library(dplyr)
library(forcats)


# Function to parse labels from the .csv file, stopping at "writeplan"
parse_label_csv <- function(csv_file) {
  # Read the .csv file
  labels_dt <- fread(csv_file, encoding = "UTF-8")
  
  # Identify the row where the "writeplan" variable appears
  stop_row <- which(labels_dt$variable == "writeplan")[1]
  
  # Subset the data to only include rows up to the "writeplan" variable
  labels_dt <- labels_dt[1:stop_row, ]
  
  # Recode missing data labels (.m, .n, .l, .d) to 996, 997, 998, and 999
  labels_dt[value == ".m", value := 996]
  labels_dt[value == ".n", value := 997]
  labels_dt[value == ".l", value := 998]
  labels_dt[value == ".d", value := 999]
  
  # Remove all types of apostrophes from the labels
  labels_dt[, desc := gsub("[‘’'`]", "", desc)]
  
  return(labels_dt)
}



# Apply labels to each variable in the dataset using the .csv file
apply_labels_to_data_csv <- function(data, labels_dt) {
  if (is.null(labels_dt)) {
    return(data)  # No labels, return the data as-is
  }
  
  unique_variables <- unique(labels_dt$variable)
  
  for (var in unique_variables) {
    if (var %in% names(data)) {
      var_labels <- labels_dt[variable == var]
      
      # Check if there are labels for values below 900 (indicating a categorical variable)
      has_labels_below_900 <- any(as.numeric(var_labels$value) < 900, na.rm = TRUE)
      
      if (has_labels_below_900) {
        # Create a named vector for labels using numeric values
        label_map <- setNames(var_labels$desc, as.character(var_labels$value))  # Ensure labels use character values for matching
        
        # Convert the variable values to character for comparison with label_map
        data[[var]] <- as.character(data[[var]])
        
        # Create the "_label" column with the mapped labels
        data[[paste0(var, "_label")]] <- ifelse(data[[var]] %in% names(label_map), label_map[data[[var]]], NA)
        
        # Convert the original column back to numeric
        data[[var]] <- as.numeric(data[[var]])
        
      }
    }
  }
  
  return(data)
}




# Directory containing the .csv files
csv_dir <- "ml-data-prep/download-nsch-data"

# Process each year's data
all_data_list <- list()

for (year in 2016:2022) {
  rds_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.rds"))
  csv_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.do.define.csv"))
  
  # Read the .rds file
  raw_data <- readRDS(rds_file)
  
  # Parse labels from the .csv file
  parsed_labels <- parse_label_csv(csv_file)
  
  # Remove stratum entries from 2016's parsed labels
  if (year == 2016) {
    parsed_labels <- parsed_labels[variable != "stratum"]
  }
  
  # Apply labels to the data
  labeled_data <- apply_labels_to_data_csv(raw_data, parsed_labels)
  
  # Print the unique stratum values for the current year
  cat(paste("\n[DEBUG] Unique stratum values for year:", year, "\n"))
  print(unique(labeled_data$stratum))
  
  # Store processed data
  all_data_list[[as.character(year)]] <- data.table(labeled_data)
}







apply_transformations <- function(data, transformations, year) {
  for (variable_name in names(transformations)) {
    details <- transformations[[variable_name]]
    
    if (year %in% details$years && variable_name %in% names(data)) {
      # Access the "_label" column
      label_col <- paste0(variable_name, "_label")
      
      # Apply JSON transformations
      for (i in seq_along(details$value)) {
        old_val <- details$value[i]
        new_val <- details$new_value[i]
        new_label <- details$new_label[i]
        
        # Update values: change raw data values if `old_val` and `new_val` are different
        data[[variable_name]][data[[variable_name]] == as.numeric(old_val)] <- as.numeric(new_val)
        
        # Update labels: set new labels for updated values
        data[[label_col]][data[[variable_name]] == new_val] <- new_label
      }
    }
  }
  
  return(data)
}




merge_columns <- function(data, merge_config, year) {
  for (variable_name in names(merge_config)) {
    details <- merge_config[[variable_name]]
    
    if (year %in% details$years && details$column_1 %in% names(data) && details$column_2 %in% names(data)) {
      column_1 <- data[[details$column_1]]
      column_2 <- data[[details$column_2]]
      
      # Combine the labels from both columns
      label_col_1 <- paste0(details$column_1, "_label")
      label_col_2 <- paste0(details$column_2, "_label")
      
      combined_labels <- ifelse(is.na(column_1), data[[label_col_2]], data[[label_col_1]])
      
      # Merge the numeric columns
      new_column <- ifelse(is.na(column_1), column_2, column_1)
      data[[variable_name]] <- as.numeric(new_column)  # Ensure the merged column is numeric
      data[[paste0(variable_name, "_label")]] <- combined_labels
    }
  }
  return(data)
}


rename_columns <- function(data, rename_config, year) {
  for (old_name in names(rename_config)) {
    details <- rename_config[[old_name]]
    if (year %in% details$years && old_name %in% names(data)) {
      new_name <- details$new_name
      setnames(data, old_name, new_name)
      # Also rename the "_label" column
      setnames(data, paste0(old_name, "_label"), paste0(new_name, "_label"))
    }
  }
  return(data)
}







# Load configuration JSON
config <- fromJSON("ml-data-prep/variable-config.json")
desired_variables <- config$desired_variables

# Process all yearly datasets
processed_datasets <- lapply(names(all_data_list), function(year) {
  # Access the original data table using the year name
  original_data <- all_data_list[[as.character(year)]]
  
  # Apply transformations using the year
  transformed_data <- apply_transformations(original_data, config$transformations$transform, year)
  
  print(unique(transformed_data$k2q01_d_label))
  
  # Apply column renaming
  renamed_data <- rename_columns(transformed_data, config$transformations$rename_columns, year)
  
  # Apply column merges
  merged_data <- merge_columns(renamed_data, config$transformations$merge_columns, year)
  
  # Subset to desired variables, including their corresponding _label columns if they exist
  label_columns <- paste0(desired_variables, "_label")
  existing_label_columns <- intersect(label_columns, names(merged_data))
  
  # Combine desired variables and the existing label columns for subsetting
  subset_columns <- c(desired_variables, existing_label_columns)
  
  # Perform the subsetting
  subset_data <- merged_data[, ..subset_columns]
  
  return(subset_data)
})




# Combine all data into one data.table
combined_data <- rbindlist(processed_datasets, use.names = TRUE, fill = TRUE)






final_conversion_to_factors <- function(data) {
  # Define the missing data labels
  na_labels <- c("No valid response", "Not in universe", "Logical skip", "Suppressed for confidentiality")
  
  for (var in names(data)) {
    label_col <- paste0(var, "_label")
    
    # Check if there is a corresponding "_label" column
    if (label_col %in% names(data)) {
      
      # Convert the original column to a factor using the labels from the "_label" column
      data[[var]] <- factor(data[[var]], levels = unique(data[[var]]), labels = unique(data[[label_col]]))
      
      # Replace missing data labels with NA
      levels(data[[var]])[levels(data[[var]]) %in% na_labels] <- NA
      
      # Remove the "_label" column as it is no longer needed
      data[[label_col]] <- NULL
    }
  }
  
  return(data)
}



# Convert labeled vectors to factors and handle missing data
combined_data <- final_conversion_to_factors(combined_data)



# Additional processing (fipsst to state conversion, merging additional data, etc.)
# Properly sets the fipsst codes to state names
fips_to_state <- setNames(
  c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
    "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
    "Wisconsin", "Wyoming"), 
  as.character(c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 
                 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 
                 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56))
)

# Create a state category that has the state name instead of the code
combined_data$state <- fips_to_state[as.character(combined_data$fipsst)]



# Read in the devscrnng variable extracted from CAHMI datasets
devscrnng_data <- readRDS("ml-data-prep/devscrnng.rds")

# Ensure that hhid is a key in both data.tables
setkey(combined_data, hhid)
setkey(devscrnng_data, hhid)

# Join the decscrnng variable to the combined dataset
final_data <- combined_data[devscrnng_data, nomatch = 0]



# Save the final data
saveRDS(final_data, "ml-data-prep/clean-data.rds")
