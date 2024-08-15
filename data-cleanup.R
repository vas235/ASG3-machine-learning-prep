library(data.table)
library(haven)
library(stringr)
library(jsonlite)
library(dplyr)

# Function to parse label definitions from .do file content
parse_label_definitions <- function(do_lines) {
  # Trim the lines to remove any trailing whitespace or incomplete lines
  do_lines <- trimws(do_lines)
  
  pattern <- "label define (?<variable>\\w+)_lab\\s+(?<code>[\\.\\w]+)\\s+\"(?<label>[^\"]+)\""
  matches <- stringr::str_match(do_lines, pattern)
  
  # Identify "cap label values" cases
  cap_lines <- grepl("cap label values", do_lines)
  
  # Check if the next label is a capped label by comparing indices
  is_capped_label <- rep(FALSE, length(matches[, "variable"]))
  for (i in seq_along(matches[, "variable"])) {
    if (!is.na(matches[i, "variable"]) && matches[i, "variable"] != "" && any(cap_lines & seq_along(cap_lines) > i)) {
      next_cap <- which(cap_lines & seq_along(cap_lines) > i)[1]
      next_label_define <- which(matches[, "variable"] != "" & seq_along(matches[, "variable"]) > i)[1]
      if (!is.na(next_cap) && !is.na(next_label_define) && next_cap < next_label_define) {
        is_capped_label[i] <- TRUE
      }
    }
  }
  
  # Filter out capped labels
  valid_matches <- !is.na(matches[, "variable"]) & !is_capped_label
  labels_dt <- data.table(
    variable = matches[valid_matches, "variable"],
    code = matches[valid_matches, "code"],
    label = matches[valid_matches, "label"]
  )
  
  # Handle all codes as character
  labels_dt[, code := as.character(code)]
  
  labels_dt
}

# Apply labels to each variable in the dataset
apply_labels_to_data <- function(data, labels_dt) {
  unique_variables <- unique(labels_dt$variable)
  
  for (var in unique_variables) {
    # Skip variables ending with '_if'
    if (grepl("_if$", var)) next
    
    if (var %in% names(data)) {
      var_labels <- labels_dt[variable == var]
      
      # Check if the variable only defines missing data
      is_missing_only <- all(var_labels$code %in% c(".m", ".n", ".l", ".d"))
      
      if (is_missing_only) {
        # Replace only the missing data codes with NA, keep other data intact
        data[[var]] <- replace(data[[var]], data[[var]] %in% var_labels$code, NA)
      } else {
        # Convert to factor with labels for other cases
        data[[var]] <- factor(data[[var]], levels = var_labels$code, labels = var_labels$label)
      }
    }
  }
  
  data
}

# Set directory and read files
data_dir <- "download-nsch-data"
do_files <- Sys.glob(file.path(data_dir, "*.do"))

# Process each .do and corresponding .dta file
all_data_list <- list()

for (do_file in do_files) {
  
  dta_file <- sub("do$", "dta", do_file)
  # Load the .dta file
  raw_data <- read_dta(dta_file)
  
  # Example usage of parse_label_definitions
  do_lines <- readLines(do_file)
  labels_dt <- parse_label_definitions(do_lines)
  
  # Apply the labels
  labeled_data <- apply_labels_to_data(raw_data, labels_dt)
  
  # Store processed data
  all_data_list[[dta_file]] <- data.table(labeled_data)
}

# Load configuration JSON
config <- fromJSON("variable-config.json")
desired_variables <- config$desired_variables

apply_transformations <- function(data, transformations, year) {
  for (variable_name in names(transformations)) {
    details <- transformations[[variable_name]]
    if (year %in% details$years && variable_name %in% names(data)) {
      for (i in seq_along(details$value)) {
        # Ensure the value is numeric if required
        if (details$value[i] == ".l") {
          old_val <- "Logical skip" 
        } else {
          old_val <- levels(data[[variable_name]])[as.numeric(details$value[i])]
        }
        new_val <- details$new_label[i]
        
        # Dynamically recode the factor levels
        data[[variable_name]] <- dplyr::recode_factor(data[[variable_name]], !!old_val := new_val)
      }
    }
  }
  data
}

merge_columns <- function(data, merge_config, year) {
  for (variable_name in names(merge_config)) {
    details <- merge_config[[variable_name]]
    if (year %in% details$years && details$column_1 %in% names(data) && details$column_2 %in% names(data)) {
      column_1 <- data[[details$column_1]]
      column_2 <- data[[details$column_2]]
      
      if (is.factor(column_1) && is.factor(column_2)) {
        levels_combined <- unique(c(levels(column_1), levels(column_2)))
        column_1 <- factor(column_1, levels = levels_combined)
        column_2 <- factor(column_2, levels = levels_combined)
        new_column <- factor(ifelse(is.na(column_1), as.character(column_2), as.character(column_1)), levels = levels_combined)
      } else {
        new_column <- ifelse(is.na(column_1), column_2, column_1)
      }
      
      data[[variable_name]] <- new_column
    }
  }
  data
}

# Function to rename columns based on configuration
rename_columns <- function(data, rename_config, year) {
  for (old_name in names(rename_config)) {
    details <- rename_config[[old_name]]
    if (year %in% details$years && old_name %in% names(data)) {
      setnames(data, old_name, details$new_name)
    }
  }
  data
}

# Process all yearly datasets
processed_datasets <- lapply(names(all_data_list), function(name) {
  original_data <- all_data_list[[name]]
  year <- sub(".*_(\\d{4})_.*", "\\1", name)  # Extract year from the filename
  
  # Apply transformations
  transformed_data <- apply_transformations(original_data, config$transformations$transform, year)
  
  # Apply column renaming
  renamed_data <- rename_columns(transformed_data, config$transformations$rename_columns, year)
  
  # Apply column merges
  merged_data <- merge_columns(renamed_data, config$transformations$merge_columns, year)
  
  # Subset to desired variables
  subset_data <- merged_data[, ..desired_variables]
  
  subset_data
})

# Combine all data into one data.table
combined_data <- rbindlist(processed_datasets, use.names = TRUE, fill = TRUE)

# Make column names unique
setnames(combined_data, make.names(names(combined_data), unique = TRUE))

# Define the missing data categories to be replaced with NA
na_categories <- c("No valid response", "Not in universe", "Logical skip", "Suppressed for confidentiality")

# Function to replace specified categories with NA in a factor column
replace_with_na <- function(column) {
  if (is.factor(column)) {
    levels(column)[levels(column) %in% na_categories] <- NA
  }
  column
}

# Apply the function to all columns in the dataset
combined_data <- combined_data %>%
  mutate(across(everything(), replace_with_na))

# Check the number of columns in combined_data
num_columns_combined_data <- ncol(combined_data)
print(paste("Number of columns in combined_data:", num_columns_combined_data))

# Check the number of variables in desired_variables
num_desired_variables <- length(desired_variables)
print(paste("Number of variables in desired_variables:", num_desired_variables))

# Compare the two numbers
if (num_columns_combined_data == num_desired_variables) {
  print("The number of columns in combined_data matches the number of variables in desired_variables.")
} else {
  print("The number of columns in combined_data does not match the number of variables in desired_variables.")
}



# Read in the devscrnng variable extracted from CAHMI datasets
devscrnng_data <- readRDS("devscrnng.rds")

# Ensure that hhid is a key in both data.tables
setkey(combined_data, hhid)
setkey(devscrnng_data, hhid)

# Join the decscrnng variable to the combined dataset
final_data <- combined_data[devscrnng_data, nomatch = 0]

# Save the final data
saveRDS(final_data, "clean-data.rds")
