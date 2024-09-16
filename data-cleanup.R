library(data.table)
library(haven)
library(stringr)
library(jsonlite)
library(dplyr)

parse_label_definitions <- function(do_lines, year) {
  # Trim the lines to remove any trailing whitespace or incomplete lines
  do_lines <- trimws(do_lines)
  
  # Regular expression to capture variable, code, and label
  pattern <- "label define (?<variable>\\w+)_lab\\s+(?<code>[\\.\\w]+)\\s+\"(?<label>[^\"]+)\""
  matches <- stringr::str_match(do_lines, pattern)
  
  # Detect missing data labels (.m, .n, .l, .d)
  is_missing_label <- matches[, "code"] %in% c(".m", ".n", ".l", ".d")
  
  # Detect numeric labels (integer values)
  has_numeric_label <- grepl("^\\d+$", matches[, "code"])
  
  # Initialize variables
  numeric_columns <- character(0)  # Variables to treat as numeric
  final_labels_dt <- data.table(variable = character(), code = character(), label = character())
  variables_with_only_label <- unique(gsub(".*label var (\\w+).*", "\\1", grep("^label var", do_lines, value = TRUE)))
  
  # Get unique variables
  unique_variables <- unique(matches[, "variable"])
  
  # Iterate over variables
  for (var in unique_variables) {
    # Skip if variable is NA
    if (is.na(var)) next
    
    # Extract all relevant rows for this variable
    var_rows <- matches[matches[, "variable"] == var, ]
    
    # Initialize flags
    numeric_found <- FALSE
    factor_found <- FALSE
    
    # Iterate through the codes and labels for this variable
    for (i in seq_len(nrow(var_rows))) {
      code <- var_rows[i, "code"]
      label <- var_rows[i, "label"]
      
      if (is.na(code) || is.na(label)) next
      
      # Check if it's a missing data label
      if (code %in% c(".m", ".n", ".l", ".d")) {
        # If no valid factor labels were found, mark this as numeric
        if (!factor_found) {
          numeric_columns <- unique(c(numeric_columns, var))
          break  # Stop further processing for this variable
        }
      } else {
        # If a valid factor label is found, mark the variable as a factor
        factor_found <- TRUE
        # Add label information for the factor
        final_labels_dt <- rbind(final_labels_dt, data.table(
          variable = var,
          code = code,
          label = label
        ))
      }
    }
    
    # Handle variables with only a "label var" line (special 2016 case)
    if (var %in% variables_with_only_label && !var %in% final_labels_dt$variable) {
      numeric_columns <- unique(c(numeric_columns, var))
    }
  }
  
  # If no valid label definitions were found, return NULL and the list of numeric columns
  if (nrow(final_labels_dt) == 0) {
    return(list(labels_dt = NULL, numeric_columns = numeric_columns))
  }
  
  # Convert the 'code' column to character for consistency
  final_labels_dt[, code := as.character(code)]
  
  return(list(labels_dt = final_labels_dt, numeric_columns = numeric_columns))
}


# Apply labels to each variable in the dataset
apply_labels_to_data <- function(data, parsed_labels) {
  labels_dt <- parsed_labels$labels_dt
  numeric_columns <- parsed_labels$numeric_columns
  
  if (is.null(labels_dt) && length(numeric_columns) == 0) {
    return(data)  # No labels and no numeric columns, return the data as-is
  }
  
  unique_variables <- unique(labels_dt$variable)
  
  for (var in unique_variables) {
    
    # Skip variables that should remain numeric
    if (var %in% numeric_columns) next
    
    # Skip state field since the categorical variables don't come in correctly
    if (var == "fipsst") next
    
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
  
  return(data)
}




# Set directory and read files
data_dir <- "ml-data-prep/download-nsch-data"
do_files <- Sys.glob(file.path(data_dir, "*.do"))

# Process each .do and corresponding .dta file
all_data_list <- list()

for (do_file in do_files) {
  
  dta_file <- sub("do$", "dta", do_file)
  raw_data <- read_dta(dta_file)
  
  # Extract year from the filename
  year <- sub(".*_(\\d{4})_.*", "\\1", dta_file)
  
  # Parse label definitions with the year included
  do_lines <- readLines(do_file)
  parsed_labels <- parse_label_definitions(do_lines, year)
  
  # Apply the labels, skipping numeric columns
  labeled_data <- apply_labels_to_data(raw_data, parsed_labels)
  
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



# properly sets the fipsst codes to state names
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


# create a state category that has the state name instead of the code
final_data$state <- fips_to_state[as.character(final_data$fipsst)]


# Save the final data
saveRDS(final_data, "clean-data.rds")
