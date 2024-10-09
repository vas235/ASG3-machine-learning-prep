import pandas as pd
import pyreadr

# Function to recode missing data
def recode_missing_data(data):
    missing_code_map = {
        ".m": 996,
        ".n": 997,
        ".l": 998,
        ".d": 999
    }
    # Recode missing values for all columns
    for col in data.columns:
        if data[col].dtype == 'object':  # Only apply to string columns
            data[col] = data[col].replace(missing_code_map)
    return data

# Special handling for 'stratum' column
def handle_stratum_column(data, year):
    if 'stratum' in data.columns:
        # Convert '2A'/'2a' to '2' before converting to numeric
        data['stratum'] = data['stratum'].str.replace('2A', '2', case=False)
    return data

# Main processing function
def process_data(years, csv_dir):
    for year in years:
        dta_file = f"{csv_dir}/nsch_{year}_topical.dta"
        
        # Read the .dta file with convert_missing=True
        data = pd.read_stata(dta_file, convert_missing=True)
        
        # Convert all columns to string for processing
        data = data.astype(str)
        
        # Recode missing data values
        data = recode_missing_data(data)
        
        # Handle the 'stratum' column for specific cases
        data = handle_stratum_column(data, year)
        
        # Convert columns to numeric where possible
        for col in data.columns:
            data[col] = pd.to_numeric(data[col], errors='coerce')
        
        # Save the DataFrame to an .rds file
        output_file = f"{csv_dir}/nsch_{year}_topical.rds"
        pyreadr.write_rds(output_file, data)
        print(f"Saved: {output_file}")

# Run the processing for the specified years
process_data(range(2016, 2023), "download-nsch-data")
