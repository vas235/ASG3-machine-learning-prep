library(data.table)
library(haven)

# This code should only need to be run once,
# Once this code has been run you will need to 
# manually add in values 1-5 for field k2q01_d
# 1	Excellent
# 2	Very Good
# 3	Good
# 4	Fair
# 5	Poor
# This is an error in the .do file that gets carried into the csv
# this is the only one I found and the only thing you need to edit in the csv




# Define the data directory
data.dir <- "ml-data-prep/download-nsch-data"

# Get a list of all .do files for the years 2016 to 2022
year.do.vec <- Sys.glob(file.path(data.dir, "*.do"))
year.do.some <- grep("2016|2017|2018|2019|2020|2021|2022", year.do.vec, value=TRUE)

# Loop through each year's .do file and generate the .define.csv file
for (year.do in year.do.some) {
  define.csv <- paste0(year.do, ".define.csv")
  
  # Generate the .define.csv file
  define.dt <- nc::capture_all_str(
    year.do,
    "label define ",
    variable=".*?",
    "_lab +",
    value=".*?",
    ' +"',
    desc=".*?",
    '"'
  )
  
  fwrite(define.dt, define.csv)
}

