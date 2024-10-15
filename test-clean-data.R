library(data.table)
library(haven)
library(RJSONIO)



clean_data <- readRDS("ml-data-prep/clean-data.rds")

# Check the structure of the columns to ensure they are factors
str(clean_data$k2q35d)
str(clean_data$family)
unique(clean_data$family)
table(clean_data$family)

with(clean_data, table(year, k2q35a, useNA="always"))
not.X <- c("year", "k2q35a")
names(clean_data)
X.name.vec <- setdiff(names(clean_data),not.X)
fwrite(dcast(clean_data, year ~ k5q21, length), sep="\t")

count.dt.list <- list()
categorical.dt.list <- list()
for(X.name in X.name.vec){
  X.col <- clean_data[[X.name]]
  col.type <- "numeric"
  if(is.factor(X.col)){
    col.type <- "factor"
    X.dt <- clean_data[
    , c(X.name, "year"), with=FALSE
    ][
    , {
      u <- unique(year)
      .(count=.N, n.years=length(u), years=paste(u, collapse=","))
    }, by=c(value=X.name)
    ][, `:=`(
      min.years = min(n.years),
      max.years = max(n.years)
    )][]
    categorical.dt.list[[X.name]] <- data.table(variable=X.name, X.dt)
  }
  dc.dt <- dcast(data.table(isNA=is.na(X.col),year=clean_data$year), . ~ year, mean,value.var="isNA")[,-1]
  count.dt.list[[X.name]] <- data.table(variable=X.name, n.unique=length(unique(X.col)), col.type, "%NA"=dc.dt*100)
}


dcast(clean_data[year>2016], year ~ fpl_i1)

regex.list <- list(
  var=list(
    "label var ",
    variable=".*?",
    ' +"',
    desc=".*?",
    '"'),
  define=list(
    "label define ",
    variable=".*?",
    "_lab +",
    value=".*?",
    ' +"',
    desc=".*?",
    '"'))
year.do.some <- Sys.glob("ml-data-prep/download-nsch-data/*.do")
if(TRUE){
  unlink("ml-data-prep/download-nsch-data/*.csv")
}
do.dt.list <- list()
for(year.do in year.do.some){
  print(year.do)
  year <- as.integer(gsub("_.*?$|^.*?_", "", year.do))
  for(data.type in names(regex.list)){
    type.csv <- paste0(year.do, ".", data.type,".csv")
    if(file.exists(type.csv)){
      type.dt <- fread(type.csv)
    }else{
      type.dt <- nc::capture_all_str(year.do, regex.list[[data.type]])
      fwrite(type.dt, type.csv)
    }
    do.dt.list[[data.type]][[year.do]] <- data.table(year, type.dt)
  }
}
do.meta <- list()
anomaly.list <- list()
by.var.list <- list(
  define=c("variable","value"),
  var="variable")
var.config.list <- RJSONIO::fromJSON("ml-data-prep/variable-config.json")
names(var.config.list$transformations)
transform.names <- names(var.config.list$transformations$transform)
col.count.list <- list()
for(data.type in names(do.dt.list)){
  one.do <- rbindlist(do.dt.list[[data.type]])
  by.vec <- by.var.list[[data.type]]
  col.counts <- data.table(one.do)[
  , n.values := .N, by=by.vec
  ][
  , .(
    years=paste(year,collapse=","),
    n.years=.N,
    n.values=n.values[1]
  ), keyby=c(by.vec,"desc")
  ]
  col.count.list[[data.type]] <- col.counts
  anomaly.list[[data.type]] <- col.counts[
    n.years<n.values
  ][
    names(clean_data), nomatch=0L
  ][
  , trans := variable %in% transform.names
  ]
  do.meta[[data.type]] <- one.do
  out.csv <- sprintf("ml-data-prep/clean-data-all-%s.csv", data.type)
  setkeyv(one.do, by.vec)
  fwrite(one.do, out.csv)
}



# Inspect the structure of 'do.dt.list$define' before combining
str(do.dt.list$define)

# Properly combine the elements of do.dt.list$define
anomaly.list$define <- rbindlist(do.dt.list$define, use.names = TRUE, fill = TRUE)

# Verify the columns in the combined data
print(names(anomaly.list$define))

# Aggregate the years for each group (variable, value)
anomaly.list$define[, years := paste(unique(year), collapse = ","), by = list(variable, value)]

# Print the final result to verify the output
print("Final output after attempting to aggregate years:")
print(anomaly.list$define[, .(variable, value, years)])








var.desc <- col.count.list$var[names(clean_data), .(
  desc,
  n.desc=.N,
  years,
  n.years
), on="variable", by=.EACHI][order(variable)]
fwrite(var.desc, "ml-data-prep/clean-data-var-all-desc.csv")
not.one <- var.desc[n.desc != 1]
fwrite(not.one, "ml-data-prep/clean-data-var-not-one-desc.csv")
(most.freq <- var.desc[, .SD[which.max(n.years)], by=variable])

nsch2017 <- haven::read_dta("ml-data-prep/download-nsch-data/nsch_2017_topical.dta")
hist(nsch2017$fpl_i1)

(count.dt <- rbindlist(count.dt.list)[
  order(n.unique)
][
, diff2017.2016 := `%NA.2017`-`%NA.2016`
][])
(count.join <- most.freq[,.(variable,desc,n.desc)][
  count.dt,
  on="variable", mult="first"])
fwrite(count.join,"ml-data-prep/clean-data-unique-type-missing.csv")

(categorical.dt <- rbindlist(categorical.dt.list)[order(min.years, variable, years, value), .(min.years, max.years, n.years, years, variable, count, value)])
categorical.dt[min.years<max.years]
(categorical.desc <- most.freq[, .(variable,desc)][categorical.dt,on="variable"][min.years<max.years])
categorical.desc[is.na(desc)]

fwrite(categorical.desc, "ml-data-prep/clean-data-values-only-in-some-years.csv", quote = TRUE)
