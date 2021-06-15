#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/') 


# sas -- open file
library(haven)
setwd("home/dir/proj/data/subdir")  
cc = as.data.frame(read_sas("dataset.csv.sas7bdat")) # if I do not open SAS file as.data.frame I will import empty rows/cells as well  


# csv -- save and open files
library(readr)
#or library(tidyverse)
setwd("~/home/dir/proj/data/subdir") 
#save
write_csv(dataset, "dataset.csv") 
# open 
ds = read_csv("dataset.csv") 


# rds -- save and open files
saveRDS(dataset, "dataset.rds") 
ds = readRDS("dataset.rds") 



# fread-- open files
library(data.table) # open fread files
ds = fread("dataset.csv")

