#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/') 


# open sas file
library(haven)
setwd("/bd-fs-mnt/sasgprod_home/o993927/nas/care_plus/data/Datasets_Bruce/CP_Bruce_dataset_2020_1102_MATCHED_CONTROLS_&_OTX")  
cc = as.data.frame(read_sas("cp_stable_otx_cases_comps.sas7bdat")) # if I do not open SAS file as.data.frame I will import empty rows/cells as well  


# save and open csv file
library(readr)
#or library(tidyverse)

setwd("~/sasgprod_home/nas/care_plus/data/cases_controls_cp") 
#save
write_csv(cc_mrn, "CASES_CONTROLS_MRNs.csv") 
# open 
cc_mrn = read_csv("CASES_CONTROLS_MRNs.csv") 


# save and open rds file
saveRDS(cc_mrn, "CASES_CONTROLS_MRNs.rds") 
cc_mrn = readRDS("CASES_CONTROLS_MRNs.rds") 

