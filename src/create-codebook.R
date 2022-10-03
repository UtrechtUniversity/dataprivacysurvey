# Code to create a codebook from the raw Qualtrics data of the Data Privacy Survey

### DEPENDENCIES ####
#install.packages("codebook")
library(data.table) # fread
library(tidyverse)  # %>%
library(codebook)   # codebook
library(labelled)   # var_label


### READ FILE ####
# Find the latest raw data file
raw_data_files <- list.files(path = "data/raw", 
                             pattern ="Data_Privacy_Survey_[0-9]*.csv")

filenamedates <- as.Date(str_extract(pattern = "[0-9]+", 
                                     string = raw_data_files),
                         format = "%Y%m%d")
most_recent_date <- filenamedates[order(filenamedates, 
                                        decreasing = TRUE)][1]

datafile <- paste0("data/raw/Data_Privacy_Survey_", 
                   gsub("-", "", as.character(most_recent_date)), 
                   ".csv")

dppsurvey <- fread(datafile, na.strings = "")


### LABEL VARIABLES ####
# Create label attributes and assign to the individual variables
# The labels are stored in the first row of the raw dataset
# Source: https://cran.r-project.org/web/packages/codebook/vignettes/codebook_tutorial.html

dict <- data.frame(variable = names(dppsurvey),
                   label = as.character(as.vector(dppsurvey[1,])))

var_label(dppsurvey) <- dict %>% dict_to_list()


### CREATE CODEBOOK ####
dppsurvey <- dppsurvey[-c(1:2),] # remove first two rows
codebookobject <- codebook_table(dppsurvey) %>%
  select(-empty, -whitespace, -min, -max)

# Write to csv
write.csv(codebookobject, "documentation/survey-codebook.csv")


### OTHER CODEBOOK OPTIONS (not used) ####
# 1. Compact_codebook (also codebook package): output = html file
#library(future)
#codebook <- compact_codebook(codebook_data)

# 2. libr package
# column explanation: https://libr.r-sassy.org/reference/dictionary.html
#library(libr)
#codebook <- dictionary(codebook_data)

# 3. WORCS package
#library(worcs)
# codebook <- make_codebook(codebook_data)
# codebook_read <- read.csv("codebook.csv")