#### Anonymize data privacy survey data ####
# This script attempts to anonymize the data from the Data Privacy Survey
# to aid reusability and reusability of the collected data.
# Last edited: 2022-07-19 by Dorien Huijser
# 

#### DEPENDENCIES ####
# install.packages("data.table")
library(data.table)

# install.packages("tidyverse")
library(tidyverse)

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

### CLEAN UP ####
# Skip first two rows as they contain question text
dppsurvey <- dppsurvey[-c(1:2),]

# Skip the previews and select only consented data
dppsurvey <- dppsurvey %>%
  filter(DistributionChannel != "preview") %>%  # No previews
  filter(Consent == "Yes") %>%                  # Only if consent
  filter(is.na(Not_at_UU)) %>%                  # Only UU personnel
  filter(is.na(Not_data))                       # Only if they work with data

# Convert Duration into a numeric variable
dppsurvey$Duration <- as.numeric(dppsurvey$`Duration (in seconds)`)

# Skip unnecessary columns
dppsurvey <- dppsurvey[,-c("StartDate","EndDate","Status", "Progress", 
                           "IPAddress", "Duration (in seconds)", "Finished", 
                           "RecordedDate", "ResponseId", "RecipientLastName", 
                           "RecipientFirstName", "RecipientEmail", 
                           "ExternalReference", "LocationLatitude",
                           "LocationLongitude", "DistributionChannel", 
                           "UserLanguage", "Q_RecaptchaScore", "Consent",
                           "Not_at_UU", "Not_data", "No_personal_data")]

# Turn "NA" into actual NA
dppsurvey[dppsurvey=="NA"] <- NA

### DELETE UNNECESSARY VARIABLES ####
# Remove open text responses
dppsurvey <- dppsurvey[, -grep("_TEXT", 
                               colnames(dppsurvey)), 
                       with = FALSE]

dppsurvey <- dppsurvey[, -c("Challenges", "Infotools_missing")]

### PREPARE DATASET FOR ANONYMITY CHECK ####
# Get all unique colnames without _[0-9]+ (ending number)
uniquevars <- unique(gsub("_[0-9]+", "", colnames(dppsurvey)))
dppsurvey_collapsed <- list()

# Create an untidy dataset with multiple responses to 1 question collapsed
for(variable in 1:length(uniquevars)){
  dppsurvey_collapsed[[variable]] <- dppsurvey %>%
    select(grep(uniquevars[variable],
                names(.), 
                value = TRUE)) %>%
    unite(!!uniquevars[variable], 
          grep(uniquevars[variable],
               names(.), 
               value = TRUE), 
          sep = ", ", 
          na.rm = TRUE,
          remove = TRUE)
}

dppsurvey_collapsed <- do.call("cbind", dppsurvey_collapsed)

# TODO
# Check for unique values for each variable: is there an answer that has been given only once?
# Check for unique combinations of faculty, department, position

