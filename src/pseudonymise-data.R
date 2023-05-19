#### Pseudonymise data privacy survey data ####
# This script pseudonymises the data from the Data Privacy Survey
# to diminish the amount of personal data in the dataset.
# Most important steps in this script:
# - Select only relevant data: with consent, and from participants from UU 
#   working with data 
# - Delete unnecessary columns (mostly automatically created by Qualtrics)
# - Separate email addresses and open text responses into separate files and
#   remove them from the pseudonymised dppsurvey dataset afterwards
# - Check for unique values and combinations of values in the demographic
#   variables Faculty, Department, and Position. Depending on the outcome
#   of the check, save the cleaned up dataset either in the pseud or in the
#   processed folder.
#
# Last edited: 2022-09-27 by Dorien Huijser

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

# Select only relevant rows: no previews, only consented + UU working with data
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

### SEPARATE AND DELETE IDENTIFIABLE INFO FROM PSEUD. DATASET ####
# Email addresses
emaildataset <- dppsurvey %>%
  filter(Email == "Yes") %>%
  select(contains(c("Faculty", 
                    "Dept", 
                    "Position", 
                    "Email_1_TEXT"))) %>%
  rename(Email = Email_1_TEXT) %>%
  unite("Faculty", grep("Faculty",names(.), value = T), 
        sep = ",", remove = TRUE, na.rm=TRUE) %>%
  unite("Dept", grep("Dept",names(.), value = T), 
        sep = ",", remove = TRUE, na.rm=TRUE) %>%
  unite("Position", grep("Position",names(.), value = T), 
        sep = ",", remove = TRUE, na.rm=TRUE)

write.csv(emaildataset, 
          file.path(paste0("data/raw/", 
                           most_recent_date, 
                           "_dppsurvey_emailaddresses.csv")))

# Open text responses
opentextresponses <- dppsurvey %>% 
  select(
    contains(c("Faculty", "Dept", "Position")),
    Better_support_9_TEXT,
    Challenges,
    Infotools_missing
  ) %>%
  unite("Faculty", grep("Faculty",names(.), value = T), 
        sep = ", ", remove = TRUE, na.rm=TRUE) %>%
  unite("Dept", grep("Dept",names(.), value = T), 
        sep = ", ", remove = TRUE, na.rm=TRUE) %>%
  unite("Position", grep("Position",names(.), value = T), 
        sep = ", ", remove = TRUE, na.rm=TRUE) %>%
  rename(Bettersupport = Better_support_9_TEXT) %>%
  mutate(
    Bettersupport = str_to_lower(str_trim(Bettersupport)),
    Challenges = str_to_lower((str_trim(Challenges))),
    Infotools_missing = str_to_lower(str_trim(Infotools_missing))
  ) %>%
  filter_at(vars(Bettersupport, 
                 Challenges, 
                 Infotools_missing), 
            any_vars(!is.na(.)))

write.csv(opentextresponses, 
          file.path(paste0("data/raw/", 
                           most_recent_date, 
                           "_dppsurvey_opentextresponses.csv")))

# Remove open text responses and email addresses from pseudonymised dataset
dppsurvey <- dppsurvey[, -grep("_TEXT", 
                               colnames(dppsurvey)), 
                       with = FALSE]

dppsurvey <- dppsurvey[, -c("Challenges", "Infotools_missing")]

### K-ANONYMITY CHECK ####
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

# Check for unique values for each demographic variable
check_vars <- dppsurvey_collapsed %>%
  select(Faculty, 
         grep("Dept_*", colnames(dppsurvey_collapsed), value = TRUE), 
         Position) %>%
  gather(key = "question", 
         value = "answer") %>% 
  group_by(question, 
           answer) %>%
  summarise(count = n()) %>%
  filter(count < 2)

# Check for unique combinations of faculty and position
check_combos <- dppsurvey_collapsed %>%
  count(Faculty, 
        #grep("Dept_*", colnames(dppsurvey_collapsed), value = TRUE), 
        Position) %>%
  filter(n  < 2)

# Output k-anonymity check
if(dim(check_vars)[1] != 0 | dim(check_combos)[1] != 0){
  print(
    paste0(
      "There are ",
      dim(check_vars)[1],
      " unique values for faculty, department, or position (separately), and ",
      dim(check_combos)[1],
      " participants who have a unique combination of faculty and position.",
      "Saving to folder data/pseud"
    ))
  
  # Save pseudonymised dataset, pseud folder is gitignored so not uploaded
  write.csv(dppsurvey, 
            file.path(paste0("data/pseud/", 
                             most_recent_date, 
                             "_dppsurvey.csv")))
  
} else if(dim(check_vars)[1] == 0 & dim(check_combos)[1] == 0){
  print( 
    "Hooray! There are no unique values for faculty, department, or position (separately),
    nor are there participants who have a unique combination of faculty and position. 
    Saving to folder data/processed"
  )
  
  # Save dataset, processed folder is not gitignored so uploaded
  write.csv(dppsurvey, 
            file.path(paste0("data/processed/", 
                             most_recent_date, 
                             "_dppsurvey.csv")))
}
