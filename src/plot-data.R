# This script functions as a sort of template for the reports of the Data
# Privacy Survey. Its intention is to have most of the code in one place
# to prevent multiple copies of exactly the same code - as much of the code is
# used in multiple R markdown reports.
# The sections in this script are sourced in the relevant code chunks in the
# R markdown files, which then run these sections and create a nice report
# without having to copy-paste code between reports, yay!

## ---- dependencies --------
# install.packages("data.table")
library(data.table)

# install.packages("tidyverse")
library(tidyverse)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("knitr")
library(knitr)

#install.packages("kableExtra")
library(kableExtra)

#install.packages("readxl")
library(readxl)


## ---- readdata --------
### Find the most recent data file in data/pseud or data/processed and read it in
# N.B. When cloning this project, this will load the fake dataset in /data/processed
# as it has the most recent date. 
# NB2: Perhaps R will whine that there is no pseud folder as it is not tracked by 
# git (files in the pseud folder contain pseudonymised data = personal data).

# List all the files in pseud and processed folder
data_files_pseud <- list.files(path = "../data/pseud")
data_files_processed <- list.files(path = "../data/processed")

data_files <- data.frame(filename = c(data_files_pseud,
                                      data_files_processed),
                         folder = c(rep("pseud", length(data_files_pseud)),
                                    rep("processed", length(data_files_processed))
                         )
)

# Get the dates out of the file names and pick the newest one
data_files$filenamedates <- as.Date(str_extract(pattern = "[0-9]+-[0-9]+-[0-9]+", 
                                                string = data_files$filename),
                                    format = "%Y-%m-%d")

# Sort by date (descending = most recent one first)
setorder(data_files, -filenamedates, na.last = TRUE)

# Get most recent file
datafile <- paste0("../data/",
                   data_files$folder[1],
                   "/",
                   data_files$filename[1])

dppsurvey <- fread(datafile)


## ---- labelsfsw --------
# The labels of the FSW Departments are too long to display properly in a graph.
# Therefore they are shortened in this block
dppsurvey <- dppsurvey %>%
  mutate(Dept_FSW_3 = recode(Dept_FSW_3, 
                             "Development & Education of Youth in Diverse Societies" = "Pedagogy in Diverse Societies"),
         Dept_FSW_4 = recode(Dept_FSW_4,
                             "Clinical Child & Family Studies" = "Clinical Child & Family"),
         Dept_FSW_5 = recode(Dept_FSW_5,
                             "Interdisciplinary Social Science" = "Interdisc. Social Sci"),
         Dept_FSW_12 = recode(Dept_FSW_12, 
                              "Social Health & Organisational Psychology" = "Social, Health & Org."))


## ---- facultydatasets --------
# Read in faculty datasets
dppsurvey_science <- dppsurvey %>% filter(!is.na(Faculty_5))
dppsurvey_fsw <- dppsurvey %>% filter(!is.na(Faculty_6))
dppsurvey_geo <- dppsurvey %>% filter(!is.na(Faculty_1))
dppsurvey_vet <- dppsurvey %>% filter(!is.na(Faculty_7))
dppsurvey_leg <- dppsurvey %>% filter(!is.na(Faculty_3))
dppsurvey_hum <- dppsurvey %>% filter(!is.na(Faculty_2))


## ---- uustyle --------
# R color brewers palettes: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# UU colors: https://www.uu.nl/en/organisation/corporate-identity/brand-policy/colour
UU_pallette <- c(
  "#FFE6AB", # Lighter yellow
  "#F3965E", # Orange
  "#AA1555", # Bordeaux-red
  "#6E3B23", # Brown
  "#24A793", # Green
  "#5287C6", # Blue
  "#001240", # Dark blue
  "#5B2182", # Purple
  "#FFCD00" # UU yellow
  )

uucol <- "#FFCD00"

countstyle <- list(
  coord_flip(),
  theme_classic(),
  geom_col(fill = uucol),
  geom_text(aes(label = Count),
            color = "black",
            position = position_stack(vjust = 0.5)),
  theme(legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
)

countstyle_noflip <- list(
  theme_classic(),
  geom_col(fill = uucol),
  geom_text(aes(label = Count),
            color = "black",
            position = position_stack(vjust = 0.5)),
  theme(legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
)


## ---- facultyplot --------
# Plot of positions per faculty
dppsurvey %>%
  pivot_longer(cols = grep("Faculty_[0-9]$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Faculty") %>%
  select(-name) %>%
  pivot_longer(cols = grep("Position_[0-9]$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Position") %>%
  select(-name) %>%
  group_by(Faculty, Position) %>%
  summarise(Count = length(Faculty)) %>%
  ggplot(aes(reorder(Faculty, 
                     -Count, 
                     sum, 
                     decreasing = T), 
             Count, 
             fill = Position)) +
  coord_flip() +
  theme_classic() + 
  geom_col() +
  scale_fill_manual(values = UU_pallette) +
  labs(x = "", 
       title = "Faculty and Position representation",
       caption = "Faculty and Position of online survey respondents") +
  scale_y_discrete(expand = expansion(add = 4)) + # make field larger to see label
  geom_label(aes(label = after_stat(y), group = Faculty), # totals per faculty
             stat = 'summary', 
             fun = sum, 
             vjust = 0.5, 
             hjust = -0.2, #"inward",
             nudge_y = 2,
             label.size = NA, #1,
             label.padding = unit(0.35, "lines"),
             color = "black",
             fill = "#FFFFFF") + 
  geom_text(aes(label = Count), # counts per position per faculty
            color = "white",
            fill = "#FFFFFF",
            position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE))


## ---- plot-departments --------
plotdepartments <- function(data,
                             string,
                             title = "Department representation",
                             caption = "Departments of online survey respondents") {
  
  data %>%
    pivot_longer(cols = grep(paste0("Dept_",string,"_[0-9]$"), 
                             names(data), 
                             value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Department") %>%
    select(-name) %>%
    group_by(Department) %>%
    summarise(Count = length(Department)) %>%
    ggplot(aes(reorder(Department, 
                       -Count, 
                       sum, 
                       decreasing = T), 
               Count)) +
    labs(x = "", title = title, caption = caption) +
    countstyle
}


## ---- read-opentext --------
# Read in coded responses from interviews
interviewfile <- "../data/coded/dppsurvey_interviews_coded.xlsx"

interviews <- read_excel(interviewfile, 
                         sheet = "interviews") %>%
  select(Interviewnr, Faculty, Position, Present, codes, tools)

# Read in coded responses from open questions
opentextfile <- "../data/coded/dppsurvey_opentextresponses_coded.xlsx"
opentext <- read_excel(opentextfile, sheet = "opentextresponses") %>%
  select(Faculty, Dept, Position, codes, tools)
opencodes <- read_excel(opentextfile, sheet = "codes") %>%
  select(-`Times mentioned`)
opentools <- read_excel(opentextfile, sheet = "tools") %>%
  select(-`Times mentioned`)


## ---- opentext-faculties --------
# Filter interview and open text responses codes on faculty
interviews_science <- interviews %>% filter(grepl("Science(?!s)", Faculty, perl = TRUE))
interviews_fsw <- interviews %>% filter(grepl("Social and Beh*", Faculty, perl = TRUE))
interviews_leg <- interviews %>% filter(grepl("Law Economics", Faculty, perl = TRUE))
interviews_geo <- interviews %>% filter(grepl("Geosciences", Faculty, perl = TRUE))
interviews_vet <- interviews %>% filter(grepl("Veterinary", Faculty, perl = TRUE))
interviews_hum <- interviews %>% filter(grepl("Humanities", Faculty, perl = TRUE))

opentext_science <- opentext %>% filter(grepl("Science(?!s)", Faculty, perl = TRUE))
opentext_fsw <- opentext %>% filter(grepl("Social and Beh*", Faculty, perl = TRUE))
opentext_leg <- opentext %>% filter(grepl("Law Economics", Faculty, perl = TRUE))
opentext_geo <- opentext %>% filter(grepl("Geosciences", Faculty, perl = TRUE))
opentext_vet <- opentext %>% filter(grepl("Veterinary", Faculty, perl = TRUE))
opentext_hum <- opentext %>% filter(grepl("Humanities", Faculty, perl = TRUE))


## ---- emailaddressplot --------
meetings_faculties <- data.frame(Faculty = str_trim(unlist(str_split(interviews$Faculty, 
                                                                     ",")), 
                                                    side = "both"))

meetings_faculties %>%
  group_by(Faculty) %>%
  summarise(Count = length(Faculty)) %>%
  mutate(Faculty = factor(Faculty, levels=Faculty)) %>% # Update factor levels
  arrange(Count) %>% # Order by count
  ggplot(aes(x = forcats::fct_rev(Faculty), y = Count)) + 
  geom_bar(stat = "identity", fill = uucol) + 
  labs(x = "", 
       title = "Faculty representation one-on-one meetings",
       caption = "Faculty representation of researchers spoken to in one-on-one meetings") +
  countstyle


## ---- positionsmeetings --------
positionsmeetings <- function(interviewdata,
                              title = "Positions one-on-one meetings",
                              caption = "Positions in one-on-one meetings"){
  
  # Split multiple comma-separated positions
  data.frame(Position = str_trim(unlist(str_split(interviewdata$Position, 
                                                                       ",")), 
                                                      side = "both")) %>%
  # Plot positions of researchers in the meetings
    group_by(Position) %>%
    summarise(Count = length(Position)) %>%
    mutate(Position = factor(Position, levels=Position)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(Position), y = Count)) + 
    geom_bar(stat = "identity", fill = uucol) + 
    labs(x = "", title = title, caption = caption) +
    countstyle
}


## ---- datatypes --------
datatypes <- function(data){
  # Table of data types
  datatypetable <- data %>% 
    pivot_longer(cols = grep("^Datatype_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Datatype") %>%
    select(-name) %>%
    group_by(Datatype) %>%
    summarise(Count = length(Datatype)) %>%
    mutate(Datatype = factor(Datatype, levels=Datatype)) %>% # Update factor levels
    mutate(Percentage = round(Count/dim(data)[1]*100,1)) %>%
    mutate(Frequency = paste0(Count, " (", Percentage, "%)")) %>%
    select(-Count, -Percentage)
  
  # Table of personal data types
  tablepersdata <- data %>%
    pivot_longer(cols = grep("^Personaldata_type_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Personal_Datatype") %>%
    select(-name) %>%
    group_by(Personal_Datatype) %>%
    summarise(Count = length(Personal_Datatype)) %>%
    mutate(Personal_Datatype = factor(Personal_Datatype, levels=Personal_Datatype), # Update factor levels
           Percentage = round(Count/dim(dppsurvey)[1]*100,1)) %>%
    mutate(Frequency = paste0(Count, " (", Percentage, "%)")) %>%
    select(-Count, -Percentage)
  
  tables <- list(datatypetable, tablepersdata)
  return(tables)
}

datatypes_html <- function(tables){
  t1 <- kable(tables[[1]], format = "html", output = FALSE, 
              col.names = gsub("_", " ", names(tables[[1]]),),
              caption = "<b>Types of research data</b>",
              table.attr='cellpadding="3", cellspacing="3"') %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "responsive"),
                  fixed_thead = T)
  t2 <- kable(tables[[2]], format = "html", output = FALSE, 
              col.names = gsub("_", " ", names(tables[[2]])),
              caption = "<b>Types of personal data</b>",
              table.attr='cellpadding="3"') %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "responsive"),
                  fixed_thead = T)
  
  htmltables <- list(t1, t2)
  return(htmltables)
} 

datatypes_latex <- function(tables){
  kables(list(
    kable(tables[[1]], "simple",
          col.names = gsub("_", " ", names(tables[[1]]),),
          caption = "Types of research data",
          valign = 't'),
    kable(tables[[2]], "simple",
          col.names = gsub("_", " ", names(tables[[2]])),
          caption = "Types of personal data",
          valign = 't')
  ))
}


## ---- personaldataplot --------
dppsurvey %>% 
  pivot_longer(cols = grep("^Personaldata_type_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Personal_Datatype") %>%
  select(-name) %>%
  pivot_longer(cols = grep("^Faculty_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Faculty") %>%
  select(-name) %>%
  group_by(Faculty, Personal_Datatype) %>%
  summarise(Count = length(Personal_Datatype)) %>%
  mutate(Personal_Datatype = factor(Personal_Datatype, 
                                    levels=Personal_Datatype)) %>% # Update factor levels
  ggplot(aes(x = forcats::fct_rev(Personal_Datatype), y = Count)) + 
  geom_bar(stat = "identity", fill = "#FFCD00") + 
  facet_wrap(~Faculty, ncol = 4,
             labeller = label_wrap_gen(width=14)) +
  labs(x = "", 
       title = "Personal data types across Faculties",
       caption = "Types of personal data used per UU faculty") +
  coord_flip() +
  theme_classic() +
  geom_col(fill = uucol) +
  geom_text(aes(label = Count),
            color = "black",
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(), #rect(fill = NA),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.major.y = element_line(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.background = element_rect(color="white",
                                        size=1.5),
        strip.text = element_text(size = 10,
                                  face = "bold"))


## ---- datatypesdepartments --------
datatypesdepartments <- function(data, string,
                                 title = "Personal data types across departments",
                                 caption = "Personal data types used in each department"){
  data %>%
    pivot_longer(cols = grep(paste0("^Dept_",string,"_[0-9]+$"), names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Department") %>%
    select(-name) %>%
    pivot_longer(cols = grep("^Personaldata_type_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Personal_Datatype") %>%
    select(-name) %>%
    group_by(Department, Personal_Datatype) %>%
    summarise(Count = length(Personal_Datatype)) %>%
    mutate(Personal_Datatype = factor(Personal_Datatype, 
                                      levels=Personal_Datatype)) %>% # Update factor levels
    ggplot(aes(forcats::fct_rev(Personal_Datatype), y = Count)) +
    coord_flip() +
    theme_classic() + 
    geom_col(fill = uucol) +
    facet_wrap(~Department, ncol = 4, labeller = label_wrap_gen(width=14)) +
    labs(x = "", title = title, caption = caption) +
    geom_text(aes(label = Count),
              color = "black",
              position = position_stack(vjust = 0.5)) +
    theme(axis.title.x = element_text(size = 10),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(), #rect(fill = NA),
          panel.spacing.y = unit(1, "lines"),
          panel.grid.major.y = element_line(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          strip.background = element_rect(color="white",
                                          size=1.5),
          strip.text = element_text(size = 10,
                                    face = "bold"))
}


## ---- measuresplot --------
measuresplot <- function(data,
                         title = "Protective and planning measures",
                         caption = "Which privacy-related measures do you (generally) implement in your research?"){
  data %>%
    pivot_longer(cols = grep("^Orgmeasures_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Measures") %>%
    select(-name) %>%
    group_by(Measures) %>%
    summarise(Count = length(Measures)) %>%
    mutate(Measures = factor(Measures, levels=Measures)) %>% # Update factor levels
    ggplot(aes(forcats::fct_rev(Measures), Count)) +
    geom_bar(stat = "identity", fill = uucol) + 
    labs(x = "", title = title, caption = caption) +
    countstyle
} 

## ---- storageplot --------
storageplot <- function(data, 
                        title = "Storage media used", 
                        caption = "Where do you store your research data?") {
  data %>% 
    pivot_longer(cols = grep("^Storage_medium_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Storage") %>%
    select(-name) %>%
    group_by(Storage) %>%
    summarise(Count = length(Storage)) %>%
    mutate(Storage = factor(Storage, levels=Storage)) %>% # Update factor levels
    ggplot(aes(x = Storage, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    countstyle
}


## ---- consentforms --------
consentforms <- function(data, title1 = "Consent Forms", 
                         title2 = "Content of forms",
                         caption1 = "Do you use consent forms in your research?",
                         caption2 = "What privacy-related content is in your average \ninformation letter and/or consent form?"){
  
  consent_usage_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Consentforms_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Consent_Forms") %>%
    select(-name) %>%
    group_by(Consent_Forms) %>%
    summarise(Count = length(Consent_Forms)) %>%
    mutate(Consent_Forms = factor(Consent_Forms, levels=Consent_Forms)) %>% # Update factor levels
    ggplot(aes(x = Consent_Forms, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    countstyle
    
  consent_content_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Consent_content_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Consent_Content") %>%
    select(-name) %>%
    group_by(Consent_Content) %>%
    summarise(Count = length(Consent_Content)) %>%
    mutate(Consent_Content = factor(Consent_Content, 
                                    levels=Consent_Content)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(Consent_Content), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    countstyle
  
  grid.arrange(consent_usage_plot, consent_content_plot, nrow = 1)
}


## ---- dpia --------
dpiaplot <- function(data, title1 = "Experience with DPIAs", 
                     caption1 = "Have you ever conducted, or will you \nconduct, a Data Protection Impact Assessment (DPIA)?", 
                     title2 = "Received support DPIAs", 
                     caption2 = "Did/Will you ask for help \nin conducting the DPIA?"){
  DPIA_experience_plot <- 
    data %>% 
    pivot_longer(cols = grep("^DPIA_experience_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "DPIA_experience") %>%
    select(-name) %>%
    group_by(DPIA_experience) %>%
    summarise(Count = length(DPIA_experience)) %>%
    mutate(DPIA_experience = factor(DPIA_experience, levels=DPIA_experience)) %>% # Update factor levels
    ggplot(aes(x = DPIA_experience, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    countstyle
  
  DPIA_help_plot <- 
    data %>% 
    pivot_longer(cols = grep("^DPIA_Help_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "DPIA_help") %>%
    select(-name) %>%
    group_by(DPIA_help) %>%
    summarise(Count = length(DPIA_help)) %>%
    mutate(DPIA_help = factor(DPIA_help, levels=DPIA_help)) %>% # Update factor levels
    ggplot(aes(x = DPIA_help, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    countstyle
  
  grid.arrange(DPIA_experience_plot, DPIA_help_plot, nrow = 1)
}


## ---- datasharing --------
datasharingplot <- function(data, 
                            title1 = "External sharing",
                            caption1 = "Do you / Will you share research data \ncontaining personal data outside of the UU?",
                            title2 = "Sharing measures",
                            caption2 = "What actions do you take to transfer \npersonal data securely outside of the UU?"){
  
  external_sharing_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Share_outside_UU_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "external_sharing") %>%
    select(-name) %>%
    group_by(external_sharing) %>%
    summarise(Count = length(external_sharing)) %>%
    mutate(external_sharing = factor(external_sharing, levels=external_sharing)) %>% # Update factor levels
    ggplot(aes(x = external_sharing, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    countstyle
  
  sharing_measures_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Share_measures_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "sharing_measures") %>%
    select(-name) %>%
    group_by(sharing_measures) %>%
    summarise(Count = length(sharing_measures)) %>%
    mutate(sharing_measures = factor(sharing_measures, 
                                     levels=sharing_measures)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(sharing_measures), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    countstyle
  
  grid.arrange(external_sharing_plot, sharing_measures_plot, nrow = 1)
}


## ---- datapublishing --------
datapublishingplot <- function(data, title = "Has published data",
                               caption = "Have you ever published a dataset containing personal data (e.g., in a repository)?"){
  data %>% 
    pivot_longer(cols = grep("^Data_publication_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "data_publishing") %>%
    select(-name) %>%
    group_by(data_publishing) %>%
    summarise(Count = length(data_publishing)) %>%
    mutate(data_publishing = factor(data_publishing, 
                                    levels=data_publishing)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(data_publishing), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    countstyle_noflip
}


## ---- publicationformat --------
publicationformatplot <- function(data, title1 = "Published data format",
                                  caption1 = "In what format was the data published?",
                                  title2 = "Reasons for not publishing",
                                  caption2 = "Why didn't you publish your data?"){
  
  publication_format <- 
    data %>% 
    pivot_longer(cols = grep("^Publication_format_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "publication_format") %>%
    select(-name) %>%
    group_by(publication_format) %>%
    summarise(Count = length(publication_format)) %>%
    mutate(publication_format = factor(publication_format, 
                                       levels=publication_format)) # Update factor levels

    # If there is no data on publication format, leave the plot empty
    if(dim(publication_format)[1] != 0){
    publication_format_plot <-
      publication_format %>%
      ggplot(aes(x = forcats::fct_rev(publication_format), y = Count)) + 
      geom_bar(stat = "identity") + 
      labs(x = "", title = title1, caption = caption1) +
      countstyle
  } else {
    publication_format_plot <- ggplot() + theme_void()
  }
  
  reasons_nopub_plot <- data %>% 
    pivot_longer(cols = grep("^Reason_NoPub_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "reasons_nopub") %>%
    select(-name) %>%
    group_by(reasons_nopub) %>%
    summarise(Count = length(reasons_nopub)) %>%
    mutate(reasons_nopub = factor(reasons_nopub, 
                                  levels=reasons_nopub)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(reasons_nopub), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    countstyle
  
  grid.arrange(publication_format_plot, reasons_nopub_plot, nrow = 1)
}


## ---- knowpoplot --------
dppsurvey %>% 
  pivot_longer(cols = grep("^Faculty_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Faculty") %>%
  select(-name) %>%
  group_by(Faculty, Know_PO) %>%
  summarise(Count = length(Know_PO)) %>%
  mutate(Know_PO = factor(Know_PO, levels=Know_PO)) %>% # Update factor levels
  arrange(Count) %>% # Order by count
  filter(!is.na(Know_PO)) %>%
  ggplot(aes(x = reorder(Know_PO, -Count, sum, decreasing = T), y = Count)) + 
  geom_bar(stat = "identity", fill = uucol) + 
  facet_wrap(~Faculty, ncol = 4,
             labeller = label_wrap_gen(width=14),
             scales = 'free_x') +
  labs(x = "", 
       title = "Do you know who your faculty privacy officer is?",
       caption = "Number of researchers indicating they were aware who their faculty privacy officer is across UU faculties") +
  theme_classic() +
  geom_text(aes(label = Count),
            color = "black",
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(), #rect(fill = NA),
        panel.spacing.x = unit(1, "lines"),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.background = element_rect(color="white",
                                        size=1.5),
        strip.text = element_text(size = 10,
                                  face = "bold"))


## ---- existingsupport --------
existingsupportplot <- function(data, 
                                title1 = "Has looked for help", 
                                caption1 = "Have you ever looked for UU-specific information, \n in-person support or tools on handling personal data?",
                                title2 = "Has found help",
                                caption2 = "Did you find what you were looking for \n(e.g., information, support, tools)?"){
  
  lookedforhelp <- data  %>% 
    pivot_longer(cols = Lookedforhelp, 
                 values_drop_na = TRUE,
                 values_to = "searchhelp") %>%
    select(-name) %>%
    group_by(searchhelp) %>%
    summarise(Count = length(searchhelp)) %>%
    mutate(searchhelp = factor(searchhelp, levels = searchhelp)) %>% # Update factor levels
    ggplot(aes(x = searchhelp, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    countstyle
  
  foundhelp <- data %>%
    pivot_longer(cols = Foundhelp, 
                 values_drop_na = TRUE,
                 values_to = "foundhelp") %>%
    select(-name) %>%
    group_by(foundhelp) %>%
    summarise(Count = length(foundhelp)) %>%
    mutate(foundhelp = factor(foundhelp, levels = c("(Almost) always", 
                                                    "Often",
                                                    "Sometimes",
                                                    "(Almost) never"))) %>% # Update factor levels
    ggplot(aes(x = foundhelp, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    countstyle
  
  grid.arrange(lookedforhelp, foundhelp, nrow = 1)
}


## ---- sourcesused --------
sourcesusedplot <- function(data,
                            title = "Channels used to find information",
                            caption = "Which channels do you use to find information about handling personal data?"){
  
  data %>% 
    pivot_longer(cols = grep("^Sourcesused_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "sourcesused") %>%
    select(-name) %>%
    group_by(sourcesused) %>%
    summarise(Count = length(sourcesused)) %>%
    mutate(sourcesused = factor(sourcesused, levels = sourcesused)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(sourcesused), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    countstyle
}


## ---- obstacle --------
obstacleplot <- function(data,
                         title = "Is privacy an obstacle?",
                         caption = "Do you see dealing with personal data as an \n
                         obstacle in open science and research data management?"){
  data %>% 
    pivot_longer(cols = Obstacle, 
                 values_drop_na = TRUE,
                 values_to = "obstacle") %>%
    select(-name) %>%
    group_by(obstacle) %>%
    summarise(Count = length(obstacle)) %>%
    mutate(obstacle = factor(obstacle, levels = c("(Almost) always",
                                                  "Sometimes",
                                                  "(Almost) never",
                                                  "Not sure"))) %>% # Update factor levels
    ggplot(aes(x = obstacle, y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    countstyle_noflip
}


## ---- bettersupport --------
bettersupportplot <- function(data, 
                               title = "Better privacy-related support",
                               caption = "What can we do better to support you in handling personal data in research?"){
data %>% 
    pivot_longer(cols = grep("^Better_support_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "bettersupport") %>%
    select(-name) %>%
    group_by(bettersupport) %>%
    summarise(Count = length(bettersupport)) %>%
    mutate(bettersupport = factor(bettersupport, 
                                  levels=bettersupport)) %>% # Update factor levels
    ggplot(aes(x = forcats::fct_rev(bettersupport), y = Count)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    countstyle
}


## ---- count-codes --------
countcodes <- function(surveydataset = opentext,
                       interviewdataset = interviews){
  opentext_freq <- data.frame(table(unlist(str_split(surveydataset$codes, ", ")))) %>%
    rename(Code = Var1,
           Count = Freq)
  
  interviews_freq <- data.frame(table(unlist(str_split(interviewdataset$codes, ", ")))) %>%
    rename(Code = Var1,
           Count = Freq)
  
  # Merge open text responses and meeting notes codes (used the same codes)
  totalcodes <- merge(opentext_freq,
                      interviews_freq,
                      by = "Code",
                      suffixes = c("_survey", "_interview"),
                      all = TRUE)
  
  # Count total times mentioned across survey and interviews
  totalcodes <- totalcodes %>%
    rowwise() %>%
    mutate(Times_mentioned_total = sum(Count_survey, Count_interview, na.rm=T))
  
  # Add code meanings
  totalcodes <- merge(totalcodes,
                      opencodes,
                      by = "Code")
  totalcodes <- arrange(totalcodes, desc(Times_mentioned_total))
  
  return(totalcodes)
}


## ---- count-tools --------
counttools <- function(surveydataset = opentext,
                       interviewdataset = interviews){
  if(!all(is.na(surveydataset$tools))){
    opentools_freq <- data.frame(table(unlist(str_split(surveydataset$tools, ", ")))) %>%
      rename(Tool = Var1,
             Count = Freq)
  }
  
  if(!all(is.na(interviewdataset$tools))){
    interviewstools_freq <- data.frame(table(unlist(str_split(interviewdataset$tools, ", ")))) %>%
      rename(Tool = Var1,
             Count = Freq)
  }
  
  # For in-text reference, count total times certain tools are mentioned
  totaltools <- merge(opentools_freq, 
                      interviewstools_freq, 
                      by = "Tool",
                      suffixes = c("_survey","_interview"),
                      all = TRUE)
  
  # Count total times mentioned across survey and interviews
  totaltools <- totaltools %>%
    rowwise() %>%
    mutate(Times_mentioned_total = sum(Count_survey, Count_interview, na.rm=T))
  
  # Add tool code meanings
  totaltools <- merge(totaltools,
                      opentools,
                      by.x = "Tool",
                      by.y = "Name tool")
  totaltools <- arrange(totaltools, desc(Times_mentioned_total))
  
  return(totaltools)
}


## ---- wordcloud --------
createwordcloud <- function(codesdata){
  totalcodes_wc <- codesdata %>%
    select(Code, Times_mentioned_total) %>%
    mutate(word = Code,
           freq = Times_mentioned_total,
           .keep = "none")
  
  library(wordcloud2)
  wordcloudcodes <- wordcloud2(data = totalcodes_wc, 
                               color=rep_len(UU_pallette, nrow(totalcodes_wc)),
                               fontFamily = "Open Sans",
                               minRotation = 0, 
                               maxRotation = 0,
                               rotateRatio = 1)
  return(wordcloudcodes)
}