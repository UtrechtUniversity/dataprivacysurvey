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


## ---- uucolors --------
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

percentagestyle <- list(
  coord_flip(),
  theme_classic(),
  geom_col(fill = uucol),
  geom_text(aes(label = paste0(Percentage, "%")), # % per department
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

## ---- plot-departments --------

plotdepartments <- function(data,
                             string,
                             title = "Department representation",
                             caption = "Survey respondents per department") {
  
  data %>%
    pivot_longer(cols = grep(paste0("Dept_",string,"_[0-9]$"), 
                             names(data), 
                             value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Department") %>%
    select(-name) %>%
    group_by(Department) %>%
    summarise(Count = length(Department)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(reorder(Department, 
                       -Percentage, 
                       sum, 
                       decreasing = T), 
               Percentage)) +
    labs(x = "", title = title, caption = caption) +
    percentagestyle
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


## ---- positionsmeetings --------

positionsmeetings <- function(interviewdata,
                              title = "Positions one-on-one meetings",
                              caption = "Positions of researchers in one-on-one meetings"){
  
  # Split multiple comma-separated positions
  data.frame(Position = str_trim(unlist(str_split(interviewdata$Position, 
                                                                       ",")), 
                                                      side = "both")) %>%
  # Plot positions of researchers in the meetings
    group_by(Position) %>%
    summarise(Count = length(Position)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    arrange(Percentage) %>% # Order by %
    mutate(Position = factor(Position, levels=Position)) %>% # Update factor levels
    ggplot(aes(x = Position, y = Percentage)) + 
    geom_bar(stat = "identity", fill = uucol) + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
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
    arrange(Count) %>% # Order by count
    mutate(Datatype = factor(Datatype, levels=Datatype)) %>% # Update factor levels
    mutate(Percentage = round(Count/dim(data)[1]*100,1)) %>%
    map_df(rev) %>% # reverse order
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
    arrange(Count) %>% # Order by count
    mutate(Personal_Datatype = factor(Personal_Datatype, levels=Personal_Datatype), # Update factor levels
           Percentage = round(Count/dim(dppsurvey)[1]*100,1)) %>%
    map_df(rev) %>% # reverse order
    mutate(Frequency = paste0(Count, " (", Percentage, "%)")) %>%
    select(-Count, -Percentage)
  
  kables(list(
    kbl(datatypetable,
        col.names = gsub("_", " ", names(datatypetable)), 
        align = "l",
        caption = "<b>Types of data used</b>",
        valign = 't') %>%
      kable_classic(latex_options = "hover",
                    full_width = F, 
                    html_font = "Verdana",
                    font_size = 12,
                    position = "float_left"),
    kbl(tablepersdata, 
        col.names = gsub("_", " ", names(tablepersdata)), 
        align = "l",
        caption = "<b>Types of <i>personal</i> data used</b>",
        valign = 't') %>%
      kable_classic(latex_options = "hover",
                    full_width = F,
                    html_font = "Verdana",
                    font_size = 12,
                    position = "left")))
}


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
    mutate(Personal_Datatype = factor(Personal_Datatype, levels=Personal_Datatype)) %>% # Update factor levels
    ggplot(aes(Personal_Datatype, Count)) +
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
          #panel.background = element_rect(fill = NA, color = "black"),
          panel.background = element_blank(),
          panel.border=element_blank(), #rect(fill = NA),
          panel.spacing.y = unit(1, "lines"),
          panel.grid.major.y = element_line(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          strip.background = element_rect(color="white",
                                          size=1.5),
          strip.text = element_text(family = "Verdana",
                                    size = 10,
                                    face = "bold"))
}

## ---- measuresplot --------
measuresplot <- function(data,
                         title = "Protective and planning measures",
                         caption = "Organisational and technical measures used in handling personal data"){
  data %>%
    pivot_longer(cols = grep("^Orgmeasures_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Measures") %>%
    select(-name) %>%
    group_by(Measures) %>%
    summarise(Count = length(Measures)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(Measures = factor(rev(Measures), levels=rev(Measures))) %>% # Update factor levels
    ggplot(aes(Measures, Percentage)) +
    geom_bar(stat = "identity", fill = uucol) + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
} 

## ---- storageplot --------
storageplot <- function(data, 
                        title = "Storage media used", 
                        caption = "Percentage of media used to store personal data") {
  data %>% 
    pivot_longer(cols = grep("^Storage_medium_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Storage") %>%
    select(-name) %>%
    group_by(Storage) %>%
    summarise(Count = length(Storage)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    #arrange(Count) %>% # Order by count
    mutate(Storage = factor(Storage, levels=Storage)) %>% # Update factor levels
    ggplot(aes(x = Storage, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
}


## ---- consentforms --------
consentforms <- function(data, title1 = "Consent Forms", 
                         title2 = "Content of forms",
                         caption1 = "Usage of consent forms",
                         caption2 = "Typical content of an informed consent form"){
  
  consent_usage_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Consentforms_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Consent_Forms") %>%
    select(-name) %>%
    group_by(Consent_Forms) %>%
    summarise(Count = length(Consent_Forms)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(Consent_Forms = factor(Consent_Forms, levels=Consent_Forms)) %>% # Update factor levels
    ggplot(aes(x = Consent_Forms, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    percentagestyle
    
  consent_content_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Consent_content_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "Consent_Content") %>%
    select(-name) %>%
    group_by(Consent_Content) %>%
    summarise(Count = length(Consent_Content)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(Consent_Content = factor(Consent_Content, levels=Consent_Content)) %>% # Update factor levels
    ggplot(aes(x = Consent_Content, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    percentagestyle
  
  grid.arrange(consent_usage_plot, consent_content_plot, nrow = 1)
}


## ---- dpia --------
dpiaplot <- function(data, title1 = "Experience with DPIAs", 
                     caption1 = "Familiarity and experience with DPIAs", 
                     title2 = "Help received with DPIAs", 
                     caption2 = "Help received with DPIAs"){
  DPIA_experience_plot <- 
    data %>% 
    pivot_longer(cols = grep("^DPIA_experience_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "DPIA_experience") %>%
    select(-name) %>%
    group_by(DPIA_experience) %>%
    summarise(Count = length(DPIA_experience)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(DPIA_experience = factor(DPIA_experience, levels=DPIA_experience)) %>% # Update factor levels
    ggplot(aes(x = DPIA_experience, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    percentagestyle
  
  DPIA_help_plot <- 
    data %>% 
    pivot_longer(cols = grep("^DPIA_Help_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "DPIA_help") %>%
    select(-name) %>%
    group_by(DPIA_help) %>%
    summarise(Count = length(DPIA_help)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(DPIA_help = factor(DPIA_help, levels=DPIA_help)) %>% # Update factor levels
    ggplot(aes(x = DPIA_help, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    percentagestyle
  
  grid.arrange(DPIA_experience_plot, DPIA_help_plot, nrow = 1)
}

## ---- datasharing --------
datasharingplot <- function(data, 
                            title1 = "External sharing",
                            caption1 = "Data sharing with external parties",
                            title2 = "Sharing measures",
                            caption2 = "Protection measures taken when sharing data"){
  
  external_sharing_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Share_outside_UU_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "external_sharing") %>%
    select(-name) %>%
    group_by(external_sharing) %>%
    summarise(Count = length(external_sharing)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(external_sharing = factor(external_sharing, levels=external_sharing)) %>% # Update factor levels
    ggplot(aes(x = external_sharing, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    percentagestyle
  
  sharing_measures_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Share_measures_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "sharing_measures") %>%
    select(-name) %>%
    group_by(sharing_measures) %>%
    summarise(Count = length(sharing_measures)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    #arrange(Count) %>% # Order by count
    mutate(sharing_measures = factor(sharing_measures, levels=sharing_measures)) %>% # Update factor levels
    ggplot(aes(x = sharing_measures, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    percentagestyle
  
  grid.arrange(external_sharing_plot, sharing_measures_plot, nrow = 1)
}

## ---- datapublishing --------
datapublishingplot <- function(data, title = "Data publishing",
                               caption = "Data publishing: frequency and content"){
  data %>% 
    pivot_longer(cols = grep("^Data_publication_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "data_publishing") %>%
    select(-name) %>%
    group_by(data_publishing) %>%
    summarise(Count = length(data_publishing)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(data_publishing = factor(data_publishing, levels=data_publishing)) %>% # Update factor levels
    ggplot(aes(x = data_publishing, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
}


## ---- publicationformat --------
publicationformatplot <- function(data, title1 = "Published data format",
                                  caption1 = "Format of published data",
                                  title2 = "Why not published?",
                                  caption2 = "Reasons for not publishing data"){
  
  publication_format_plot <- 
    data %>% 
    pivot_longer(cols = grep("^Publication_format_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "publication_format") %>%
    select(-name) %>%
    group_by(publication_format) %>%
    summarise(Count = length(publication_format)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(publication_format = factor(publication_format, 
                                       levels=publication_format)) %>% # Update factor levels
    ggplot(aes(x = publication_format, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    percentagestyle
  
  reasons_nopub_plot <- data %>% 
    pivot_longer(cols = grep("^Reason_NoPub_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "reasons_nopub") %>%
    select(-name) %>%
    group_by(reasons_nopub) %>%
    summarise(Count = length(reasons_nopub)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    mutate(reasons_nopub = factor(reasons_nopub, levels=reasons_nopub)) %>% # Update factor levels
    ggplot(aes(x = reasons_nopub, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    percentagestyle
  
  grid.arrange(publication_format_plot, reasons_nopub_plot, nrow = 1)
}


## ---- existingsupport --------
existingsupportplot <- function(data, 
                                title1 = "Has looked for help (%)", 
                                caption1 = "Have you ever looked for UU-specific information, \n in-person support or tools on handling personal data?",
                                title2 = "Has found help (%)",
                                caption2 = "Did you find what you were looking for \n(e.g., information, support, tools)?"){
  
  lookedforhelp <- data  %>% 
    pivot_longer(cols = Lookedforhelp, 
                 values_drop_na = TRUE,
                 values_to = "searchhelp") %>%
    select(-name) %>%
    group_by(searchhelp) %>%
    summarise(Count = length(searchhelp)) %>%
    mutate(searchhelp = factor(searchhelp, levels = searchhelp)) %>% # Update factor levels
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(x = searchhelp, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title1, caption = caption1) +
    percentagestyle
  
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
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(x = foundhelp, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title2, caption = caption2) +
    percentagestyle
  
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
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(x = sourcesused, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
}


## ---- obstacle --------
obstacleplot <- function(data,
                         title = "Is privacy an obstacle?",
                         caption = "Percentage of respondents indicating privacy is an obstacle in open science and research data management"){
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
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(x = obstacle, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    coord_flip() +
    percentagestyle
}


## ---- bettersupport --------
bettersupportplot <- function(data, 
                               title = "Better privacy-related support",
                               caption = "How can we improve personal data-related services?"){
data %>% 
    pivot_longer(cols = grep("^Better_support_[0-9]+$", names(data), value=TRUE), 
                 values_drop_na = TRUE,
                 values_to = "bettersupport") %>%
    select(-name) %>%
    group_by(bettersupport) %>%
    summarise(Count = length(bettersupport)) %>%
    mutate(bettersupport = factor(bettersupport, levels=bettersupport)) %>% # Update factor levels
    #mutate(Percentage = round(Count/dim(data)[1]*100,1)) %>%
    mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
    ggplot(aes(x = bettersupport, y = Percentage)) + 
    geom_bar(stat = "identity") + 
    labs(x = "", title = title, caption = caption) +
    percentagestyle
}


## ---- count-codes --------
opentext_freq <- data.frame(table(unlist(str_split(opentext$codes, ", ")))) %>%
  rename(Code = Var1,
         Count = Freq)

interviews_freq <- data.frame(table(unlist(str_split(interviews$codes, ", ")))) %>%
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



## ---- count-tools --------
opentools_freq <- data.frame(table(unlist(str_split(opentext$tools, ", ")))) %>%
  rename(Tool = Var1,
         Count = Freq)

interviewstools_freq <- data.frame(table(unlist(str_split(interviews$tools, ", ")))) %>%
  rename(Tool = Var1,
         Count = Freq)

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



## ---- wordcloud --------
# Prepare wordcloud
totalcodes_wc <- totalcodes %>%
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
wordcloudcodes

# Save coded dataset to processed folder
#write.csv(totalcodes, "../data/processed/codes-freq_survey_meetings.csv")

## ---- sessioninfo --------
sessionInfo()