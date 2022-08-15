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
# Split multiple comma-separated positions
meetings_position <- data.frame(Position = str_trim(unlist(str_split(interviews$Position, 
                                                                     ",")), 
                                                    side = "both"))

# Plot positions of researchers in the meetings
meetings_position %>%
  group_by(Position) %>%
  summarise(Count = length(Position)) %>%
  mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
  arrange(Percentage) %>% # Order by %
  mutate(Position = factor(Position, levels=Position)) %>% # Update factor levels
  ggplot(aes(x = Position, y = Percentage)) + 
  geom_bar(stat = "identity", fill = uucol) + 
  labs(x = "", 
       title = "Positions one-on-one meetings",
       caption = "Positions of researchers in one-on-one meetings") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = paste0(Percentage, "%")), position=position_dodge(width=1.2),
             vjust = 0.5, 
             hjust = 'inward', #-0.2,
             label.size = NA, #1,
             label.padding = unit(0.35, "lines"),
             color = "black",
             fill = "#FFFFFF") +
  theme(axis.title.x = element_text(size = 11),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

# Trying donut chart -> would need to add labels directly in chart
#meetings_position %>%
#group_by(Position) %>%
#summarise(Count = length(Position)) %>%
#mutate(Percentage = round(Count/sum(Count)*100,0)) %>%
#arrange(Percentage) %>% # Order by %
#mutate(Position = factor(Position, levels=Position)) %>% # Update factor levels
#mutate(ymax = cumsum(Percentage)) %>%
#mutate(ymin = c(0, head(ymax, n = -1))) %>%
#mutate(labelPosition = (ymax + ymin)/2) %>%
#mutate(label = paste0(Percentage, "%")) %>%
#ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Position)) +
#geom_rect() +
#geom_label(x = 3.5,
#           aes(y = labelPosition, label = label),
#           size = 3,
#           fill = "white") + 
#coord_polar(theta = "y") +
#xlim(c(1, 4)) + 
#scale_fill_manual(values = UU_pallette) +
#theme_void()



## ---- datatypes --------
# Table of data types
datatypetable <- dppsurvey %>% 
  pivot_longer(cols = grep("^Datatype_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Datatype") %>%
  select(-name) %>%
  group_by(Datatype) %>%
  summarise(Count = length(Datatype)) %>%
  arrange(Count) %>% # Order by count
  mutate(Datatype = factor(Datatype, levels=Datatype)) %>% # Update factor levels
  mutate(Percentage = round(Count/dim(dppsurvey)[1]*100,1)) %>%
  map_df(rev) %>% # reverse order
  mutate(Frequency = paste0(Count, 
                            " (",
                            Percentage,
                            "%)")) %>%
  select(-Count, -Percentage)

# Table of personal data types
personaldatatypes <- dppsurvey %>%
  pivot_longer(cols = grep("^Personaldata_type_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Personal_Datatype") %>%
  select(-name) %>%
  group_by(Personal_Datatype)

tablepersdata <- personaldatatypes %>%
  summarise(Count = length(Personal_Datatype)) %>%
  arrange(Count) %>% # Order by count
  mutate(Personal_Datatype = factor(Personal_Datatype, levels=Personal_Datatype), # Update factor levels
         Percentage = round(Count/dim(dppsurvey)[1]*100,1)) %>%
  map_df(rev) %>% # reverse order
  mutate(Frequency = paste0(Count, 
                            " (",
                            Percentage,
                            "%)"),
  ) %>%
  select(-Count, -Percentage)

kables(list(
  kbl(datatypetable,
      col.names = gsub("_", 
                       " ", 
                       names(datatypetable)), 
      align = "l",
      caption = "<b>Types of data used</b>",
      valign = 't') %>%
    kable_classic(latex_options = "hover",
                  full_width = F, 
                  html_font = "Verdana",
                  font_size = 12,
                  position = "float_left"),
  kbl(tablepersdata, 
      col.names = gsub("_", 
                       " ", 
                       names(tablepersdata)), 
      align = "l",
      caption = "<b>Types of <i>personal</i> data used</b>",
      valign = 't') %>%
    kable_classic(latex_options = "hover",
                  full_width = F,
                  html_font = "Verdana",
                  font_size = 12,
                  position = "left")))



## ---- storageplot --------
dppsurvey %>% 
  pivot_longer(cols = grep("^Storage_medium_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Storage") %>%
  select(-name) %>%
  group_by(Storage) %>%
  summarise(Count = length(Storage)) %>%
  arrange(Count) %>% # Order by count
  mutate(Storage = factor(Storage, levels=Storage)) %>% # Update factor levels
  ggplot(aes(x = Storage, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", title = "Storage media used",
       caption = "Storage Media Used for Storing Personal Data") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)




## ---- consentforms --------
consent_usage_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^Consentforms_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Consent_Forms") %>%
  select(-name) %>%
  group_by(Consent_Forms) %>%
  summarise(Count = length(Consent_Forms)) %>%
  arrange(Count) %>% # Order by count
  mutate(Consent_Forms = factor(Consent_Forms, levels=Consent_Forms)) %>% # Update factor levels
  ggplot(aes(x = Consent_Forms, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Consent Forms",
       caption = "Usage of consent forms") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

consent_content_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^Consent_content_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "Consent_Content") %>%
  select(-name) %>%
  group_by(Consent_Content) %>%
  summarise(Count = length(Consent_Content)) %>%
  arrange(Count) %>% # Order by count
  mutate(Consent_Content = factor(Consent_Content, levels=Consent_Content)) %>% # Update factor levels
  ggplot(aes(x = Consent_Content, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Content of forms",
       caption = "Typical content of an informed consent form") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

grid.arrange(consent_usage_plot, consent_content_plot, nrow = 1)



## ---- dpia --------
DPIA_experience_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^DPIA_experience_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "DPIA_experience") %>%
  select(-name) %>%
  group_by(DPIA_experience) %>%
  summarise(Count = length(DPIA_experience)) %>%
  arrange(Count) %>% # Order by count
  mutate(DPIA_experience = factor(DPIA_experience, levels=DPIA_experience)) %>% # Update factor levels
  ggplot(aes(x = DPIA_experience, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Experience with DPIAs",
       caption = "Familiarity and experience with DPIAs") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

DPIA_help_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^DPIA_Help_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "DPIA_help") %>%
  select(-name) %>%
  group_by(DPIA_help) %>%
  summarise(Count = length(DPIA_help)) %>%
  arrange(Count) %>% # Order by count
  mutate(DPIA_help = factor(DPIA_help, levels=DPIA_help)) %>% # Update factor levels
  ggplot(aes(x = DPIA_help, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Help received with DPIAs",
       caption = "Help received with DPIAs") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

grid.arrange(DPIA_experience_plot, DPIA_help_plot, nrow = 1)


## ---- datasharing --------
external_sharing_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^Share_outside_UU_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "external_sharing") %>%
  select(-name) %>%
  group_by(external_sharing) %>%
  summarise(Count = length(external_sharing)) %>%
  arrange(Count) %>% # Order by count
  mutate(external_sharing = factor(external_sharing, levels=external_sharing)) %>% # Update factor levels
  ggplot(aes(x = external_sharing, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "External sharing",
       caption = "Data Sharing with External Parties") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

sharing_measures_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^Share_measures_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "sharing_measures") %>%
  select(-name) %>%
  group_by(sharing_measures) %>%
  summarise(Count = length(sharing_measures)) %>%
  arrange(Count) %>% # Order by count
  mutate(sharing_measures = factor(sharing_measures, levels=sharing_measures)) %>% # Update factor levels
  ggplot(aes(x = sharing_measures, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Sharing measures",
       caption = "Protection measures taken while sharing data") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

grid.arrange(external_sharing_plot, sharing_measures_plot, nrow = 1)


## ---- datapublishing --------
dppsurvey %>% 
  pivot_longer(cols = grep("^Data_publication_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "data_publishing") %>%
  select(-name) %>%
  group_by(data_publishing) %>%
  summarise(Count = length(data_publishing)) %>%
  arrange(Count) %>% # Order by count
  mutate(data_publishing = factor(data_publishing, levels=data_publishing)) %>% # Update factor levels
  ggplot(aes(x = data_publishing, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Data Publishing",
       caption = "Data Publishing: Frequency and Content") +
  aes(stringr::str_wrap(data_publishing, 15), Count) + xlab(NULL) +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)



## ---- publicationformat --------
publication_format_plot <- 
  dppsurvey %>% 
  pivot_longer(cols = grep("^Publication_format_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "publication_format") %>%
  select(-name) %>%
  group_by(publication_format) %>%
  summarise(Count = length(publication_format)) %>%
  arrange(Count) %>% # Order by count
  mutate(publication_format = factor(publication_format, 
                                     levels=publication_format)) %>% # Update factor levels
  ggplot(aes(x = publication_format, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Published Data Format",
       caption = "Format of Published Data") +
  aes(stringr::str_wrap(publication_format, 15), Count) + xlab(NULL) +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

reasons_nopub_plot <- dppsurvey %>% 
  pivot_longer(cols = grep("^Reason_NoPub_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "reasons_nopub") %>%
  select(-name) %>%
  group_by(reasons_nopub) %>%
  summarise(Count = length(reasons_nopub)) %>%
  arrange(Count) %>% # Order by count
  mutate(reasons_nopub = factor(reasons_nopub, levels=reasons_nopub)) %>% # Update factor levels
  ggplot(aes(x = reasons_nopub, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", 
       title = "Why Not Published?",
       caption = "Reasons for Not Publishing Data") +
  coord_flip() +
  theme_classic() +
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

grid.arrange(publication_format_plot, reasons_nopub_plot, nrow = 1)



## ---- existingsupport --------
# Ever looked for help with privacy?
lookedforhelpplot <- dppsurvey %>% 
  pivot_longer(cols = Lookedforhelp, 
               values_drop_na = TRUE,
               values_to = "searchhelp") %>%
  select(-name) %>%
  group_by(searchhelp) %>%
  summarise(Count = length(searchhelp)) %>%
  arrange(Count) %>% # Order by count
  mutate(searchhelp = factor(searchhelp, levels = searchhelp)) %>% # Update factor levels
  ggplot(aes(x = searchhelp, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", title = "Has looked for help",
       caption = "Have you ever looked for UU-specific information, \n in-person support or tools on handling personal data?") +
  theme_classic() + 
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

# Found what you're looking for?
foundhelpplot <- dppsurvey %>% 
  pivot_longer(cols = Foundhelp, 
               values_drop_na = TRUE,
               values_to = "foundhelp") %>%
  select(-name) %>%
  group_by(foundhelp) %>%
  summarise(Count = length(foundhelp)) %>%
  arrange(Count) %>% # Order by count
  mutate(foundhelp = factor(foundhelp, levels = c("(Almost) always", 
                                                  "Often",
                                                  "Sometimes",
                                                  "(Almost) never"))) %>% # Update factor levels
  ggplot(aes(x = foundhelp, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", title = "Has found what they were looking for",
       caption = "Did you find what you were looking for \n(e.g., information, support, tools)?") +
  theme_classic() + 
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)

grid.arrange(lookedforhelpplot, foundhelpplot, nrow = 1)



## ---- sourcesused --------
# Sources used to find information and help
dppsurvey %>% 
  pivot_longer(cols = grep("^Sourcesused_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "sourcesused") %>%
  select(-name) %>%
  group_by(sourcesused) %>%
  summarise(Count = length(sourcesused)) %>%
  arrange(Count) %>% # Order by count
  mutate(sourcesused = factor(sourcesused, levels = sourcesused)) %>% # Update factor levels
  ggplot(aes(x = sourcesused, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", title = "Sources used to find help") +
  coord_flip() +
  theme_classic() + 
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)


## ---- obstacle --------
dppsurvey %>% 
  pivot_longer(cols = Obstacle, 
               values_drop_na = TRUE,
               values_to = "obstacle") %>%
  select(-name) %>%
  group_by(obstacle) %>%
  summarise(Count = length(obstacle)) %>%
  arrange(Count) %>% # Order by count
  mutate(obstacle = factor(obstacle, levels = c("(Almost) always",
                                                "Sometimes",
                                                "(Almost) never",
                                                "Not sure"))) %>% # Update factor levels
  ggplot(aes(x = obstacle, y = Count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", title = "Is privacy an obstacle?",
       caption = "Do you see dealing with personal data as an obstacle in open science and research data management?") +
  theme_classic() + 
  geom_label(aes(label = Count), position=position_dodge(width=1.2),
             color = "black",
             fill = uucol)


## ---- bettersupport --------
bettersupporttable <- dppsurvey %>% 
  pivot_longer(cols = grep("^Better_support_[0-9]+$", names(dppsurvey), value=TRUE), 
               values_drop_na = TRUE,
               values_to = "bettersupport") %>%
  select(-name) %>%
  group_by(bettersupport) %>%
  summarise(Count = length(bettersupport)) %>%
  arrange(Count) %>% # Order by count
  mutate(bettersupport = factor(bettersupport, levels=bettersupport)) %>% # Update factor levels
  mutate(Percentage = round(Count/dim(dppsurvey)[1]*100,1)) %>%
  map_df(rev) #%>% # reverse order

kable(bettersupporttable, 
      col.names = gsub("_", " ", names(bettersupporttable)), 
      align = "l",
      caption = "How can we improve personal data-related services?")


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