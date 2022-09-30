# Try out file to make a codebook
# Resources: https://github.com/nehamoopen/digital-garden/issues/12
# More resources: https://github.com/nehamoopen/digital-garden/issues/45

#Install packages
#install.packages("codebook")
library(codebook)

# also needed for compact_codebook: package future
#library(future)

# Load data to make codebook for
codebook_data <- read.csv("data/processed/Data_Privacy_Survey_fakedataset_20220929.csv")

# Prepare metadata (WIP)
attributes(codebook_data)$label <- codebook_data[1,]
# Does not work > load as vector not df!


# This creates an html file with a table with basic information
#htmlcodebook <- codebook_items(codebook_data)
#codebook2 <- compact_codebook(codebook_data) # also results in index.html file

# This is what we basically want, can be written to csv
# Only thing missing are labels and other attributes not present in the dataset
codebookobject <- codebook_table(codebook_data)



# OLD ####
# Now try with libr package
install.packages("libr")
library(libr)

codebook4 <- dictionary(codebook_data) # a lot faster than codebook package 
# column explanation: https://libr.r-sassy.org/reference/dictionary.html

# Now try with worcs
install.packages("worcs")
library(worcs)

codebook5 <- make_codebook(codebook_data)
codebook6 <- read.csv("codebook.csv")

# Look at colnames for each package
codebookpackage_cols <- data.frame(codebook = colnames(codebook3))
libr_cols <- data.frame(libr = tolower(colnames(codebook4)))
worcs_cols <- data.frame(worcs = colnames(codebook6))

columns <- data.frame(codebook=character(), 
                      libr=character(), 
                      worcs = character(),
                      stringsAsFactors = FALSE)

columns$codebook <- codebookpackage_cols