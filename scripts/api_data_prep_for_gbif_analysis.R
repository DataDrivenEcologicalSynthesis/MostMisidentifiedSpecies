setwd("/Users/brendathompson/Desktop/mylocalrepo/MostMisidentifiedSpecies/data")

# Load the data

# Libraries
library("tidyverse")

# Import data subfiles and reassemble the dataset
for(i in 1:3){
  subFile <- readRDS(paste("../data/",i,"-Can_bees_API_2020-05-27.rds",sep = ""))
  
  if(i == 1) data_api <- subFile
  if(i > 1) data_api <- bind_rows(data_api,subFile)
}

# Take out genus information 

data_api$genus <- sub(" [A-z ]*", "", data_api$taxon.name)

# Subset data

new_data_api <- subset(data_api, select=c("genus","quality_grade","identifications_most_agree","species_guess","num_identification_disagreements","identifications_count"))

