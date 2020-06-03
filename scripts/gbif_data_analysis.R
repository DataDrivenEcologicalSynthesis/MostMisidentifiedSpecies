setwd("/Users/brendathompson/Desktop/mylocalrepo")
library(tidyverse)

#### Open the GBIF data without iNat observations ####

bees <- read.csv('canada_bee_dataset.csv')

t <- tail(bees,2)

# Return only the genus and species columns

species_genus <- bees %>% select(species, genus)

# Remove all genus records with no associated species

species_genus <- species_genus[!(species_genus$species==""), ]

# Remove all duplicated records 

species_genus <- species_genus[!duplicated(species_genus$species),]

# For every genus, sum the number of species

table(species_genus$genus)

#### Open the GBIF data with iNat observations and do the same thing####

all_bees <- read.csv('bees_withiNatdata.csv')

# Return only the genus and species columns

species_genus_2 <- all_bees %>% select(species, genus)

# Remove all genus records with no associated species

species_genus_2 <- species_genus_2[!(species_genus_2$species==""), ]

# Remove all duplicated records 

species_genus_2 <- species_genus_2[!duplicated(species_genus_2$species),]

# For every genus, sum the number of species

table(species_genus_2$genus)

