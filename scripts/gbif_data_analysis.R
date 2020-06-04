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

genuscount_1 <- as.data.frame(table(species_genus$genus))

#### Open the GBIF data with iNat observations and do the same thing####

all_bees <- read.csv('bees_withiNatdata.csv')

# Return only the genus and species columns

species_genus_2 <- all_bees %>% select(species, genus)

# Remove all genus records with no associated species

species_genus_2 <- species_genus_2[!(species_genus_2$species==""), ]

# Remove all duplicated records 

species_genus_2 <- species_genus_2[!duplicated(species_genus_2$species),]

# For every genus, sum the number of species

genuscount_2 <- as.data.frame(table(species_genus_2$genus))


### Join bees table to genuscount_2 based on genus name

genuscount_2 <- genuscount_2 %>% 
  rename(genus=Var1,
  )

genuscount_2 <- genuscount_2 %>% 
  rename(species_count=Freq,
  )

new_bees <- left_join(new_data_api, genuscount_2)

# Now we need to deal with the problematic species names in data_api, but we'll identify them first here: 

problematic_genera <- new_bees[is.na(new_bees$species_count),]

# Now we have repeats of problematic genera, so now I'll create a subset with only unique values.

problematic_genera <- problematic_genera[!duplicated(problematic_genera$genus),]

# Now with this list, we can re-assign new genus names to the problematic ones, that way they can be identified from the GBIF data. This can only be done for observations that have a higher-level genus rank (for example, observations in the API where the genus ID is actually subgenus; in this case, I grabbed the genus associated with that subgenus. But, this can't be done for observations where, for the genus ID, a family name is given)  
# Apinae,Xylocopinae, and Meliponini are subfamilies and so identifications at the rank subfamily do not qualify for this genus-species count analysis
# Anthophorini,Epeolini,Bombini,Apini, and Eucerini are tribes and so are also excluded.
# I use new_data_api here which is just a subset of the api data, so feel free to use whatever datatable you want here.
new_data_api$genus[new_data_api$genus=='bombus'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Psithyrus'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Pyrobombus'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Zadontomerus'] <- 'Ceratina'
new_data_api$genus[new_data_api$genus=='Thoracobombus'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Synhalonia'] <- 'Eucera'
new_data_api$genus[new_data_api$genus=='Bombias'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Eumelissodes'] <- 'Melissodes'
new_data_api$genus[new_data_api$genus=='Cullumanobombus'] <- 'Bombus'
new_data_api$genus[new_data_api$genus=='Ceratinidia'] <- 'Ceratina'

