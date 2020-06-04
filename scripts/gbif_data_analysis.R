setwd("/Users/brendathompson/Desktop/mylocalrepo")
library(tidyverse)

# The first part of this script reads in the GBIF data without the iNat observations. I am not using this dataset for analysis at the moment, because it excluded genuses that are present in the iNaturalist dataset, so I thought it was better to include them rather than exclude them. Part two reads in the GBIF data that includes iNat observations, and performs an analysis on the api dataset using the genus-species data from this GBIF data.
# There is also a really important step in the Part 2 of this script, which normalizes the genus-species names in the api dataset. This should actually be used before any analysis on the API data, otherwise observations are misrepresented.

#### PART 1: Open the GBIF data without iNat observations ####

bees <- read.csv('canada_bee_dataset.csv')

# Return only the genus and species columns

species_genus <- bees %>% select(species, genus)

# Remove all genus records with no associated species

species_genus <- species_genus[!(species_genus$species==""), ]

# Remove all duplicated records 

species_genus <- species_genus[!duplicated(species_genus$species),]

# For every genus, sum the number of species

genuscount_1 <- as.data.frame(table(species_genus$genus))

#### PART 2: Open the GBIF data with iNat observations and do the same thing####

setwd("/Users/brendathompson/Desktop/mylocalrepo")
library(tidyverse)

all_bees <- read.csv('bees_withiNatdata.csv')

# Return only the genus and species columns

species_genus_2 <- all_bees %>% select(species, genus)

# Remove all genus records with no associated species

species_genus_2 <- species_genus_2[!(species_genus_2$species==""), ]

# Remove all duplicated records 

species_genus_2 <- species_genus_2[!duplicated(species_genus_2$species),]

# For every genus, sum the number of species

genuscount_2 <- as.data.frame(table(species_genus_2$genus))


# Now we need to deal with the problematic species names in data_api, but we'll identify them first here: 

problematic_genera <- new_bees[is.na(new_bees$species_count),]

# Now we have repeats of problematic genera, so now I'll create a subset with only unique values.

problematic_genera <- problematic_genera[!duplicated(problematic_genera$genus),]

# Now with our list, I will re-assign new genus names to the problematic ones, that way they can be identified from the GBIF data.
# Apinae,Xylocopinae, and Meliponini are subfamilies and so identifications at the rank subfamily do not qualify for this genus-species count analysis
# Anthophorini,Epeolini,Bombini,Apini, and Eucerini are tribes and so are also excluded.
# I use new_data_api here which is just a subset of the api data, so feel free to use whatever datatable you want here.
new_data_api$genus[new_data_api$genus=='Alpinobombus'] <- 'Bombus'
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

# Dealing with cases where a species level (or genus level) ID is reached, but the genus ID in new_data_api is subfamily or tribe. 
# Apinae, Anthophorini,Epeolini,Eucerini, Apidae, and Meliponini are excluded because the species guesses made were not at the genus level or lower

# Carpenter bees, which is the species guess for all observations with a genus input of 'Xylocopinae', are actually of genus Xylocopa
new_data_api$genus[new_data_api$genus=='Xylocopinae'] <- 'Xylocopa'

#There is only a single living genus of the tribe 'bombini', which is bombus, so this is an easy substitution
new_data_api$genus[new_data_api$genus=='Bombini'] <- 'Bombus'

# There is a single genus in the tribe Apini, which is Apis
new_data_api$genus[new_data_api$genus=='Apini'] <- 'Apis'

# Dealing with exceptions #
# In some cases, a species guess does not reflect the genus ID given, so this will be examined more closely in the remaining problematic genera. 

# Apidae
new_data_api$genus[new_data_api$species_guess =='Great Basin Bumble Bee'] <- 'Bombus'
new_data_api$genus[new_data_api$species_guess =='Common Eastern Bumble Bee'] <- 'Bombus'
new_data_api$genus[new_data_api$species_guess =='Bumble Bee']<-'Bombus'
new_data_api$genus[new_data_api$species_guess =='Eastern Carpenter Bee']<-'Xylocopa'
new_data_api$genus[new_data_api$species_guess =='Carpenter Bee']<-'Xylocopa'
new_data_api$genus[new_data_api$species_guess =='Carpenter bee']<-'Xylocopa'

### Joining API data to the species per genus data ### 
# Now that the tables have been normalized, joins can be made.

genuscount_2 <- genuscount_2 %>% 
  rename(genus=Var1,
  )

genuscount_2 <- genuscount_2 %>% 
  rename(species_count=Freq,
  )

new_bees <- left_join(new_data_api, genuscount_2)

# Running an analysis to examine misidentification level and number of species per genus

library(dplyr)
library(ggplot2)
library(viridis)

# select data with ratio of DISAGREEMENT per observation > 0
new_bees$RatioDisagree <- new_bees$num_identification_disagreements/new_bees$identifications_count
data_disagree <- dplyr::filter(new_bees, RatioDisagree > 0)




