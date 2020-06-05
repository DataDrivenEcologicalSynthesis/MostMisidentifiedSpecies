#=======================
# GLMs
# June 4, 2020
# Victor Cameron
#=======================

#### Libraries ####

library("tidyverse")
library("viridis")


#### Import data ####

# Import data with phylogeny
data<-read.csv("../data/iNatDataset_Normalized_SpeciesCount_SubsetforAnalysis.csv")

# Remove casual quality grade observations
data <- data_api[! data_api$quality_grade == "casual",]


#### Add disagreement score ####

species <- data %>%
  filter(taxon.rank == "species") %>%
  group_by(taxon.name) %>%
  #add_tally() %>%
  summarize(disagreementScore = mean(num_identification_disagreements)/mean(identifications_count), 
            num_identification_disagreements=sum(num_identification_disagreements), 
            num_identification_agreements=sum(num_identification_agreements), 
            identifications_count=sum(identifications_count),
            comments_count=sum(comments_count),
            user.identifications_count=sum(user.identifications_count),
            genus=genus[1],
            species_count=species_count[1],
            n()) %>%
  filter(!is.na(.$disagreementScore))

species$NoObs <- species$`n()`

#### GLM ####

model <- glm(disagreementScore ~ comments_count + user.identifications_count + identifications_count + species_count + NoObs, data = species)

summary(model)  
Anova(model)


