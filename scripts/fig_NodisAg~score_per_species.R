#=======================
# No disagreeing IDs vs. total no IDs
# June 4, 2020
# Victor Cameron
#=======================

#### Libraries ####

library("tidyverse")
library("viridis")


#### Import data ####

# Import data subfiles and reassemble the dataset
for(i in 1:3){
  subFile <- readRDS(paste("../data/",i,"-Can_bees_API_2020-05-27.rds",sep = ""))
  
  if(i == 1) data_api <- subFile
  if(i > 1) data_api <- bind_rows(data_api,subFile)
}

# Remove casual quality grade observations
data <- data_api[! data_api$quality_grade == "casual",]


#### Plot No disagreeing IDs vs. total No IDs per species ####

# Open new plot window
quartz(width = 8,height = 5)

# Data
species <- data %>%
  filter(taxon.rank == "species") %>%
  group_by(taxon.name) %>%
  #add_tally() %>%
  summarize(disagreementScore = mean(num_identification_disagreements)/mean(identifications_count), num_identification_disagreements=sum(num_identification_disagreements), num_identification_agreements=sum(num_identification_agreements), identifications_count=sum(identifications_count), n()) %>%
  filter(disagreementScore > 0)

species$disagreementScore <- round(species$disagreementScore,digits=3)
species$'identifications count' <- species$`n()`

# Plot
species %>%
  ggplot(aes(disagreementScore,num_identification_disagreements,label=taxon.name)) +
  #geom_jitter() +
  geom_point(aes(colour=species$'identifications count')) +
  scale_color_viridis(species$'identifications count', name = "Observation number") +
  geom_text(aes(label=taxon.name),nudge_y=0.5,check_overlap = T,size=2) +
  ggtitle("Species with a large number of identifications tend to have more disagreements, \nbut a low score") +
  xlab("Disagreement score") +
  ylab("Disagreements number") +
  theme( axis.title.y = element_text(size=15),
         axis.text.y= element_text(size=13),
         axis.title.x = element_text(size=15),
         axis.text.x= element_text(size=13))


  


















