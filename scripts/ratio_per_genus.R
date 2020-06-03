#=======================
# Exploration of disagreement and agreement scores distribution on genus
# Score: disagreements to identifications and agreements to identifications ratios
# June 2, 2020
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


#### Plot distribution of variables ####

# Identifications count
  # # Open new plot window
  quartz(width = 10,height = 6)
  
  # # Plot 
  data %>%
    with(
      hist(identifications_count,
           main="Identifications count density plot",
           xlab="Identifications count"))

# num_identification_disagreements count
  # # Open new plot window
  quartz(width = 10,height = 6)
  
  # # Plot
  data %>%
    with(
      hist(num_identification_disagreements,
           main="num_identification_disagreements count density plot",
           xlab="num_identification_disagreements count"))

# num_identification_agreements count
  # # Open new plot window
  quartz(width = 10,height = 6)
  
  # # Plot
  data %>%
    with(
      hist(num_identification_agreements,
           main="num_identification_agreements count density plot",
           xlab="num_identification_agreements count"))


#### Plot observations per genus ####

# Open new plot window
quartz(width = 10,height = 6)

# Plot
data %>%
  filter(taxon.rank == "genus" | taxon.rank == "subgenus" | taxon.rank == "species" | taxon.rank == "subspecies") %>%
  mutate(disagreementScore = (num_identification_disagreements/identifications_count)) %>%
  subset(disagreementScore>0) %>%
  add_column(genus = sub(" [A-z ]*", "", .$taxon.name)) %>%
  group_by(genus) %>%
  summarize(count = n()) %>%
  ggplot(aes(genus, count, fill = genus)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis("genus",discrete=TRUE) +
  theme(axis.title.y = element_text(size=25),
        axis.text.y= element_text(size=20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x= element_text(size=20, angle=45, hjust=1))


#### Plot disagreements/identifications score per genus ####

# Open new plot window
quartz(width = 10,height = 6)

# Plot
data %>%
  filter(taxon.rank == "genus" | taxon.rank == "subgenus" | taxon.rank == "species" | taxon.rank == "subspecies") %>%
  mutate(disagreementScore = (num_identification_disagreements/identifications_count)) %>%
  subset(disagreementScore>0) %>%
  add_column(genus = sub(" [A-z ]*", "", .$taxon.name)) %>%
  #group_by(genus) %>%
  #summarize(mean_ratio = mean(disagreementScore, na.rm = TRUE)) %>%
  ggplot(aes(genus, disagreementScore, fill = genus)) +
  geom_boxplot(aes(alpha=0.5), na.rm = T, outlier.shape = NA) +
  geom_jitter(aes(colour = genus), na.rm = T) +
  ylim(0, 1) +
  scale_color_viridis("genus",alpha=0.6,discrete = TRUE) +
  scale_fill_viridis("genus",discrete=TRUE) +
  theme(axis.title.y = element_text(size=25),
        axis.text.y= element_text(size=20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x= element_text(size=20, angle=45, hjust=1))


#### Plot agreements/identifications score per genus ####

# Open new plot window
quartz(width = 10,height = 6)

# Plot observations agreement score per genus (1 point per observation)
data %>%
  filter(taxon.rank == "genus" | taxon.rank == "subgenus" | taxon.rank == "species" | taxon.rank == "subspecies") %>%
  mutate(agreementScore = num_identification_agreements/identifications_count) %>%
  # # Remove Ratios of 1 to focus on observations that have some degree of id disagreement
  subset(agreementScore<1) %>%                                                 
  add_column(genus = sub(" [A-z ]*", "", .$taxon.name)) %>%
  # # Plot
  ggplot(aes(genus, agreementScore, fill = genus)) +
  geom_boxplot(aes(alpha=0.5), na.rm = T, outlier.shape = NA) +
  geom_jitter(aes(colour = genus), na.rm = T) +
  ylim(0, 1) +
  scale_color_viridis("genus",alpha=0.6,discrete = TRUE) +
  scale_fill_viridis("genus",discrete=TRUE) +
  theme(axis.title.y = element_text(size=25),
        axis.text.y= element_text(size=20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x= element_text(size=20, angle=45, hjust=1))

# Open new plot window
quartz(width = 10,height = 6)

# Plot species agreement score per genus (1 point per species)
data %>%
  filter(taxon.rank == "genus" | taxon.rank == "subgenus" | taxon.rank == "species" | taxon.rank == "subspecies") %>%
  group_by(taxon.name) %>%
  summarize(agreementScore = mean(num_identification_agreements)/mean(identifications_count)) %>%
  # # Remove Ratios of 1 to focus on species that have some degree of id disagreement
  subset(agreementScore<1) %>%                                                 
  add_column(genus = sub(" [A-z ]*", "", .$taxon.name)) %>%
  # # Plot
  ggplot(aes(genus, agreementScore, fill = genus)) +
  geom_boxplot(aes(alpha=0.5), na.rm = T, outlier.shape = NA) +
  geom_jitter(aes(group = genus,colour = genus), na.rm = T) +
  ylim(0, 1) +
  scale_color_viridis("genus",alpha=0.6,discrete = TRUE) +
  scale_fill_viridis("genus",discrete=TRUE) +
  theme(axis.title.y = element_text(size=25),
        axis.text.y= element_text(size=20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x= element_text(size=20, angle=45, hjust=1))


# Open new plot window
quartz(width = 10,height = 6)

# Plot species agreement score per genus (1 point per observation)
data %>%
  filter(taxon.rank == "genus" | taxon.rank == "subgenus" | taxon.rank == "species" | taxon.rank == "subspecies") %>%
  group_by(taxon.name) %>%
  mutate(agreementScore = mean(num_identification_agreements)/mean(identifications_count)) %>%
  ungroup() %>%
  # # Remove Ratios of 1 to focus on species that have some degree of id disagreement
  subset(agreementScore<1) %>%                                                 
  add_column(genus = sub(" [A-z ]*", "", .$taxon.name)) %>%
  # # Plot
  ggplot(aes(genus, agreementScore, fill = genus)) +
  geom_boxplot(aes(alpha=0.5), na.rm = T, outlier.shape = NA) +
  geom_jitter(aes(group = genus,colour = genus), na.rm = T) +
  ylim(0, 1) +
  scale_color_viridis("genus",alpha=0.5,discrete = TRUE) +
  scale_fill_viridis("genus",discrete=TRUE) +
  theme(axis.title.y = element_text(size=25),
        axis.text.y= element_text(size=20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x= element_text(size=20, angle=45, hjust=1))

  







