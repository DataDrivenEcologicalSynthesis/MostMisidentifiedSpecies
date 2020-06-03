setwd("/Users/brendathompson/Desktop/mylocalrepo")
bees <- read.csv('canada_bee_dataset.csv')

distinct(bees$species)

x<- length(unique(bees$species))
x<- length(unique(bees$genus))

unique(bees["species","genus"])

options(max.print=100000)
species_count = as.data.frame(table(bees$species, bees$genus))

# For every genus, sum the number of species

aggregate(species_count$Freq, by=list(Genus=species_count$Var2), FUN=sum)
