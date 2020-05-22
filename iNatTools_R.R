#/////////////////////////////////////////////#

# Getting the data

#/////////////////////////////////////////////#

library(devtools)
library(install_github)
install_github("pjhanly/iNatTools")
library(iNatTools)
# R Wrapper for downloading iNaturalist data

?iNat
data_disagree <- iNat(identifications = "most_disagree",created_d1 ="2020-05-16")
# Data added since last weekend where most users disagree on the identifications

data_disagree_multID <- subset(data_disagree,data_disagree$identifications_count>1)
# subsets the first data.frame to select observations that have more than 1 identifier. 

# Focussing on bees in Greater MTL
mtl_bee <- iNat(taxon_id = 47221, place_id = 139965)
#all records of honey bees, bumblebees, and allies in MTL area
