#/////////////////////////////////////////////#

# Getting the data

#/////////////////////////////////////////////#


install_github("pjhanly/iNatTools")
library(iNatTools)
# R Wrapper for downloading iNaturalist data

?iNat
data_disagree <- iNat(identifications = "most_disagree",created_d1="2020-05-16")
# Data added since last weekend where most users disagree on the identifications

data_disagree_multID <- subset(data_disagree,data_disagree$identifications_count>1)
# subsets the first data.frame to select observations that have more than 1 identifier. 



