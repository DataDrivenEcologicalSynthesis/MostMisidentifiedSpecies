library(iNatTools)

beecanada <- read.csv("data/Bees_canada_26_05_20.csv")

beecanda_verif <- beecanada[ ! beecanada$quality_grade=="casual",]

library(plyr)
temp <- vector(mode="list", length=nrow(beecanda_verif))
do.call()


bee_canada_inat <- iNat(id=beecanda_verif$id[1])

for (i in (2:nrow(beecanda_verif))){
  temp[[i]] <- iNat(id=beecanda_verif$id[i])
  print(paste("Now doing", beecanda_verif$id[i]))
}

error <- subset(beecanda_verif,beecanda_verif$id==1718549)
error2 <- iNat(id=1718549)

