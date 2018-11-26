rm(list=ls())
veg <- read.csv("~/Dropbox/Yosemite/Data/Yosemite_Veg/YosemiteVeg2013.csv")

spec <- read.csv("~/Dropbox/Yosemite/Data/Specimens/Yosemite_specimens_081913.csv")

spec <- spec[spec$NetPan =="net",]

spec.cat <- paste(spec$Site, spec$PlantSp)
veg.cat <- paste(veg$Site, veg$PlantSp)

spec[!spec.cat %in% veg.cat]