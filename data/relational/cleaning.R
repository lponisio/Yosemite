rm(list=ls())
setwd("~/Dropbox/Yosemite/data")

original <-
  read.csv("relational/original/specimens/cleaned/specimens.csv")
traditional <-
  read.csv("relational/relational/traditional/specimens-complete.csv")

weather <- read.csv("relational/preped_data/weather.csv")

missingIDs <- original$UniqueID[!original$UniqueID %in%
                                traditional$UniqueID]

missing <- original[original$UniqueID %in% missingIDs,]


table(paste(missing$Site, missing$Date))
