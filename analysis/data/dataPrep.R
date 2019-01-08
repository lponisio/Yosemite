rm(list=ls())
library(vegan)
library(fields)
library(bipartite)
library(fossil)

## setwd('~/Dropbox/Yosemite/')
setwd("data")
source('speciesIDs/src/AssignSpecies.R')

setwd('relational/relational')
source('../src/relational_prep.R')
source('../src/relational_make.R')
source('../src/make_traditional.R')
sr.sched <- read.csv('tables/conditions.csv')
spec <- read.csv('traditional/specimens-complete.csv')
veg <- read.csv('traditional/veg-complete.csv')
cond <- read.csv("tables/conditions.csv")

setwd('../../../analysis/data/')
source('src/misc.R')

## drop pan data
spec <- spec[spec$NetPan == "net",]
## drop non-bees
bee.fams <- c("Halictidae", "Andrenidae", "Apidae", "Megachilidae",
              "Colletidae")
spec <- spec[spec$Family %in% bee.fams,]
## correct date format
spec$Date <- as.Date(spec$Date, format='%m/%d/%y')
spec$doy <- as.numeric(strftime(spec$Date, format='%j'))
spec$Year <- as.factor(spec$Year)
## get specimen data ready

## drop extra round
# from specimens
extra.round <- spec$Site == 'L21' & spec$Date == '2014-07-01'
spec <- spec[!extra.round,]
## from sampling schedule
sr.sched$Date <- as.Date(sr.sched$Date, format='%m/%d/%y')
extra.round <- sr.sched$Site == 'L21' & sr.sched$Date == '2014-07-01'
sr.sched <- sr.sched[!extra.round,]
## from veg data
extra.round <- veg$Site == 'L21' & veg$Date == '2014-07-01'
veg <- veg[!extra.round,]
## from conditions
cond$Date <- as.Date(cond$Date, format='%m/%d/%y')
extra.round <- cond$Site == 'L21' & cond$Date == '2014-07-01'
cond <- cond[!extra.round,]

## create genus species columns
spec$GenusSpecies <- fix.white.space(paste(spec$Genus,
                                           spec$Species,
                                           spec$SubSpecies))

spec$PlantGenusSpecies <-  fix.white.space(paste(spec$PlantGenus,
                                                 spec$PlantSpecies,
                                                 spec$PlantVar,
                                                 spec$PlantSubSpecies))
spec <-  spec[spec$GenusSpecies != '',]
spec <-  spec[spec$PlantGenusSpecies != '',]

## interactions
spec$Int <-  fix.white.space(paste(spec$GenusSpecies,
                                   spec$PlantGenusSpecies))
spec$IntGen <-  fix.white.space(paste(spec$Genus,
                                      spec$PlantGenus))

print(paste("Bee species", length(unique(spec$GenusSpecies))))
print(paste("Plant species", length(unique(spec$PlantGenusSpecies))))
print(paste("Bee genera", length(unique(spec$Genus))))
print(paste("Interactions", length(unique(spec$Int))))
print(paste("Specimens", nrow(spec)))


## create plant by pollinator matrix to calculate specialization
prep.comm <- aggregate(spec$GenusSpecies,
                       list(PlantGenusSpecies=spec$PlantGenusSpecies,
                            PolGenusSpecies=spec$GenusSpecies,
                            Year=spec$Year),
                       length)

save(spec, file='specimens/spec.Rdata')

##******************************************************
## veg data
##******************************************************
## correct date format
veg$Date <- as.Date(veg$Date, format='%m/%d/%y')
veg$Year <- format(veg$Date, format='%Y')
veg$doy <- as.numeric(strftime(veg$Date, format='%j'))

## drop data from incomplete sampling round
extra.round <- veg$Site == 'L21' & veg$Date == '2014-07-01'
veg <- veg[!extra.round,]

veg$FlowerNum[is.na(veg$FlowerNum)] <- 0
veg$Occ <- veg$FlowerNum
veg$Occ[veg$Occ > 0] <- 1

veg$logFlowerNum <- veg$FlowerNum
veg$logFlowerNum[veg$logFlowerNum == 2] <- 10
veg$logFlowerNum[veg$logFlowerNum == 3] <- 100
veg$logFlowerNum[veg$logFlowerNum == 4] <- 1000
veg$logFlowerNum[veg$logFlowerNum == 5] <- 10000

veg$PlantGenusSpecies <-  fix.white.space(paste(veg$PlantGenus,
                                                veg$PlantSpecies,
                                                veg$PlantVar,
                                                veg$PlantSubSpecies))
veg$SiteStatus <- spec$SiteStatus[match(veg$Site,
                                        spec$Site)]
write.csv(veg, 'veg/veg.csv', row.names=FALSE)
save(veg, file='veg/veg.Rdata')

##******************************************************
## site level data
##******************************************************
cond$doy <- as.numeric(strftime(cond$Date, format="%j"))
samp.sr <- data.frame(doy=cond$doy,
                      Site=cond$Site,
                      Year=cond$Year,
                      Richness=0,
                      Abund=0,
                      Div=0)
samp.sr <- unique(samp.sr, MARGIN=1)

source('~/Dropbox/yosemite/analysis/data/src/siteLevelPrep.R')

