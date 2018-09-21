rm(list=ls())
library(lme4)
library(vegan)
library(bipartite)
library(FD)
setwd('~/Dropbox/Yosemite/analysis/functionalDiv')
source('src/funcDivCalc.R')
source('src/funcDivPlotting.R')
source('src/make_structure.R')
source('src/misc.R')
load('../data/matrices/net/bees/sp.Rdata')
load('../data/specimens/spec.Rdata')

occ <- 'abund'
load(file='../beta-div/saved/communities/bee-abund.R')
load(file='../beta-div/saved/nulls/bee-abund.Rdata')

## ************************************************************
## functional diversity by drawing random species for each site
## with a set alpha, calculating the mean specialization
## ************************************************************
## ************************************************************
## bees only
## ************************************************************ 
## prep nulls
comm.bee$status <- lapply(comm.bee$status, function(x)
                          factor(x, levels=c('LOW',
                                             'MOD',
                                             'HIGH')))
comm.bee.all <- make.all.comm(comm.bee)

nulls.yr.strata(nulls.bee, comm.bee.all,
                years= unique(comm.bee$years),
                'saved/nulls/bee-traits.Rdata')

load(file='saved/nulls/bee-traits.Rdata')

## reference specialization
ref.specialization.bees <- null.traits('YoseSpec',
                                   comm.bee,
                                   comm.bee.all,
                                   nulls,
                                   sub='bees',
                                       binary=FALSE)
save(ref.specialization.bees, file="saved/traits/YoseSpec.Rdata")

## specialization
specialization.bees <- null.traits('PolSpec',
                                   comm.bee,
                                   comm.bee.all,
                                   nulls,
                                   sub='bees')
save(specialization.bees, file="saved/traits/spec.Rdata")

## Nesting
nestLoc <- null.traits('NestLoc', comm.bee,
                       comm.bee.all,
                       nulls,
                       sub='bees')
save(nestLoc, file="saved/traits/nestLoc.Rdata")

## excavate vs. rent
nestBuild <- null.traits('Excavate', comm.bee, comm.bee.all,
                         nulls,
                         sub='bees')
save(nestBuild, file="saved/traits/nestBuild.Rdata")

## sociality
social <- null.traits('Sociality', comm.bee, comm.bee.all,
                      nulls,
                      sub='bees')
save(social, file="saved/traits/social.Rdata")

## fire survival
fire <- null.traits('FireKill', comm.bee, comm.bee.all,
                      nulls,
                      sub='bees')
save(fire, file="saved/traits/firekill.Rdata")

## need wood
## sociality
wood <- null.traits('Wood', comm.bee, comm.bee.all,
                      nulls,
                      sub='bees')
save(wood, file="saved/traits/wood.Rdata")

## plotting
source('src/plot-func-div.R')
## ************************************************************
## models
## ************************************************************ 
sites <- unlist(comm.bee$sites)
statuses <- unlist(comm.bee$status)

statuses <- factor(statuses, levels=c("LOW", "MOD", "HIGH"))

ref.spec.bees.mod <- summary(lmer(ref.specialization.bees$obs$traits  ~
                              statuses*comm.bee$years +
                                  (1|sites)))

spec.bees.mod <- summary(lmer(specialization.bees$obs$traits  ~
                              statuses*comm.bee$years + (1|sites)))

nestLoc.mod <- summary(lmer(nestLoc$obs$traits  ~
                            statuses*comm.bee$years + (1|sites)))

nestBuild.mod <- summary(lmer(nestBuild$obs$traits  ~
                              statuses*comm.bee$years +
                              (1|sites)))

social.mod <- summary(lmer(social$obs$traits  ~
                           statuses*comm.bee$years +
                            (1|sites)))

fire.mod <- summary(lmer(fire$obs$traits  ~
                           statuses*comm.bee$years + (1|sites)))

wood.mod <- summary(lmer(wood$obs$traits  ~
                           statuses*comm.bee$years + (1|sites)))

## ************************************************************

## comm.plant <- make.struct(spec, type= 'PlantGenusSpecies')
## comm <- comm.plant$comm[[1]]
## ## changes abundances to occurrence
## comm[comm > 0] <- 1

## nulls.plant <- replicate(1000,
##                    commsimulator(comm, method= 'quasiswap'),
##                    simplify= FALSE)
## save(nulls.plant, file='saved/nulls/func-div-plant-nulls.Rdata')
## load(file='saved/nulls/func-div-plant-nulls.Rdata')

## comm.plant.all <- make.all.comm(comm.plant)
## plant.specialization <- null.traits(trait='PlantSpec',
##                                     comm=comm.plant,
##                                     comm.all=spec,
##                                     nulls=nulls.plant,
##                                     sub='Plant Specialization',
##                                     type='PlantGenusSpecies')
