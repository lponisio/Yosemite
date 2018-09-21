rm(list=ls())
library(lme4)
library(vegan)
library(bipartite)
library(FD)
setwd('~/Dropbox/Yosemite/analysis/functionalDiv')
source('src/funcDivCalc.R')
source('src/misc.R')
source('src/make_structure.R')
source('src/analyses.R')
load('../data/matrices/net/bees/sp.Rdata')

## ************************************************************
## functional dispersion, richness, evenness and divergence
## ************************************************************

comm.pol <- make.struct(spec, type= 'GenusSpecies')
site.by.yr <- data.frame(unique(spec$Site),
                         rep(4, length(unique(spec$Site))))
colnames(site.by.yr) <- c('Site', '2013')

trait.mat <- unique(spec[,c('GenusSpecies', 'Sociality', 'NestLoc',
                             'Excavate', 'PolSpec')], MARGIN=1)
rownames(trait.mat) <- trait.mat$GenusSpecies

## trait.mat <- trait.mat[,-1]
## traits <- list(cts=c('PolSpec'),
##                cat=c('Sociality', 'NestLoc','Excavate'))

trait.mat <- trait.mat[,-c(1, 3)]
traits <- list(cts=c('PolSpec'),
               cat=c('Sociality', 'Excavate'))

mat.tmp <- comm.pol$comm[[1]]
mat.y <- array(mat.tmp, dim=c(dim(mat.tmp)[1], 1, dim(mat.tmp)[2]))
dimnames(mat.y) <- list(site=dimnames(mat.tmp)$site,
                        year='2013',
                        species=dimnames(mat.tmp)$spp)

trait.mat <- trait.mat[match(dimnames(mat.y)$species,
                             rownames(trait.mat)),]

drop.sp <- which(apply(trait.mat, 1, function(x) any(is.na(x))))
mat.y <- mat.y[,,-drop.sp,drop=FALSE]
trait.mat <- trait.mat[-drop.sp,]

dd.model <- list(mat.y=mat.y,
                 dd.traits=trait.mat,
                 traits=traits,
                 status=comm.pol$status[[1]])

res <- run.analysis(dd.model=dd.model)

summary(res$FRic$model.out)
summary(res$FEve$model.out)
summary(res$FDiv$model.out)
summary(res$FDis$model.out)


## ************************************************************
## functional diversity by drawing random species for each site with a
## set alpha, calculating the mean specialization
## ************************************************************
comm.pol <- make.struct(spec, type= 'GenusSpecies')
comm <- comm.pol$comm[[1]]
## changes abundances to occurrence
comm[comm > 0] <- 1

## nulls <- replicate(1000,
##                    commsimulator(comm, method= 'quasiswap'),
##                    simplify= FALSE)
## save(nulls, file='saved/nulls/func-div-nulls.Rdata')
load(file='saved/nulls/func-div-nulls.Rdata')

## ************************************************************
## specialization
specialization <- null.traits('PolSpec', comm.pol, spec,
                              nulls, 'Specialization')

## Nesting
nestLoc <- null.traits('NestLoc', comm.pol, spec,
                              nulls, 'Nest Location Diversity')

## excavate vs. rent
nestBuild <- null.traits('Excavate', comm.pol, spec,
                              nulls, 'Nest Construction Diversity')

## sociality
social <- null.traits('Sociality', comm.pol, spec,
                              nulls, 'Behavioral Diversity')


## ************************************************************
comm.plant <- make.struct(spec, type= 'PlantGenusSpecies')
comm <- comm.plant$comm[[1]]
## changes abundances to occurrence
comm[comm > 0] <- 1

nulls.plant <- replicate(1000,
                   commsimulator(comm, method= 'quasiswap'),
                   simplify= FALSE)
save(nulls.plant, file='saved/nulls/func-div-plant-nulls.Rdata')
load(file='saved/nulls/func-div-plant-nulls.Rdata')


plant.specialization <- null.traits('PlantSpec', comm.plant, spec,
                              nulls.plant, 'Plant Specialization',
                                    type='PlantGenusSpecies')
