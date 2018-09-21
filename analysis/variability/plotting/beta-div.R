## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('plotting/src/predictIntervals.R')
source('plotting/src/CIplotting.R')
source('plotting/src/plotPanels.R')
library(RColorBrewer)

only.baci <- TRUE
args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
    type <- "pols"
    binary <- FALSE
} else{
    type <- args[1]
    if.occ <- args[2]
    if(if.occ == "occ"){
        binary <- TRUE
    } else{
        binary <- FALSE
    }
}
alpha <- TRUE

source('src/initialize_beta.R')

xvar.species <- c("r.degree", "median.abund", "median.days")

load(file= file.path('saved/speciesTurnover',
                     sprintf('%s.Rdata', paste(dis.method, alpha, occ, type,
                                             sep='_'))))

ys <- c("dist")
ylabs <- c("Variability in \n interaction partners")

## ************************************************************
## pols
## ************************************************************

## plot for degree
r.degree.dd <- expand.grid(r.degree=seq(from= min(dats.type$r.degree,
                                                  na.rm=TRUE),
                                        to= max(dats.type$r.degree,
                                                na.rm=TRUE),
                                        length=10),
                           median.days=mean(dats.type$median.days,
                                           na.rm=TRUE),
                           SiteStatus = "all",
                           dist = 0)

dd.pi <- predict.int(mod= degree.type.mod.beta,
                     dd=r.degree.dd,
                     y=ys,
                     family="gaussian")

plot.predict.ypr(new.dd=dd.pi,
                 ylabel=ylabs,
                 dats=dats.type,
                 y1=ys,
                 extinction.method=paste(type, occ, sep="_"),
                 agg.col="species",
                 xs="r.degree",
                 xlabel= "Degree")


## plot for abundance
abund.dd <- expand.grid(median.abund=seq(from= min(dats.type$median.abund,
                                                  na.rm=TRUE),
                                        to= max(dats.type$median.abund,
                                                na.rm=TRUE),
                                        length=10),
                           median.days=mean(dats.type$median.days,
                                           na.rm=TRUE),
                           SiteStatus="all",
                           dist = 0)


dd.pi.pols <- predict.int(mod= abund.type.mod.beta,
                     dd=abund.dd,
                     y=ys,
                     family="gaussian")

plot.predict.ypr(new.dd=dd.pi.pols,
                 ylabel=ylabs,
                 dats=dats.type,
                 y1=ys,
                 extinction.method=paste(type, occ, sep="_"),
                 agg.col="species",
                 xs=xvar.species[2],
                 xlabel= "Abundance")

## plot for median days
days.dd <- expand.grid(median.days=seq(from= min(dats.type$median.days,
                                                   na.rm=TRUE),
                                         to= max(dats.type$median.days,
                                                 na.rm=TRUE),
                                         length=10),
                       median.abund=mean(dats.type$median.abund,
                                         na.rm=TRUE),
                        SiteStatus="all",
                        dist = 0)

days.pi.pols <- predict.int(mod= abund.type.mod.beta,
                          dd=days.dd,
                          y=ys,
                          family="gaussian")

plot.predict.ypr(new.dd=days.pi.pols,
                 ylabel=ylabs,
                 dats=dats.type,
                 y1=ys,
                 extinction.method=paste(type, occ, sep="_"),
                 agg.col="species",
                 xs=xvar.species[3],
                 xlabel= "Phenology")
