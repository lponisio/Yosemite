rm(list=ls())
setwd('analysis/network')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/diagnostics.R")
source("plotting/src/plotNetworkMets.R")
load('saved/mods/metrics.Rdata')

## ************************************************************
## network metrics by pyrodiversity
## ************************************************************
ys <- names(mods.div)
ylabs <- c("Pollinator \n redundancy",
           "Plant \n redundancy",
           "Pollinator \n complementarity",
           "Plant \n complementarity",
           "Pollinator \n generalization",
           "Plant \n generalization")
names(ylabs) <- ys


makePanels(mods=mods.div,
           xvars="simpson.div",
           xlabel= "Pyrodiversity",
           ys=ys,
           not.sig.2013=1:2,
           not.sig.2014=2:6)


## ************************************************************
## network metrics by Richness
## ************************************************************


ys <- names(mods.rich)
ylabs <- c("Pollinator \n redundancy",
           "Plant \n redundancy",
           "Pollinator \n complementarity",
           "Plant \n complementarity",
           "Plant \n generalization",
           "Pollinator \n generalization")

names(ylabs) <- ys

makePanels(mods=mods.rich,
           xvars= c("Richness", "FloralRichness"),
           xlabel= "Richness",
           ys=ys)
