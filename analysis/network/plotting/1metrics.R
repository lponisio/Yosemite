## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/network')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/diagnostics.R")
source("plotting/src/plotNetworkMets.R")

xvars <- c("simpson.div")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## network metrics
## ************************************************************

load('saved/mods/metrics.Rdata')

ys <- names(mods.div)
ylabs <- c("Plant functional \n complementarity",
           "Pollinator functional \n complementarity",
           "Plant functional \n redundancy",
           "Pollinator functional \n redundancy",
           "Generalization")

names(ylabs) <- ys
x <- xvars

pdf.f(plotNetworkMets, file=file.path("figures",
                        sprintf("%s.pdf", type)),
      width=5, height=8)
