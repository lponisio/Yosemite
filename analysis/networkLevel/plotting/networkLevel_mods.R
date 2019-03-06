setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")
source("plotting/src/diagnostics.R")
library(RColorBrewer)

xvars <- c("simpson.div")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## network metrics
## ************************************************************

load('~/Dropbox/Yosemite/analysis/networkLevel/saved/mods/metrics.Rdata')

ys <- names(mods.div)
## ylabs <- c("Nestedness", "Modularity", "Specialization",
##            "Connectance", "Interaction evenness",
##            "Pollinator richness", "Pollinator Vulnerability",
##            "Pollinator  mean shared partners",
##            "Pollinator mean niche overlap",
##            "Pollinator partner Diversity",
##            "Pollinator complementarity",
##            "Pollinator mean d'",
##            "Plant richness",
##            "Plant  mean shared partners",
##            "Plant mean niche overlap",
##            "Plant partner Diversity",
##            "Plant complementarity", "Plant mean d'")


ylabs <- ys

x <- xvars

for(i in 1:length(ys)){
    print(ys[i])
    dd.met <- expand.grid(xvar =seq(
                              from=  min(cor.dats[, x]),
                              to= max(cor.dats[, x]),
                              length=10),
                          SiteStatus= c("all"),
                          Year= c("2013", "2014"),
                          yvar=0)
    colnames(dd.met)[1] <- x
    colnames(dd.met)[4] <- ys[i]

    met.pi <- predict.int(mod= mods.div[[ys[i]]],
                          dd=dd.met,
                          y=ys[i],
                          family="guassian")
    cor.dats$SiteStatus <- "all"
    plot.predict.div(new.dd=met.pi,
                     ylabel=ylabs[i],
                     dats=cor.dats,
                     xs=x,
                     y1=ys[i],
                     type=type,
                     xlabel=xlabel,
                     legend.loc="bottomleft",
                     legend.loc.year="topright")

}

