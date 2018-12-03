setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")

xvars <- c("simpson.div", "FuncDis")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## network metrics
## ************************************************************

load('~/Dropbox/Yosemite/analysis/networkLevel/saved/mods/metrics.Rdata')

ys <- names(mods.div)
ylabs <- c("Nestedness", "Modularity", "Specialization",
           "Connectance", "Richness")
mods <- list(mods.div, mods.dis)

for(j in 1:length(xvars)){
    x <- xvars[j]
    print(x)
    for(i in 1:length(ys)){
        print(ys[i])
        dd.met <- expand.grid(xvar =seq(
                                  from=  min(dat.mods[, x]),
                                  to= max(dat.mods[, x]),
                                  length=10),
                              SiteStatus= c("LOW", "MOD", "HIGH"),
                              Year= c("2013", "2014"),
                              yvar=0)
        colnames(dd.met)[1] <- x
        colnames(dd.met)[4] <- ys[i]

        met.pi <- predict.int(mod= mods[[j]][[ys[i]]],
                              dd=dd.met,
                              y=ys[i],
                              family="guassian")

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
}

## ************************************************************
## ## robustness
## ## ************************************************************

ys <- "Robustness"
ylabs <- c("Robustness")
extinction.methods <- c("abund", "degree")
participants<- c("higher", "lower")

for(sp.level in participants){
    for(ex.method in extinction.methods){
        load(file.path('saved/mods/', sprintf('robustness_%s_%s.Rdata',
                                              ex.method, sp.level)))
        mods <- list(mod.div, mod.dis)
        for(j in 1:length(xvars)){
            x <- xvars[j]
            print(x)
            dd.met <- expand.grid(xvar =seq(
                                      from=  min(dat.mods[, x]),
                                      to= max(dat.mods[, x]),
                                      length=10),
                                  SiteStatus= c("LOW", "MOD", "HIGH"),
                                  Year= c("2013", "2014"),
                                  yvar=0)
            colnames(dd.met)[1] <- x
            colnames(dd.met)[4] <- ys

            met.pi <- predict.int(mod= mods[[j]],
                                  dd=dd.met,
                                  y=ys,
                                  family="guassian")

            plot.predict.div(new.dd=met.pi,
                             ylabel=ylabs,
                             dats=res,
                             xs=x,
                             y1=ys,
                             type=paste0(ex.method, sp.level),
                             xlabel=xlabel,
                             legend.loc="topleft",
                             legend.loc.year="topright")

        }
    }
}
