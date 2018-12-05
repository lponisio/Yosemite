## setwd('~/Dropbox/Yosemite')
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
## ## robustness
## ## ************************************************************

ys <- "Robustness"
ylabs <- c("Robustness")
extinction.methods <- c("abund")
participants<- c("lower")

for(sp.level in participants){
    for(ex.method in extinction.methods){
        load(file.path('saved/mods/', sprintf('robustness_%s_%s.Rdata',
                                              ex.method, sp.level)))
        mods <- list(mod.div)
        for(j in 1:length(xvars)){
            x <- xvars[j]
            print(x)
            dd.met <- expand.grid(xvar =seq(
                                      from=  min(dat.mods[, x]),
                                      to= max(dat.mods[, x]),
                                      length=10),
                                  SiteStatus= c("LOW", "MOD", "HIGH"),
                                  yvar=0)
            colnames(dd.met)[1] <- x
            colnames(dd.met)[3] <- ys

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
                             legend.loc.year="topright",
                             by.year=FALSE,
                             height=4, width=3.5)


            plotDiag <- function(){
                plotDiagnostics(mods=mods[[j]], dats=res)
            }

            pdf.f(plotDiag,
                  file=file.path('figures/diagnostics/', sprintf('robustness_%s_%s.pdf',
                                              ex.method, sp.level)),
                  height=7, width=3)


        }
    }
}

