## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/diagnostics.R")

xvars <- c("simpson.div")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## ## robustness
## ## ************************************************************

ys <- "Robustness"
ylabs <- c("Co-extinction cascade resistance")

extinction.methods <- c("veg", "degree", "visit")
participants<- c("lower")
net.type <- c("obs", "potential")

for(net in net.type){
    for(sp.level in participants){
        for(ex.method in extinction.methods){
            load(file.path('saved/mods/',
                            sprintf('robustness_%s_%s_%s.Rdata',
                                        ex.method, sp.level, net)))
            mods <- list(mod.div)
            for(j in 1:length(xvars)){
                x <- xvars[j]
                print(x)
                dd.met <- expand.grid(xvar =seq(
                                          from=  min(dat.mods[, x]),
                                          to= max(dat.mods[, x]),
                                          length=10),
                                      Year= c("2013", "2014"),
                                       SiteStatus= c("all"),
                                      yvar=0)
                colnames(dd.met)[1] <- x
                colnames(dd.met)[4] <- ys

                met.pi <- predict.int(mod= mods[[j]],
                                      dd=dd.met,
                                      y=ys,
                                      family="guassian")
                leg.labs <- c("Drought",
                              "Extreme drought")
                names(leg.labs) <- c("2013", "2014")

                res$SiteStatus <-  "all"
                plot.predict.div(new.dd=met.pi,
                                 ylabel=ylabs,
                                 dats=res,
                                 xs=x,
                                 y1=ys,
                                 type=paste0(ex.method, sp.level, net),
                                 xlabel=xlabel,
                                 legend.loc="topleft",
                                 legend.loc.year="bottomleft",
                                 by.year=TRUE,
                                 height=4, width=6,
                                 leg.labs=leg.labs)


                plotDiag <- function(){
                    plotDiagnostics(mods=mods[[j]], dats=res)
                }

                pdf.f(plotDiag,
                      file=file.path('figures/diagnostics/',
                                      sprintf('robustness_%s_%s_%s.pdf',
                                        ex.method, sp.level, net)),
                      height=7, width=3)


            }
        }
    }
}

