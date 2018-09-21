setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
source('plotting/src/predictIntervals.R')
source('plotting/src/CIplotting.R')
source('plotting/src/plotPanels.R')


xvar.species <- c("r.degree", "median.abund", "median.days")
load(file="../../../speciesRoles_saved/pcaScores.Rdata")

ys <- c("var.pca1")
ylabs <- c("Variability in \n network role")

## ************************************************************
## pols
## ************************************************************
pol.pca.scores$pca.var <-
    pol.pca.scores$pca.var[!is.na(pol.pca.scores$pca.var[,ys]),]
plant.pca.scores$pca.var <-
    plant.pca.scores$pca.var[!is.na(plant.pca.scores$pca.var[,ys]),]
## pol.pca.scores$pca.var$r.degree <-
##     scale(log(pol.pca.scores$pca.var$r.degree))
## pol.pca.scores$pca.var$median.abund <-
##     scale(log(pol.pca.scores$pca.var$median.abund))
## pol.pca.scores$pca.var$median.days <-
##     scale(log(pol.pca.scores$pca.var$median.days))

pol.pca.scores$pca.var$var.pca1 <-
    sqrt(pol.pca.scores$pca.var$var.pca1)

## plot for degree
r.degree.dd <- expand.grid(r.degree=seq(from= min(pol.pca.scores$pca.var$r.degree,
                                                  na.rm=TRUE),
                                        to= max(pol.pca.scores$pca.var$r.degree,
                                                na.rm=TRUE),
                                        length=10),
                           median.days=mean(pol.pca.scores$pca.var$median.days,
                                           na.rm=TRUE),
                           SiteStatus="all",
                           var.pca1 = 0)

dd.pi <- predict.int(mod= degree.pol.mod.pca,
                     dd=r.degree.dd,
                     y=ys,
                     family="gaussian")

plot.predict.ypr(new.dd=dd.pi,
                 ylabel=ylabs,
                 dats=pol.pca.scores$pca.var,
                 y1=ys,
                 extinction.method="pols",
                 agg.col=NULL,
                 xs=xvar.species[1],
                 xlabel= "Degree")




## plot for abundance
abund.dd <- expand.grid(median.abund=seq(from= min(pol.pca.scores$pca.var$median.abund,
                                                  na.rm=TRUE),
                                        to= max(pol.pca.scores$pca.var$median.abund,
                                                na.rm=TRUE),
                                        length=10),
                           median.days=mean(pol.pca.scores$pca.var$median.days,
                                           na.rm=TRUE),
                           SiteStatus="all",
                           var.pca1 = 0)


dd.pi.pols <- predict.int(mod= abund.pol.mod.pca,
                     dd=abund.dd,
                     y=ys,
                     family="gaussian")

plot.predict.ypr(new.dd=dd.pi.pols,
                 ylabel=ylabs,
                 dats=pol.pca.scores$pca.var,
                 y1=ys,
                 extinction.method="pols",
                 agg.col=NULL,
                 xs=xvar.species[2],
                 xlabel= "Abundance")
