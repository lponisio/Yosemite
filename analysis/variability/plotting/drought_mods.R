setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')

source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")

## ************************************************************
## network metrics
## ************************************************************

load('~/Dropbox/Yosemite/analysis/variability/saved/mods/drought.Rdata')


## network role
dd.met <- expand.grid(var.pca1=seq(from= min(delta$var.pca1),
                      to=max(delta$var.pca1), length=10),
                      simpson.div = mean(delta$simpson.div),
                      beta.dist=mean(delta$beta.dist),
                      mean.pca1=mean(delta$mean.pca1),
                      deltaFloralRichness=mean(delta$deltaFloralRichness),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      deltaAbund=0)

met.pi <- predict.int(mod= mods,
                      dd=dd.met,
                      y="deltaAbund",
                      family="guassian")

plot.predict.div(new.dd=met.pi,
                 ylabel="Relative change \n in abundance",
                 dats=delta,
                 xs="var.pca1",
                 y1="deltaAbund",
                 type="all",
                 xlabel="Role Flexibility",
                 legend.loc="topleft")

## partner


dd.met <- expand.grid(beta.dist=seq(from= min(delta$beta.dist),
                      to=max(delta$beta.dist), length=10),
                      simpson.div = mean(delta$simpson.div),
                      var.pca1=mean(delta$var.pca1),
                      mean.pca1=mean(delta$mean.pca1),
                      deltaFloralRichness=mean(delta$deltaFloralRichness),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      deltaAbund=0)

met.pi <- predict.int(mod= mods,
                      dd=dd.met,
                      y="deltaAbund",
                      family="guassian")

plot.predict.div(new.dd=met.pi,
                 ylabel="Relative change \n in abundance",
                 dats=delta,
                 xs="beta.dist",
                 y1="deltaAbund",
                 type="all",
                 xlabel="Partner Flexibility",
                 legend.loc="topleft")
