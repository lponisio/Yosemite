## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize-delta.R')
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")
source("plotting/src/diagnostics.R")

## ************************************************************
## delta abund
## ************************************************************

load('~/Dropbox/Yosemite/analysis/variability/saved/mods/drought.Rdata')


## network role
dd.abund.var.pca <- expand.grid(var.pca1=seq(from= min(delta$delta$var.pca1),
                                   to=max(delta$delta$var.pca1), length=10),
                      simpson.div = mean(delta$delta$simpson.div),
                      beta.dist=mean(delta$delta$beta.dist),
                      deltaFloralAbund=mean(delta$delta$deltaFloralAbund),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      deltaAbund=0)

abund.var.pca.pi <- predict.int(mod= mods,
                      dd=dd.abund.var.pca,
                      y="deltaAbund",
                      family="guassian")

plot.predict.div(new.dd=abund.var.pca.pi,
                 ylabel="Relative change \n in abundance",
                 dats=delta$delta[delta$delta$deltaAbund != 0,],
                 xs="var.pca1",
                 y1="deltaAbund",
                 type="all",
                 xlabel="Role Flexibility",
                 legend.loc="topleft")

## partner
dd.abund.beta.dist <- expand.grid(beta.dist=seq(from= min(delta$delta$beta.dist),
                                    to=max(delta$delta$beta.dist), length=10),
                      simpson.div = mean(delta$delta$simpson.div),
                      var.pca1=mean(delta$delta$var.pca1),
                      deltaFloralAbund=mean(delta$delta$deltaFloralAbund),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      deltaAbund=0)

abund.beta.dist.pi <- predict.int(mod= mods,
                      dd=dd.abund.beta.dist,
                      y="deltaAbund",
                      family="guassian")

plot.predict.div(new.dd=abund.beta.dist.pi,
                 ylabel="Relative change \n in abundance",
                 dats=delta$delta[delta$delta$deltaAbund != 0,],
                 xs="beta.dist",
                 y1="deltaAbund",
                 type="all",
                 xlabel="Partner Flexibility",
                 legend.loc="topleft")


plotDeltaDiag <- function(){
              plotDiagnostics(mods=mods, dats=delta$delta[delta$delta$deltaAbund != 0,])
}

pdf.f(plotDeltaDiag, file="figures/diagnostics/deltaAbund.pdf",
      height=9, width=3)

## ************************************************************
## persist or extinct
## ************************************************************
## role
dd.persist.var.pca <- expand.grid(var.pca1=seq(from= min(delta$delta$var.pca1),
                                   to=max(delta$delta$var.pca1), length=10),
                      simpson.div = mean(delta$delta$simpson.div),
                      beta.dist=mean(delta$delta$beta.dist),
                      Abund=mean(delta$delta$Abund),
                      deltaFloralAbund=mean(delta$delta$deltaFloralAbund),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      Persist=0)

persist.var.pca.pi <- predict.int(mod= mods.ext,
                      dd=dd.persist.var.pca,
                      y="Persist",
                      family="binomial")

plot.predict.div(new.dd=persist.var.pca.pi,
                 ylabel="Proportion persistence",
                 dats=delta$delta,
                 xs="var.pca1",
                 y1="Persist",
                 type="all",
                 xlabel="Role Flexibility",
                 legend.loc="topleft")

## partner
dd.persist.beta.dist <- expand.grid(beta.dist=seq(from= min(delta$delta$beta.dist),
                                    to=max(delta$delta$beta.dist), length=10),
                      Abund=mean(delta$delta$Abund),
                      simpson.div = mean(delta$delta$simpson.div),
                      var.pca1=mean(delta$delta$var.pca1),
                      deltaFloralAbund=mean(delta$delta$deltaFloralAbund),
                      SiteStatus= c("LOW", "MOD", "HIGH"),
                      Persist=0)

persist.beta.dist.pi <- predict.int(mod= mods.ext,
                      dd=dd.persist.beta.dist,
                      y="Persist",
                      family="binomial")


plot.predict.div(new.dd=persist.beta.dist.pi,
                 ylabel="Proportion persistence",
                 dats=delta$delta,
                 xs="beta.dist",
                 y1="Persist",
                 type="all",
                 xlabel="Role Flexibility",
                 legend.loc="topleft")


plotPersistDiag <- function(){
              plotDiagnostics(mods=mods.ext, dats=delta$delta)
}

pdf.f(plotPersistDiag, file="figures/diagnostics/persist.pdf",
      height=9, width=3)



plot.panels()
