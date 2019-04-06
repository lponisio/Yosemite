## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('plotting/src/initialize.R')

## ************************************************************
## delta abund
## ************************************************************
load('saved/mods/drought.Rdata')
delta <- delta$delta

## network role
dd.abund.var.pca <- expand.grid(var.pca1=seq(
                                    from= min(delta$var.pca1),
                                    to=max(delta$var.pca1),
                                    length=10),
                                simpson.div = mean(delta$simpson.div),
                                beta.dist=mean(delta$beta.dist),
                                deltaFloralAbund=
                                    mean(delta$deltaFloralAbund),
                                deltaAbund=0)
abund.var.pca.pi <- predict.int(mod= mods,
                                dd=dd.abund.var.pca,
                                y="deltaAbund",
                                family="guassian")

## partner
dd.abund.beta.dist <- expand.grid(beta.dist=seq(
                                      from= min(delta$beta.dist),
                                      to=max(delta$beta.dist),
                                      length=10),
                                  simpson.div = mean(delta$simpson.div),
                                  var.pca1=mean(delta$var.pca1),
                                  deltaFloralAbund=
                                      mean(delta$deltaFloralAbund),
                                  deltaAbund=0)
abund.beta.dist.pi <- predict.int(mod= mods,
                                  dd=dd.abund.beta.dist,
                                  y="deltaAbund",
                                  family="guassian")
pdf.f(plotDeltaDiag,
      file="figures/diagnostics/deltaAbund.pdf",
      height=9, width=3)

plotInteractionsPyroDiv()
