## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('plotting/src/initialize.R')

## ************************************************************
## delta abund
## ************************************************************
load('~/Dropbox/Yosemite/analysis/variability/saved/mods/drought.Rdata')
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
                                SiteStatus= c("LOW", "MOD", "HIGH"),
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
                                  SiteStatus= c("LOW", "MOD", "HIGH"),
                                  deltaAbund=0)
abund.beta.dist.pi <- predict.int(mod= mods,
                                  dd=dd.abund.beta.dist,
                                  y="deltaAbund",
                                  family="guassian")
pdf.f(plotDeltaDiag,
      file="figures/diagnostics/deltaAbund.pdf",
      height=9, width=3)

## ************************************************************
## persist or extinct
## ************************************************************
## role
dd.persist.var.pca <- expand.grid(var.pca1=seq(
                                      from= min(delta$var.pca1),
                                      to=max(delta$var.pca1),
                                      length=10),
                                  simpson.div = mean(delta$simpson.div),
                                  beta.dist=mean(delta$beta.dist),
                                  Abund=mean(delta$Abund),
                                  deltaFloralAbund=
                                      mean(delta$deltaFloralAbund),
                                  SiteStatus= c("LOW", "MOD", "HIGH"),
                                  Persist=0)
persist.var.pca.pi <- predict.int(mod= mods.ext,
                                  dd=dd.persist.var.pca,
                                  y="Persist",
                                  family="binomial")

## partner
dd.persist.beta.dist <- expand.grid(beta.dist=seq(
                                        from= min(delta$beta.dist),
                                        to=max(delta$beta.dist),
                                        length=10),
                                    Abund=mean(delta$Abund),
                                    simpson.div = mean(delta$simpson.div),
                                    var.pca1=mean(delta$var.pca1),
                                    deltaFloralAbund=
                                        mean(delta$deltaFloralAbund),
                                    SiteStatus= c("LOW", "MOD", "HIGH"),
                                    Persist=0)
persist.beta.dist.pi <- predict.int(mod= mods.ext,
                                    dd=dd.persist.beta.dist,
                                    y="Persist",
                                    family="binomial")

pdf.f(plotPersistDiag, file="figures/diagnostics/persist.pdf",
      height=9, width=3)

plot.panels()
plotInteractionsPyroDiv()
