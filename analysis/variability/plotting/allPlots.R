## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('plotting/src/predictIntervals.R')
source('plotting/src/CIplotting.R')
source('plotting/src/plotPanels.R')
library(RColorBrewer)

source('src/misc.R')

#loadings the data
load(file='saved/speciesTurnover/chao_TRUE_abund_pols.Rdata')
load(file="~/Dropbox/speciesRoles_saved/pcaScores.Rdata")
load(file="~/Dropbox/speciesRoles_saved/contrNodf.Rdata")

# dd <- expand.grid(ypr=seq(from= min(dats.type$ypr, na.rm=TRUE),
#                           to= max(cor.dats$ypr, na.rm=TRUE),
#                           length=10))
# 
## ************************************************************
## contribution to nodf
## ************************************************************

ys <- c("contr.nodf")

## phenology
nodf.days.dd <- expand.grid(median.days=seq(from= min(traits.contr.nodf$median.days,
                                                      na.rm=TRUE),
                                            to= max(traits.contr.nodf$median.days,
                                                    na.rm=TRUE),
                                            length=10),
                            median.abund=mean(traits.contr.nodf$median.abund,
                                              na.rm=TRUE),
                            SiteStatus="all",
                            contr.nodf = 0)

nodf.days.pi <- predict.int(mod= abund.pol.mod.contr.nodf,
                            dd=nodf.days.dd,
                            y=ys,
                            family="gaussian")


## abundace
nodf.abund.dd <- expand.grid(median.abund=seq(from= min(traits.contr.nodf$median.abund,
                                                        na.rm=TRUE),
                                              to= max(traits.contr.nodf$median.abund,
                                                      na.rm=TRUE),
                                              length=10),
                             median.days=mean(traits.contr.nodf$median.days,
                                              na.rm=TRUE),
                             SiteStatus="all",
                             contr.nodf = 0)

nodf.abund.pi <- predict.int(mod= abund.pol.mod.contr.nodf,
                             dd=nodf.abund.dd,
                             y=ys,
                             family="gaussian")

## degree
nodf.rdegree.dd <- expand.grid(r.degree=seq(from= min(traits.contr.nodf$r.degree,
                                                      na.rm=TRUE),
                                            to= max(traits.contr.nodf$r.degree,
                                                    na.rm=TRUE),
                                            length=10),
                               median.abund=mean(traits.contr.nodf$median.abund,
                                                 na.rm=TRUE),
                               SiteStatus="all",
                               contr.nodf = 0)

nodf.rdegree.pi <- predict.int(mod= degree.pol.mod.contr.nodf,
                               dd=nodf.rdegree.dd,
                               y=ys,
                               family="gaussian")

## ************************************************************
## role variability
## ************************************************************
ys <- c("var.pca1")
## plot for phenology
role.days.dd <- expand.grid(median.days=seq(from= min(pol.pca.scores$pca.var$median.days,
                                                     na.rm=TRUE),
                                           to= max(pol.pca.scores$pca.var$median.days,
                                                   na.rm=TRUE),
                                           length=10),
                            median.abund=mean(pol.pca.scores$pca.var$median.abund,
                                        na.rm=TRUE),
                          SiteStatus="all",
                          var.pca1 = 0)


role.days.pi <- predict.int(mod= abund.pol.mod.pca,
                               dd=role.days.dd,
                               y=ys,
                               family="gaussian")

## plot for degree
role.rdegree.dd <- expand.grid(r.degree=seq(from= min(pol.pca.scores$pca.var$r.degree,
                                                  na.rm=TRUE),
                                        to= max(pol.pca.scores$pca.var$r.degree,
                                                na.rm=TRUE),
                                        length=10),
                               median.abund=mean(pol.pca.scores$pca.var$median.abund,
                                            na.rm=TRUE),
                           SiteStatus="all",
                           var.pca1 = 0)

role.rdegree.pi <- predict.int(mod= degree.pol.mod.pca,
                     dd=role.rdegree.dd,
                     y=ys,
                     family="gaussian")

## plot for abundance
role.abund.dd <- expand.grid(median.abund=seq(from= min(pol.pca.scores$pca.var$median.abund,
                                                   na.rm=TRUE),
                                         to= max(pol.pca.scores$pca.var$median.abund,
                                                 na.rm=TRUE),
                                         length=10),
                             median.days=mean(pol.pca.scores$pca.var$median.days,
                                         na.rm=TRUE),
                        SiteStatus="all",
                        var.pca1 = 0)


role.abund.pi <- predict.int(mod= abund.pol.mod.pca,
                          dd=role.abund.dd,
                          y=ys,
                          family="gaussian")


## ************************************************************
## partner variability
## ************************************************************
ys <- c("dist")
## phenology

## plot for median days
partner.days.dd <- expand.grid(median.days=seq(from= min(dats.type$median.days,
                                                         na.rm=TRUE),
                                               to= max(dats.type$median.days,
                                                       na.rm=TRUE),
                                               length=10),
                               median.abund=mean(dats.type$median.abund,
                                                 na.rm=TRUE),
                               SiteStatus="all",
                               dist = 0)

partner.days.pi <- predict.int(mod= abund.type.mod.beta,
                               dd=partner.days.dd,
                               y=ys,
                               family="gaussian")

## plot for median degree
partner.rdegree.dd <- expand.grid(r.degree=seq(from= min(dats.type$r.degree,
                                                         na.rm=TRUE),
                                               to= max(dats.type$r.degree,
                                                       na.rm=TRUE),
                                               length=10),
                                  median.days=mean(dats.type$median.days,
                                                   na.rm=TRUE),
                                  SiteStatus = "all",
                                  dist = 0)

partner.rdegree.pi <- predict.int(mod= degree.type.mod.beta,
                                  dd=partner.rdegree.dd,
                                  y=ys,
                                  family="gaussian")

## plot for abundance
partner.abund.dd <- expand.grid(median.abund=seq(from= min(dats.type$median.abund,
                                                           na.rm=TRUE),
                                                 to= max(dats.type$median.abund,
                                                         na.rm=TRUE),
                                                 length=10),
                                median.days=mean(dats.type$median.days,
                                                 na.rm=TRUE),
                                SiteStatus="all",
                                dist = 0)


partner.abund.pi <- predict.int(mod= abund.type.mod.beta,
                                dd=partner.abund.dd,
                                y=ys,
                                family="gaussian")


## ************************************************************
## all panels
## ************************************************************
plot.panels()
