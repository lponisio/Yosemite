rm(list=ls())
library(lme4)
library(lmerTest)
library(RColorBrewer)
setwd('analysis/resources')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/floralPlotPanels.R")
source("plotting/src/CIplotting.R")
load('../data/siteLevel/floral/bySite.Rdata')
load('saved/mods.Rdata')
load('saved/Spmods.Rdata')
load('saved/polSpmods.Rdata')
load('../data/veg/veg.Rdata')
load('../data/matrices/combined/bees/sp.Rdata')
veg$s.doy <- scale(veg$doy)
by.site$s.doy <- scale(by.site$doy)

## ************************************************************
## species abundance
## ************************************************************
veg$logFlowerNum <- log(veg$logFlowerNum + 1)
dd.abund.sp <- expand.grid(s.doy= seq(
                             from= min(veg$s.doy),
                             to= max(veg$s.doy),
                             length=10),
                           SiteStatus= c("LOW", "MOD", "HIGH"),
                           Year= c("2013", "2014"),
                           logFlowerNum = 0)


abund.pi.sp <- predict.int(mod= out.mods.sp,
                           dd=dd.abund.sp,
                           y="logFlowerNum",
                           family="gaussian")

plot.predict.div(new.dd=abund.pi.sp,
                 ylabel="Flowering Species Abundance (log)",
                 dats=veg,
                 y1="logFlowerNum",
                 xs="s.doy",
                 legend.loc="topleft",
                 legend.loc.year="topright")

## ************************************************************
## Richness
## ************************************************************
## dd.richness <- expand.grid(s.doy=seq(
##                              from= min(by.site$s.doy),
##                              to= max(by.site$s.doy),
##                              length=10),
##                            SiteStatus= c("LOW", "MOD", "HIGH"),
##                            Year= c("2013", "2014"),
##                            Richness = 0)

## richness.pi <- predict.int(mod= out.mods[["Richness"]],
##                            dd=dd.richness,
##                            y="Richness",
##                            family="poisson")

## plot.predict.div(new.dd=richness.pi,
##                  ylabel="Richness",
##                  dats=by.site,
##                  y1="Richness",
##                  xs="s.doy",
##                  legend.loc="topleft",
##                  legend.loc.year="topright")


## ************************************************************
## pol species abundance
## ************************************************************

dd.abund.pol.sp <- expand.grid(YoseSpec= seq(
                             from= min(sp$YoseSpec, na.rm=TRUE),
                             to= max(sp$YoseSpec, na.rm=TRUE),
                             length=10),
                           s.doy = 0,
                           SiteStatus= c("LOW", "MOD", "HIGH"),
                           Year= c("2013", "2014"),
                           Abund = 0)

abund.pi.pol.sp <- predict.int(mod= out.mods.spec[[1]],
                           dd=dd.abund.pol.sp,
                           y="Abund",
                           family="poisson")

plot.predict.div(new.dd=abund.pi.pol.sp,
                 ylabel="Bee Abundance",
                 dats=sp,
                 y1="Abund",
                 xs="YoseSpec",
                 xlabel= "Specialization",
                 legend.loc="topleft",
                 legend.loc.year="topright")

## ************************************************************
## Div
## ************************************************************
## dd.div <- expand.grid(s.doy=seq(
##                         from= min(by.site$s.doy),
##                         to= max(by.site$s.doy),
##                         length=10),
##                       SiteStatus= c("LOW", "MOD", "HIGH"),
##                       Year= c("2013", "2014"),
##                       Div = 0)

## div.pi <- predict.int(mod= out.mods[["Div"]],
##                       dd=dd.div,
##                       y="Div",
##                       family="gaussian")

## plot.predict.div(new.dd=div.pi,
##                  ylabel="Diversity",
##                  dats=by.site,
##                  y1="Div",
##                  xs="s.doy",
##                  legend.loc="topright")


## by year
box.sp(by.site, veg)
source(
'~/Dropbox/yosemite/analysis/spatialData/config/plotting/src/CIplotting.R')
veg$maxFlower <-6
floral.plot.panels()



## ************************************************************
## abundance not log
## ************************************************************
## dd.abund <- expand.grid(s.doy=seq(
##                           from= min(by.site$s.doy),
##                           to= max(by.site$s.doy),
##                           length=10),
##                         SiteStatus= c("LOW", "MOD", "HIGH"),
##                         Year= c("2013", "2014"),
##                         Abund = 0)

## abund.pi <- predict.int(mod= out.mods[["Abund"]],
##                         dd=dd.abund,
##                         y="Abund",
##                         family="poisson")

## plot.predict.div(new.dd=abund.pi,
##                  ylabel="Abundance",
##                  dats=by.site,
##                  y1="Abund",
##                  xs="s.doy",
##                  legend.loc="topleft",
##                  legend.loc.year="topright")

## ************************************************************
## abundance logged
## ************************************************************
## by.site$Abund <- log(by.site$Abund)
## by.site$Abund[!is.finite(by.site$Abund)] <- 0

## dd.abund <- expand.grid(s.doy=seq(
##                           from= min(by.site$s.doy),
##                           to= max(by.site$s.doy),
##                           length=10),
##                         SiteStatus= c("LOW", "MOD", "HIGH"),
##                         Year= c("2013", "2014"),
##                         Abund = 0)

## abund.pi <- predict.int(mod= out.mods[["Abund"]],
##                         dd=dd.abund,
##                         y="Abund",
##                         family="gaussian")

## plot.predict.div(new.dd=abund.pi,
##                  ylabel="Abundance (log)",
##                  dats=by.site,
##                  y1="Abund",
##                  xs="s.doy",
##                  legend.loc="bottomleft",
##                  legend.loc.year="bottomright")
