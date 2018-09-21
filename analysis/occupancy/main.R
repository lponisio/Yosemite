## ************************************************************
rm(list=ls())
setwd('~/Dropbox/Yosemite/analysis/occupancy')
source('src/misc.R')
library('abind')
library('rjags')
library('R2jags')
load.module("glm")
source('src/prep.R')
source('src/model-SevClass.R')
## ************************************************************

## ************************************************************
scale <- 1e3
save.dir <- 'saved'
d <- prep(nzero=0,
          threshold=1,
          phen=TRUE,
          save.dir=save.dir)

res <- analyse.ms.ms(d,
                     cases[i,]$s2,
                     ni=(1e3+1e1)*scale,
                     nt=scale,
                     nb=1e1*scale,
                     nc=3)



load('saved/1-0-summary.RData')
cols <- c('mean', 'sd', '2.5%', '97.5%', 'Rhat', 'n.eff')
summary$bugs[,cols]

