## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
library(sem) 
library(lavaan)
library(semPlot)
library(ggplot2)
fig.path <- '~/Dropbox/speciesRoles_saved/figures/'

load("~/Dropbox/speciesRoles_saved/results.pathLecty.Rdata")
load("~/Dropbox/speciesRoles_saved/results.path.Rdata")

inspect(partner.path.model, 'fit.measures')
inspect(partner.path.model.lecty, 'fit.measures')

anova(partner.path.model, partner.path.model.lecty)
