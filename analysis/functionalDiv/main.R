## ************************************************************
rm(list=ls())
setwd('~/Dropbox/Yosemite')
library(abind)
library(FD)
library(bipartite)
library(parallel)
library(lme4)
src.dir <- 'analysis/functionalDiv/src'
source(file.path(src.dir, 'misc.R'))
source(file.path(src.dir, 'site-lists.R'))
source(file.path(src.dir, 'prep.R'))
source(file.path(src.dir, 'analyses.R'))
## ************************************************************

## ************************************************************
## create and save data
sites <- 'total'
bee.syr <- 'bees'
net.combined <- 'net'
cts.traits <- c('PolSpec', "YoseSpec")
cat.traits <- c('NestLoc',
                'Excavate',
                'Sociality',
                'Wood',
                'FireKill')

## ****************************************
## to get number of samples per year
dd.model <- prep.dat(sites=sites,
                     bee.all=bee.syr,
                     drop.na=TRUE,
                     drop.parasites=FALSE,
                     cts.traits=cts.traits,
                     cat.traits=cat.traits,
                     occ.abun='abun',
                     net.combined=net.combined)

## ****************************************

res <- run.analysis(dd.model)

fn <- sprintf('%s_%s_%s.RData', bee.syr, sites, 'abun')
save(res, file=file.path('analysis/functionalDiv/saved', fn))

summary(res$FDiv$model.out)
summary(res$FEve$model.out)
summary(res$FDis$model.out)
## ************************************************************

func.div.stats <- cbind(res$FRic$dd,
                        res$FEve$dd$vals,
                        res$FDiv$dd$vals,
                        res$FDis$dd$vals)
colnames(func.div.stats) <-
  c("Site", "SiteStatus", "Year", "FRich", "FEven", "FDiv", "FDis")
save(func.div.stats,
     file=sprintf("analysis/data/siteLevel/%s/%s/funcDiv.Rdata",
       net.combined,
       bee.syr))
