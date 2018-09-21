library(igraph)
library(bipartite)
library(lme4)
library(lmerTest)
library(RColorBrewer)

source('src/vaznull2.R')
source('src/CalcMetrics.R')
source('src/misc.R')
source("src/resilience.R")
source('src/diffs.R')
source('src/prepNets.R')

load('../data/specimens/spec.Rdata')
load('~/Dropbox/Yosemite/analysis/spatialData/config/saved/data/all150.Rdata')

save.path <- 'saved'

