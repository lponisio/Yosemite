library(igraph, quietly = TRUE)
library(bipartite, quietly = TRUE)
library(SYNCSA, quietly = TRUE)
library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)

source('src/vaznull2.R')
source('src/CalcMetrics.R')
source('src/misc.R')
source("src/robustness.R")
source('src/diffs.R')
source('src/extOrder.R')
source('src/makeNets.R')

load('../data/specimens/spec.Rdata')
load('../data/specimens/nets.Rdata')
load('../data/pyrodiv_buffers/all150.Rdata')
load('../data/veg/veg.Rdata')

save.path <- 'saved'

filled.nets <-  makeFilledNets(spec, nets)
in.both.nets <- makeInBothNets(spec, nets)
fill.nets.in.both <- makeFilledNets(spec, in.both.nets)

all.nets <- list(nets, filled.nets, in.both.nets, fill.nets.in.both)
names(all.nets) <- c("obs", "potential", "both", "filledBoth")
