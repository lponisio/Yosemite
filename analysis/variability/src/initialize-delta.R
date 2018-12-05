library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)
source('src/setup.R')
source('src/delta.R')
source('src/misc.R')
load('saved/results/partnerVar.Rdata')
load('saved/results/pcaVar.Rdata')
load('../data/functionalTraits/bees.Rdata')
load('../data/pyrodiv_buffers/all150.Rdata')
load('../data/veg/veg_updated.Rdata')
load('../data/specimens/spec.Rdata')
save.path <- 'saved'
spec <- spec[spec$NetPan == "net",]
spec <- spec[!spec$PlantGenusSpecies == "",]
