library(lme4)
library(vegan)
library(lmerTest)
library(bipartite)
source('src/misc.R')
source('src/nestednessCalc.R')

if(!binary & alpha){
  occ <- "abund"
  weighted <- TRUE
  load(file=file.path(src.dir, 'communities/bee-abund.R'))
  load(file=file.path(src.dir, 'nulls/bee-abund.Rdata'))
  load(file=file.path(src.dir, 'communities/flower-abund.R'))
  load(file=file.path(src.dir, 'nulls/flower-abund.Rdata'))
}

if(!binary & !alpha){
  occ <- "indiv"
  weighted <- TRUE
  load(file=file.path(src.dir, 'communities/bee-abund.R'))
  load(file=file.path(src.dir, 'nulls/bee-indiv.Rdata'))
  load(file=file.path(src.dir, 'communities/flower-abund.R'))
  load(file=file.path(src.dir, 'nulls/flower-indiv.Rdata'))
}

if(binary){
  occ <- "occ"
  weighted <- FALSE
  load(file=file.path(src.dir, 'communities/bee-occ.R'))
  load(file=file.path(src.dir, 'nulls/bee-occ.Rdata'))
  load(file=file.path(src.dir, 'communities/flower-occ.R'))
  load(file=file.path(src.dir, 'nulls/flower-occ.Rdata'))
}
