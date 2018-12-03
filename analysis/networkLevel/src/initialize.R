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
load('../data/pyrodiv_buffers/all150.Rdata')

save.path <- 'saved'

spec <- spec[spec$NetPan == "net",]
spec <- spec[!spec$PlantGenusSpecies == "",]

nets <- break.net(spec)
nets <- unlist(nets, recursive=FALSE)

getNetData <- function(nets){
    sites <- sapply(strsplit(nets, "[.]"),
                       function(x) x[1])
    dates <-  sapply(strsplit(nets, "[.]"),
                        function(x) x[2])
    years <- format(as.Date(dates), "%Y")
    return(data.frame(Date=dates,
                      Site=sites,
                      Year=years))
}
