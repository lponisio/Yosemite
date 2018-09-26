library(igraph)
library(bipartite)

source('src/misc.R')
source('src/cvCalc.R')
source('src/calcPca.R')
source('src/calcSpec.R')
source('../networkLevel/src/prepNets.R')

load('../data/specimens/spec.Rdata')

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
