setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
source('src/commPrep.R')
save.dir.comm <- "saved/communities"
save.dir.nulls <- "saved/nulls"

source('src/misc.R')
source('src/vaznull2.R')

sites <- unique(spec$Site)
years <- unique(spec$Year)
spec$Int <- paste(spec$GenusSpecies,
                  spec$PlantGenusSpecies)

makeIdentialInt <- function(x){
    x[1:nrow(x), 1:ncol(x)] <- 0
    x[,c(1,2,3)] <- 2
    return(bipartite::empty(x))
}

## *********************************************************************
## partner var test
## *********************************************************************

type <- "pol"
nnull <- 99

species.type="GenusSpecies"
species.type.int="PlantGenusSpecies"

load(file=file.path(save.dir.comm,
                    sprintf('%s-abund.Rdata', type)))

## set interactions to be all the same across sampling rounds for each
## species
test.comm <- lapply(comm$comm$'2013', makeIdentialInt)
comm$comm$'2013' <- test.comm
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

source('src/beta.R')

dis <- mapply(function(a, b, c, d)
    calcBeta(comm= a, ## observed communities
             dis.method="chao", ## dissimilarity metric
             nulls=b, ## null communities
             occ=FALSE, ## binary or abundance weighted?
             sub=type,
             zscore=FALSE), ## use Chase method not zscores
    a=comm$comm,
    b= nulls,
    SIMPLIFY=FALSE)

beta.dist <- makeBetaDataPretty()

mean.beta.dist <- tapply(beta.dist$dist[beta.dist$Year == "2013"],
                         beta.dist$GenusSpecies[beta.dist$Year == "2013"],
                         cv)

## super small numbers, as expected
hist(mean.beta.dist)


## *********************************************************************
## role var test
## *********************************************************************
## species
test.net <- lapply(nets, makeIdentialInt)

species.roles <- calcSpec(test.net, spec, dist.metric="chao")

## vector of pca loadings of interest
loadings <- c(1)
metrics <- c("rare.degree", "weighted.betweenness", "weighted.closeness",
             "niche.overlap", "species.strength", "d")

## the metrics used in the PCA
var.method <- cv
ave.method <- mean

pol <- species.roles[species.roles$speciesType == "pollinator",]

pol.pca.scores <- calcPcaMeanVar(species.roles=pol,
                                 var.method=var.method,
                                 ave.method=ave.method,
                                 metrics= metrics,
                                 loadings=loadings,
                                 agg.col = "Year")

hist(pol.pca.scores$pca.var$var.pca1)

## basically no varaibility
