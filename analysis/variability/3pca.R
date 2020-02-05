## This script calculates the pollinator role/network niche
## varaibility between sites within a year.
rm(list=ls())
setwd('analysis/variability')
this.script <- "role"
source('src/initialize.R')
type <- "all"

species.roles <- calcSpec(nets, spec, dist.metric="chao")

## vector of pca loadings of interest
loadings <- c(1)
metrics <- c("rare.degree",
             "weighted.betweenness",
             "weighted.closeness",
             "niche.overlap",
             "species.strength",
             "d")

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


autoplot(pol.pca.scores$'2013'$pca.loadings, loadings=TRUE,
         loadings.colour = 'blue')

save(pol.pca.scores,  file="saved/results/pcaVar.Rdata")
