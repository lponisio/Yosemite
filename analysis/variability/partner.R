## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')

args <- commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
    type <- args[1]
    occ <- args[2]
} else{
    type <- "pol"
    occ <- "abund"
}

source('src/initialize_beta.R')

## ************************************************************
## beta diversity as variation between years,
## centroid for each site
## ************************************************************

dis <- mapply(function(a, b, c, d)
    calcBeta(comm= a, ## observed communities
             dis.method, ## dissimilarity metric
             nulls=b, ## null communities
             occ=binary, ## binary or abundance weighted?
             sub=type,
             zscore=FALSE), ## use Chase method not zscores
    a=comm$comm,
    b= nulls,
    SIMPLIFY=FALSE)

beta.dist <- makeBetaDataPretty()

save(beta.dist, file="saved/results/partnerVar.Rdata")
