## This script calculates the "beta diversity" of interaction partners
## for each species. For this study, the partner beta diversity is
## calculated across sites within a year
## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize_beta.R')

## metrics for takeing the average and variability of beta div
var.method <- cv

dis <- mapply(function(a, b, c, d)
    calcBeta(comm= a, ## observed communities
             dis.method, ## dissimilarity metric
             nulls=b, ## null communities
             occ=binary, ## binary or abundance weighted?
             sub=type,
             zscore=FALSE), ## use zscores vs. Chase method?
    a=comm$comm,
    b= nulls,
    SIMPLIFY=FALSE)

beta.dist <- makeBetaDataPretty()

This script calculates the "beta diversity" of interaction partners
## for each species. For this study, the partner beta diversity is
## calculated across sites within a year
## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize_beta.R')

## metrics for takeing the average and variability of beta div
var.method <- cv

dis <- mapply(function(a, b, c, d)
    calcBeta(comm= a, ## observed communities
             dis.method, ## dissimilarity metric
             nulls=b, ## null communities
             occ=binary, ## binary or abundance weighted?
             sub=type,
             zscore=FALSE), ## use zscores vs. Chase method?
    a=comm$comm,
    b= nulls,
    SIMPLIFY=FALSE)

beta.dist <- makeBetaDataPretty()

var.beta.dist <- tapply(beta.dist$dist[beta.dist$Year == "2013"],
                        beta.dist$GenusSpecies[beta.dist$Year == "2013"],
                        var.method)

save(var.beta.dist, file="saved/results/partnerVar.Rdata")
