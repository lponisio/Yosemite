## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')

binary <- FALSE
alpha <- TRUE
## ints or pols
type <- "pols"

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

dats <- makeBetaDataPretty()

average.partner.var <- tapply(dats[dates$year=="2013",], ]
