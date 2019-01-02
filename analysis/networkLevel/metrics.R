## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

## ## number of null communities
N <- 99

## ## ************************************************************
## ## calculate metrics and zscores ## beware this takes a while!
## ## ************************************************************
## mets <- lapply(nets, calcNetworkMetrics,  N)

## cor.dats <- prepDat(mets,  spec)
## cor.dats <- merge(cor.dats, dat.mods)

## save(cor.dats, file='saved/corMets.Rdata')

## ************************************************************
load(file='saved/corMets.Rdata')
cor.dats$SiteStatus <- factor(cor.dats$SiteStatus,
                              levels=c("LOW", "MOD", "HIGH"))

ys <- c("zNODF", "zmod.met.R", "zH2", "connectance",
        "number.of.species.HL", "zgenerality.HL",
        "mean.number.of.shared.partners.HL",
        "zniche.overlap.HL", "partner.diversity.HL")

## simpson's diversity of fire history
formulas.div <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(simpson.div)*SiteStatus + Year",
                           "(1|Site)",
                           sep="+")))
})

mods.div <- lapply(formulas.div, function(x){
    lmer(x,
         data=cor.dats)
})

names(mods.div) <- ys
## results
lapply(mods.div, summary)


## functional dispersion fo fire history
## formulas.dis <-lapply(ys, function(x) {
##     as.formula(paste(x, "~",
##                      paste("scale(FuncDis)*Year*SiteStatus",
##                            "(1|Site)",
##                            sep="+")))
## })

## mods.dis <- lapply(formulas.dis, function(x){
##     lmer(x,
##          data=cor.dats)
## })

## names(mods.dis) <- ys
## ## results
## lapply(mods.dis, summary)



save(mods.div, cor.dats,
     file=file.path(save.path, 'mods/metrics.Rdata'))
