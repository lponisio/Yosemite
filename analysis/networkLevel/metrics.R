## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

## ## number of null communities
N <- 999

## ## ************************************************************
## ## calculate metrics and zscores ## beware this takes a while!
## ## ************************************************************
mets <- lapply(nets, calcNetworkMetrics,  N)

cor.dats <- prepDat(mets,  spec)
cor.dats <- merge(cor.dats, dat.mods)

save(cor.dats, file='saved/corMets.Rdata')

## ************************************************************
load(file='saved/corMets.Rdata')

cor.dats$Year <- factor(cor.dats$Year,
                              levels=c("2013", "2014"))

ys <- c("partner diversity", "zH2", "links.per.species",
        "functional.complementarity.HL",
        "functional.complementarity.LL")

## simpson's diversity of fire history
formulas.div <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(simpson.div)*Year",
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

lapply(mods.div, anova)

save(mods.div, cor.dats,
     file=file.path(save.path, 'mods/metrics.Rdata'))
