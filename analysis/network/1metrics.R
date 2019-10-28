## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/network')
source('src/initialize.R')

args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 0){
    N <- as.numeric(args[1])
} else{
    N <- 2
}

## including null models is relic of previously calculating metrics
## where I wanted to control for species richness

## ## ************************************************************
## ## calculate metrics and zscores ## beware this takes a while!
## ## ************************************************************
mets <- lapply(all.nets[["obs"]], calcNetworkMetrics,
               N=N)

cor.dats <- prepDat(mets,  spec)
cor.dats <- merge(cor.dats, dat.mods)
save(cor.dats, file='saved/corMets.Rdata')

## ************************************************************
load(file='saved/corMets.Rdata')

cor.dats$Year <- factor(cor.dats$Year,
                     levels=c("2014", "2013"))

ys <- c("pol.FunRedundancy",
        "plant.FunRedundancy",
        "functional.complementarity.HL",
        "functional.complementarity.LL",
        "mean.number.of.links.HL",
        "mean.number.of.links.LL")

## simpson's diversity of fire history
formulas.div <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(Richness)*Year",
                           "(1|Site)",
                           sep="+")))
})

mods.div <- lapply(formulas.div, function(x){
    lmer(x, data=cor.dats)
})


names(mods.div) <- ys
## results
lapply(mods.div, summary)

## check sig levels with method other than wald CI
lapply(mods.div, anova)

save(mods.div, cor.dats,
     file=file.path(save.path, 'mods/metrics.Rdata'))
