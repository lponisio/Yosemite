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
                     levels=c("2013", "2014"))

ys <- c("pol.FunRedundancy",
        "plant.FunRedundancy",
        "functional.complementarity.HL",
        "functional.complementarity.LL",
        "mean.number.of.links.HL",
        "mean.number.of.links.LL")

## simpson's diversity of fire history
formulas.div <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(simpson.div)*Year",
                           "(1|Site)",
                           sep="+")))
})
## floral richness
formulas.floral.rich <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(FloralRichness)",
                           "(1|Site)",
                           sep="+")))
})
## pollinator richness
formulas.pol.rich <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("scale(Richness)",
                           "(1|Site)",
                           sep="+")))
})
mods.div <- lapply(formulas.div, function(x){
    lmer(x, data=cor.dats)
})
mods.floral.rich <- lapply(formulas.floral.rich, function(x){
    lmer(x, data=cor.dats)
})
mods.pol.rich <- lapply(formulas.pol.rich, function(x){
    lmer(x, data=cor.dats)
})


names(mods.div) <- names(mods.floral.rich) <-
    names(mods.pol.rich) <- ys


## pollinator redund/compl/generalziation should be regressed against
## the floral community, similarly plant redund/comp/generalization
## should be regressed agains the pollinator

mods.rich <- c(mods.pol.rich["pol.FunRedundancy"],
               mods.floral.rich["plant.FunRedundancy"],
               mods.pol.rich["functional.complementarity.HL"],
               mods.floral.rich["functional.complementarity.LL"],
               mods.pol.rich["mean.number.of.links.LL"],
               mods.floral.rich["mean.number.of.links.HL"])

## results
lapply(mods.div, summary)
lapply(mods.rich, summary)

## check sig levels with method other than wald CI
lapply(mods.div, anova)
lapply(mods.rich, anova)

aics <- cbind(sapply(mods.div, AIC), sapply(mods.rich, AIC), NA)
aics[,3]  <- aics[,2]- aics[,1]
colnames(aics) <- c("pyrodiv", "richness", "deltaAIC")
aics


save(mods.div, mods.rich, cor.dats,
     file=file.path(save.path, 'mods/metrics.Rdata'))
