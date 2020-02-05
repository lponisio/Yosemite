## This script calculates the log abundance of each species at each
## site and then regresses that value against their interaction
## variability values.

rm(list=ls())
setwd('analysis/variability')
source('src/initialize-delta.R')

## Calculate the abundance of each species in each year
spec.abund <- calcSpecABund(spec)
## add on the varaibility/mean metrics for partner and network role
## calculated from scripes 1-3
spec.abund <- getScorePrepDrought(pol.pca.scores,
                                  var.beta.dist,
                                  spec.abund)

## calculate the difference the log ratio of abundance between year
## pairs
delta <- calcYearDiff(spec.abund)
delta <- prepDeltaVar(delta=delta$'2013',
                      spec.abund=spec.abund,
                      site.char=dat.mods,
                      veg=veg)

## remove any rows with NAs
xvars <- c("beta.dist", "var.pca1")
delta$delta <- delta$delta[!apply(delta$delta[, xvars], 1, function(x)
    any(is.na(x))),]

## drop extreme role var value. Does not affect results but residuals
## do not look nice
delta$delta <- delta$delta[delta$delta$var.pca1 !=
                           min(delta$delta$var.pca1, na.rm=TRUE),]

## ************************************************************
## delta abund
## ************************************************************
print("******** delta log ratio abund **********")
mods <- lmer(deltaAbund ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) +
                 scale(deltaFloralAbund) +
                  scale(mean.pca1) +
                 (1|GenusSpecies),
             data=delta$delta[delta$delta$deltaAbund != 0,])
vif.mer(mods)
summary(mods)

save(mods,delta, spec.abund, file=file.path(save.path, 'mods/drought.Rdata'))
