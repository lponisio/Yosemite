## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize-delta.R')

spec.abund <- calcSpecABund(spec)
spec.abund <- getScorePrepDrought(pol.pca.scores,
                                  beta.dist,
                                  spec.abund)


delta <- calcYearDiff(spec.abund)
delta <- prepDeltaVar(delta=delta,
                      spec.abund=spec.abund,
                      site.char=dat.mods,
                      veg=veg)

xvars <- c("beta.dist", "var.pca1")
delta$delta <- delta$delta[!apply(delta$delta[, xvars], 1, function(x)
    any(is.na(x))),]

delta$delta <- delta$delta[delta$delta$var.pca1 != min(delta$delta$var.pca1),]

## ************************************************************
## delta abund
## ************************************************************

mods <- lmer(deltaAbund ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) +
                 + SiteStatus*scale(simpson.div)  + scale(deltaFloralAbund) +
                 (1|GenusSpecies), na.action = "na.fail",
             data=delta$delta[delta$delta$deltaAbund != 0,])
vif.mer(mods)
summary(mods)

print("******** delta abund **********")
## removed pyrodiversity*Site status bec VIF high, does not change sig of the
## results


mods <- lmer(deltaAbund ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) +
                 + SiteStatus  + scale(deltaFloralAbund) +
                 (1|GenusSpecies), na.action = "na.fail",
             data=delta$delta[delta$delta$deltaAbund != 0,])
vif.mer(mods)
summary(mods)

## ************************************************************
## persist or go extinct?
## ************************************************************

delta$delta$Persist <- delta$delta$deltaAbund
delta$delta$Persist[delta$delta$Persist != 0] <- 1


mods.ext <- glmer(Persist ~ scale(beta.dist)*scale(simpson.div) +
                      scale(var.pca1)*scale(simpson.div) + scale(Abund) +
                  + SiteStatus*scale(simpson.div)  + scale(deltaFloralAbund) +
                  (1|GenusSpecies), na.action = "na.fail",
                  data=delta$delta, family="binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=1e9),  tolPwrss=1e-5))
vif.mer(mods.ext)
summary(mods.ext)

print("******** prob persist  **********")
## removed pyrodiversity * site status bec VIF high, also site random effect due to
## singularity of convergence. Neither change the significance of the
## results
mods.ext <- glmer(Persist ~ scale(beta.dist)*scale(simpson.div) +
                      scale(var.pca1)*scale(simpson.div) + scale(Abund) +
                  + SiteStatus  + scale(deltaFloralAbund) +
                  (1|GenusSpecies), na.action = "na.fail",
                  data=delta$delta, family="binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=1e9),  tolPwrss=1e-5))
vif.mer(mods.ext)
summary(mods.ext)

save(delta, mods.ext, mods, file="saved/mods/drought.Rdata")

