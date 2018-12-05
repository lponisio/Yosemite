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


## ************************************************************
## delta abund
## ************************************************************

mods <- lmer(deltaAbund ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) +
                 + SiteStatus*scale(simpson.div) + scale(deltaFloralAbund) +
                 (1|GenusSpecies) + (1|Site), na.action = "na.fail",
             data=delta$delta[delta$delta$deltaAbund != 0,])

print("******** delta abund **********")
summary(mods)
vif.mer(mods)

## ************************************************************
## persist or go extinct?
## ************************************************************

delta$delta$Persist <- delta$delta$deltaAbund
delta$delta$Persist[delta$delta$Persist != 0] <- 1


mods.ext <- glmer(Persist ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) + Abund +
                 + SiteStatus*scale(simpson.div) + scale(deltaFloralAbund) +
                 (1|GenusSpecies) + (1|Site), na.action = "na.fail",
                 data=delta$delta, family="binomial",
                 control=glmerControl(optimizer="bobyqa",
                      optCtrl=list(maxfun=1e9),  tolPwrss=1e-5))

print("******** prob persist  **********")
summary(mods.ext)
vif.mer(mods.ext)

save(delta, mods.ext, mods, file="saved/mods/drought.Rdata")

