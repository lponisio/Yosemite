setwd('~/Dropbox/Yosemite')
rm(list=ls())
library(MuMIn)
setwd('analysis/variability')
source('src/initialize.R')
source('src/setup.R')
source('src/delta.R')
load('saved/results/partnerVar.Rdata')
load('saved/results/pcaVar.Rdata')
load('../data/functionalTraits/bees.Rdata')
load('../data/pyrodiv_buffers/all150.Rdata')

spec.abund <- calcSpecABund(spec)
spec.abund <- getScorePrepDrought(pol.pca.scores,
                                  beta.dist,
                                  spec.abund)


delta <- calcYearDiff(spec.abund)
delta <- prepDeltaVar(delta=delta,
                      spec.abund=spec.abund,
                      site.char=dat.mods)

xvars <- c("beta.dist", "var.pca1", "mean.pca1")
delta$delta <- delta$delta[!apply(delta$delta[, xvars], 1, function(x) any(is.na(x))),]

mods <- lmer(deltaAbund ~ scale(beta.dist) + scale(var.pca1) +
                 + SiteStatus*scale(simpson.div) + scale(deltaFloralRichness) +
                 (1|GenusSpecies) + (1|Site), na.action = "na.fail",
             data=delta$delta)

summary(mods)

model.select <- dredge(mods)

vif.mer(mods)


save(delta, mods, file="saved/mods/drought.Rdata")



mods <- lmer(var.pca1 ~ SiteStatus*scale(simpson.div) +
                 scale(FloralRichness) + (1|Site),
             na.action = "na.fail",
             data=delta)

mods <- lmer(beta.dist ~ SiteStatus*scale(simpson.div) +
                 scale(FloralRichness) + (1|Site),
             na.action = "na.fail",
             data=delta)

mods <- lm(deltaFloralRichness ~
                 + SiteStatus*scale(simpson.div), na.action = "na.fail",
             data=delta$site.data)

summary(mods)
