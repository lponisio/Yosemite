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


getScorePrepDrought <- function(pol.pca.scores, beta.dist, spec.abund ){
    ## match pca var and pca mean scores from 2013 (for var)
    pca.var <- pol.pca.scores$pca.var
    pca.mean <- pol.pca.scores$pca.mean
    spec.abund <- merge(spec.abund, pca.var)
    spec.abund <- merge(spec.abund, pca.mean)
    ## add partner varaibility data from 2013
    mean.beta.dist <- aggregate(list(beta.dist=beta.dist$dist),
                                list(Year= beta.dist$Year,
                                     GenusSpecies=
                                         beta.dist$GenusSpecies,
                                     Site= beta.dist$Site),
                                mean,
                                na.rm=TRUE)
    spec.abund <- merge(spec.abund, mean.beta.dist)

    return(spec.abund)
}

spec.abund <- calcSpecABund(spec)
spec.abund <- getScorePrepDrought(pol.pca.scores,
                                  beta.dist,
                                  spec.abund)

site.data <- unique(data.frame(Site=dat.mods$Site,
                               simpson.div=dat.mods$simpson.div,
                               SiteStatus=dat.mods$SiteStatus,
                               Year=dat.mods$Year,
                               FloralRichness=dat.mods$FloralRichness,
                               Richness=dat.mods$Richness))

## take the average across a year at a site for floral richness
## and pollinator richness
site.data <- aggregate(list(FloralRichness=site.data$FloralRichness,
                            Richness=site.data$Richness),
                       list(Site=site.data$Site,
                            simpson.div=site.data$simpson.div,
                            SiteStatus=site.data$SiteStatus,
                            Year=site.data$Year),
                       mean)

spec.abund  <- merge(spec.abund, site.data)
spec.abund  <- merge(spec.abund, traits)
spec.abund$SiteStatus <- factor(spec.abund$SiteStatus,
                                level=c("LOW", "MOD", "HIGH"))

yvars <- c("beta.dist", "var.pca1")
spec.abund <- spec.abund[!apply(spec.abund[, yvars], 1, function(x) any(is.na(x))),]

xvars <- c("scale(FloralRichness)", "scale(Richness)",
           "scale(simpson.div)*SiteStatus")

formulas <-lapply(yvars, function(x) {
  as.formula(paste(x, "~",
                   paste(xvars[1],
                         xvars[2],
                         xvars[3],
                         "(1|Site)",
                          "(1|GenusSpecies)",
                         sep="+")))
})

mods <- lapply(formulas, function(x){
    lmer(x,
         data=spec.abund,
          na.action = "na.fail",)
})

lapply(mods, summary)

model.select <- lapply(mods, dredge)

lapply(mods, vif.mer)


save(spec.abund, mods, file="saved/mods/drought.Rdata")

