rm(list=ls())
setwd('analysis/variability')
load('../data/HR_spec.Rdata')
source("src/misc.R")
source("src/delta.R")
source("src/plotHRComparisons.R")
load('saved/mods/drought.Rdata')

## script for calculating the log ratio of abundance for hedgerow
## data set (another pollinator dataset from CA) for comparison with
## the Yosemite data

spec.abund <- aggregate(list(Abund=spec$GenusSpecies),
                        list(GenusSpecies=spec$GenusSpecies,
                             Site=spec$Site,
                             Year=spec$Year,
                             SiteStatus=spec$SiteStatus),
                        length)

## drop assembling hedgerows because they are actively recruiting
## species/increasing in abundance (in theory)
BACI <- c("Barger", "Butler", "MullerB", "Sperandio", "Hrdy")
spec.abund <- spec.abund[!spec.abund$Site %in% BACI,]
delta.HR <- calcYearDiff(spec.abund)

pdf.f(makePointLine, file="figures/HR_Yose.pdf",
      height=4, width=5)

##  statistical analysis in response to reviewer comment: " If there
##   was statistical support that population changes from 2013 to 2014
##   were more extreme than in other years the argument by the author
##   could be strengthened. "

delta.HR <- do.call(rbind, delta.HR)

delta.HR <- delta.HR[is.finite(delta.HR[,"deltaAbund"]),]
delta.HR <- delta.HR[delta.HR[, "deltaAbund"] != 0,]

mod <- lmer(deltaAbund~Years + (1|Site) + (1|GenusSpecies),
            data=delta.HR)
