library(lme4)
library(lmerTest)
source('src/misc.R')
src.dir <- '../beta-div/saved/'
load('../data/siteLevel/floral/bySite.Rdata')
load('../data/veg/veg.Rdata')

veg$s.doy <- scale(veg$doy)
by.site$s.doy <- scale(by.site$doy)

by.site$SiteStatus <- factor(by.site$SiteStatus,
                             levels=c("LOW", "MOD", "HIGH"))
veg$SiteStatus <- factor(veg$SiteStatus,
                         levels=c("LOW", "MOD", "HIGH"))
veg$Year <- factor(veg$Year,
                   levels=c("2013", "2014"))
veg$PlantGenusSpecies <- factor(veg$PlantGenusSpecies)
veg$Site <- factor(veg$Site)

veg$logFlowerNum <- log(veg$logFlowerNum + 1)

save(veg, file='../data/veg/veg_updated.Rdata')
