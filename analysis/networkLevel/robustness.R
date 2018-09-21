## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

## either "abund" or "degree"
extinction.method <- "abund"

## **********************************************************
## robustness
## **********************************************************
## simulate plant extinction
## simmpson div pyrodiversity
res <- simExtinction(nets, extinction.method, dat.mods)

mod.div <- lmer(Robustness ~ s.simpson.div*Year*SiteStatus
                + (1|Site),
                data=res)

summary(mod.div)

## functional disperson pyrodiversity
mod.dis <- lmer(Robustness ~ s.FuncDis*Year*SiteStatus
                + (1|Site),
                data=res)

summary(mod.dis)

save(mod.dis, mod.div, res, file=file.path(save.path,
                             sprintf('mods/robustness_%s.Rdata',
                                     extinction.method)))
