## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

## either "abund" or "degree"
extinction.method <- "degree"

## **********************************************************
## robustness
## **********************************************************
## simulate plant extinction
## simmpson div pyrodiversity
res <- simExtinction(nets, extinction.method, dat.mods)

mod.div <- lmer(Robustness ~ s.simpson.div*Year
                + (1|Site),
                data=res)

summary(mod.div)

## functional disperson pyrodiversity
mod.dis <- lmer(Robustness ~ s.FuncDis*Year
                + (1|Site),
                data=res)

summary(mod.dis)

save(mod.dis, mod.div, res, file=file.path(save.path,
                             sprintf('mods/robistness_%s.Rdata',
                                     extinction.method)))
