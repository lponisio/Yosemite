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

res <- simExtinction(nets, extinction.method, dat.mods)

mod.div <- lmer(Robustness ~ s.simpson.div*Year
                + (1|Site),
                data=res)

summary(mod.div)

mod.dis <- lmer(Robustness ~ s.FuncDis*Year
                + (1|Site),
                data=res)

summary(mod.dis)

## @knitr external_resil_end


save(mod.dis, mod.div, res, file=file.path(save.path,
                             sprintf('mods/robistness_%s.Rdata',
                                     extinction.method)))
