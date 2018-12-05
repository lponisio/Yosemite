## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

extinction.methods <- c("abund", "degree")
participants<- c("lower")

## **********************************************************
## robustness
## **********************************************************
## simulate plant extinction
## Simpson div pyrodiversity

for(sp.level in participants){
    for(ex.method in extinction.methods){
        res <- simExtinction(nets, extinction.method=ex.method,
                             dat.mods, participant=sp.level)
        res$SiteStatus <- factor(res$SiteStatus, levels=c("LOW", "MOD", "HIGH"))

        res.ave <- aggregate(list(Robustness = res$Robustness),
                             list(Site=res$Site,
                                  Year=res$Year,
                                  SiteStatus=res$SiteStatus,
                                  simpson.div=res$simpson.div),
                             mean, na.rm=TRUE)
        res.delta <- res.ave[res.ave$Year == "2013",]
        res.2014 <- res.ave[res.ave$Year == "2014",]
        res.delta$Robustness.2014 <- res.2014$Robustness[match(res.delta$Site,
                                                     res.2014$Site)]
        res.delta$delta <-
            log(res.delta$Robustness.2014)/log(res.delta$Robustness)

        mod.diff <- lm(delta ~ scale(simpson.div)*SiteStatus,
                       data=res.delta)

        mod.div <- lmer(Robustness ~ scale(simpson.div)*SiteStatus
                        + (1|Site),
                        data=res)

        print(paste("*******", ex.method, sp.level, "*******"))
        print(summary(mod.div))

        print(paste("*******", "Delta robustness", "*******"))
        print(summary(mod.diff))

        save(mod.div, res, mod.diff, res.delta,
             file=file.path(save.path,
                            sprintf('mods/robustness_%s_%s.Rdata',
                                    ex.method, sp.level)))
    }
}


