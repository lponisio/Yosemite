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

split.pol <- split(spec, spec$GenusSpecies)
all.plant <- lapply(split.pol, function(x) unique(x$PlantGenusSpecies))

filled.nets <- lapply(nets, function(y){
    for(sp in colnames(y)){
        this.int <- y[,sp]
        this.ref <- all.plant[[sp]]
        in.ref <- names(this.int) %in% this.ref
        y[,sp][in.ref] <- 1
    }
    return(y)
})

all.nets <- list(nets, filled.nets)
names(all.nets) <- c("obs", "potential")

for(net.type in names(all.nets)){
    for(sp.level in participants){
        for(ex.method in extinction.methods){
            res <- simExtinction(all.nets[[net.type]], extinction.method=ex.method,
                                 dat.mods, participant=sp.level)
            res$SiteStatus <- factor(res$SiteStatus, levels=c("LOW", "MOD", "HIGH"))

            res.ave <- aggregate(list(Robustness = res$Robustness),
                                 list(Site=res$Site,
                                      Year=res$Year,
                                      SiteStatus=res$SiteStatus,
                                      simpson.div=res$simpson.div),
                                 mean, na.rm=TRUE)
            ## res.delta <- res.ave[res.ave$Year == "2013",]
            ## res.2014 <- res.ave[res.ave$Year == "2014",]
            ## res.delta$Robustness.2014 <- res.2014$Robustness[match(res.delta$Site,
            ##                                              res.2014$Site)]
            ## res.delta$delta <-
            ##     log(res.delta$Robustness.2014)/log(res.delta$Robustness)


            mod.div <- lmer(Robustness ~ scale(simpson.div)*SiteStatus
                            + (1|Site),
                            data=res)

            print(paste("*******", net.type, ex.method, sp.level, "*******"))
            print(summary(mod.div))

            save(mod.div, res, mod.div,
                 file=file.path(save.path,
                                sprintf('mods/robustness_%s_%s_%s.Rdata',
                                        ex.method, sp.level, net.type)))
        }
    }
}
