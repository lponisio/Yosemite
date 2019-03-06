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
            res <- simExtinction(all.nets[[net.type]],
                                 extinction.method=ex.method,
                                 dat.mods, participant=sp.level)
            res$SiteStatus <- factor(res$SiteStatus,
                                     levels=c("LOW", "MOD", "HIGH"))
            res$Year <- factor(res$Year,
                                     levels=c("2014", "2013"))

            mod.div <- lmer(Robustness ~
                                scale(simpson.div)*Year
                                + (1|Site),
                            data=res)

            print(paste("*******", net.type, ex.method, sp.level,
                        "Robustness", "*******"))
            print(summary(mod.div))
            print(anova(mod.div))
            save(mod.div, res,
                 file=file.path(save.path,
                                sprintf('mods/robustness_%s_%s_%s.Rdata',
                                        ex.method, sp.level, net.type)))
        }
    }
}
