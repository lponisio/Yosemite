## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

extinction.methods <- c("degree", "visit", "veg")
participants<- c("lower")
by.abund <- c("LR abund", "abund")

## **********************************************************
## robustness
## **********************************************************
## simulate plant extinction
## Simpson div pyrodiversity

split.pol <- split(spec, spec$GenusSpecies)
all.plant <- lapply(split.pol, function(x) unique(x$PlantGenusSpecies))

filled.nets <- lapply(nets, function(y){
    y[y > 1] <- 1
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
            for(ab in by.abund){
                print(paste("*******", net.type, ex.method, sp.level, ab, "*******"))
                ext.rows <- getExtinctionOrder(ex.method,
                                               by.abund=ab,
                                               nets,
                                               spec,
                                               veg)
                res <- simExtinction(all.nets[[net.type]],
                                     extinction.method="external",
                                     dat.mods,
                                     participant=sp.level,
                                     ext.row=ext.rows)
                res$Year <- factor(res$Year,
                                   levels=c("2013", "2014"))

                mod.div <- lmer(Robustness ~
                                    scale(simpson.div)*Year
                                + (1|Site),
                                data=res)

                print(paste("*******", net.type, ex.method, sp.level, ab, "*******"))
                ## print(summary(mod.div))
                ## print(anova(mod.div))
                save(mod.div, res,
                     file=file.path(save.path,
                                    sprintf('mods/robustness_%s_%s_%s_%s.Rdata',
                                            ex.method, sp.level, net.type, ab)))
            }
        }
    }
}



diff.mats  <-  mapply(function(a, b){b-a},
                      a = all.nets[["obs"]],
                      b = all.nets[["potential"]],
                      SIMPLIFY = FALSE)

added.int <- sapply(diff.mats, sum)
mean(added.int)

sites <- sapply(strsplit(names(diff.mats), "[.]"), function(x) x[[1]])
pyrodiv <- res$simpson.div[match(sites, res$Site)]

mod <- lmer(added.int ~ pyrodiv + (1|sites))
summary(mod)

obs <- sapply(all.nets[["obs"]], mean)
hist(obs)
mean(obs)
sd(obs)


potential <- sapply(all.nets[["potential"]], mean)
hist(potential)
mean(potential)
sd(potential)
