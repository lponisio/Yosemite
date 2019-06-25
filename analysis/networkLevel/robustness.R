## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

extinction.methods <- c("visit") ## visit, degree, veg
participants<- c("lower")

## **********************************************************
## robustness
## **********************************************************
## simulate plant extinction
## Simpson div pyrodiversity

for(net.type in names(all.nets)){
    for(sp.level in participants){
        for(ex.method in extinction.methods){
            ext.rows <- getExtinctionOrder(ex.method,
                                           all.nets[[net.type]],
                                           spec,
                                           veg)
            res <- simExtinction(all.nets[[net.type]],
                                 extinction.method="external",
                                 dat.mods,
                                 participant=sp.level,
                                 ext.row=ext.rows)
            res$Year <- factor(res$Year,
                               levels=c("2014", "2013"))

            mod.div <- lmer(Robustness ~
                                scale(simpson.div)*Year
                            + (1|Site),
                            data=res)

            print(paste("*******", net.type, ex.method, sp.level, "*******"))
            print(summary(mod.div))
            save(mod.div, res,
                 file=file.path(save.path,
                                sprintf('mods/robustness_%s_%s_%s.Rdata',
                                        ex.method, sp.level, net.type)))
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
