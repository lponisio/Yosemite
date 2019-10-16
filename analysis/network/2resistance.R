## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/network')
source('src/initialize.R')

## what order to use for the extinction of plant species? Visit drop
## species based on their abundance in the visitation data (lowest to
## highest, this result is presented in the manuscript). Degree drops
## species based on their degree (as per the original Memmot study and
## the bipartite package). Veg drop species by their abundance in the
## veg data which is based on flower counts. Dropping by abundance in
## the visitation data seemed most reflective of a drought scenario.
extinction.methods <- c("visit") ## visit, degree, veg

## frop plants ("lower") or pollinators ("higher"). I focus on plants
## because while pollinaotrs try on plants for resrouces thus thus
## co-extinctions would not be unheard of, the other direction
## (pollinators leading to the co-extinction of plants) seems likely.
participants <- c("lower")

## the tree network types are the observed network (obs) the potential
## network where are possible interactions are filled in as ones
## (potential), and there same two where only the species present in
## both years are included (both and filled both respectivly). I
## created the last version in an attempt to understand how the
## extinction simulation deviated from the actual drought event.

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
                               levels=c("2013", "2014"))

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


## **********************************************************
## exploration of the differences between the obs and potential
## networks

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
