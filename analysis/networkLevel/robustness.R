## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')

extinction.methods <- c("degree")
participants<- c("lower")

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


nets <- lapply(nets, function(y){
    y[y > 1] <- 1
    y
})

all.nets <- list(nets, filled.nets)
names(all.nets) <- c("obs", "potential")


ext.rows <- lapply(nets, function(x){
    plant.degree <- rowSums(x)
    ext.row <- order(plant.degree,
                     decreasing=TRUE)
    return(ext.row)
})

for(net.type in names(all.nets)){
    for(sp.level in participants){
        for(ex.method in extinction.methods){
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
