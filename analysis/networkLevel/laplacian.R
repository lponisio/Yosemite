## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')
source('src/laplacian_functions.R')

metrics <- c("Ncomp", "AlgCon", "EigenRatio")

alg.con <- t(do.call(cbind.data.frame, lapply(nets, algCone)))
colnames(alg.con) <- metrics
alg.con <- cbind(alg.con, getNetData(rownames(alg.con)))
rownames(alg.con) <- NULL

alg.con <- merge(alg.con, dat.mods)

ys <- metrics

## simpson's diversity of fire history
formulas.div <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("s.simpson.div*Year",
                           "(1|Site)",
                           sep="+")))
})

mods.div <- lapply(formulas.div, function(x){
    lmer(x,
         data=alg.con)
})

names(mods.div) <- ys
## results
lapply(mods.div, summary)


## functional dispersion fo fire history
formulas.dis <-lapply(ys, function(x) {
    as.formula(paste(x, "~",
                     paste("s.FuncDis*Year",
                           "(1|Site)",
                           sep="+")))
})

mods.dis <- lapply(formulas.dis, function(x){
    lmer(x,
         data=alg.con)
})

names(mods.dis) <- ys
## results
lapply(mods.dis, summary)

## look at correlation between AlgCon and species richness
cor.test(alg.con$Richness, alg.con$AlgCon)
