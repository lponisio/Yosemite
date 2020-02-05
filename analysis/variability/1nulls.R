## Thisc script generates randomized "communities" for standardizied
## partner beta diversity scores. Because species will vry in their
## partner richness, this standardized for partner "alpha diversity"
## for the beta diversity calculation
rm(list=ls())
setwd('analysis/variability')
this.script <- "nulls"
source('src/initialize.R')
source('src/commPrep.R')
source('src/vaznull2.R')

sites <- unique(spec$Site)
years <- unique(spec$Year)

## ************************************************************
## year by species matrices pollinators
## ************************************************************
comms <- lapply(years, calcSiteBeta,
                species.type=species.type,
                spec=spec,
                species.type.int=species.type.int)

comm <- makePretty(comms, spec)

save(comm, file=file.path(save.dir.comm,
                          sprintf('%s-abund.Rdata', type)))

## ************************************************************
## alpha div nulls
## ************************************************************
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-alpha.Rdata', type)))

## ************************************************************
## occurrence nulls
## ************************************************************
occ.null <- function(web){
    simulate(vegan::nullmodel(web, method="quasiswap"),1)[,,1]
}

rep.occ.null <- function(web, N){
    replicate(N, occ.null(web), simplify = FALSE)
}

nulls <- rapply(comm$comm, rep.occ.null, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-occ.Rdata', type)))

## ************************************************************
## test of variability metrics
## ************************************************************
## set interactions to be all the same across sampling rounds for each
## species
test.comm <- lapply(comm$comm$'2013', function(x){
    x[1:nrow(x), 1:ncol(x)] <- 0
    x[,c(1,2,3)] <- 2
    x
})
comm$comm$'2013' <- test.comm
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-alpha-test.Rdata', type)))
save(comm, file=file.path(save.dir.comm,
                          sprintf('%s-abund-test.Rdata', type)))
