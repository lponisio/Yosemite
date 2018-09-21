## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
source('src/commPrep.R')
save.dir.comm <- "saved/communities"
save.dir.nulls <- "saved/nulls"

source('src/misc.R')
source('src/vaznull2.R')

sites <- unique(spec$Site)
years <- unique(spec$Year)
spec$Int <- paste(spec$GenusSpecies,
                  spec$PlantGenusSpecies)

type <- "pols"
nnull <- 99

if(type=="pols"){
    species.type="GenusSpecies"
    species.type.int="PlantGenusSpecies"
}

if(type=="plants"){
    species.type="PlantGenusSpecies"
    species.type.int="GenusSpecies"
}

## ************************************************************
## year by species matrices pollinators!
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
load(file=file.path(save.dir.comm,
                    sprintf('%s-abund.Rdata', type)))
nulls <- rapply(comm$comm, vaznull.2, N=nnull, how="replace")

save(nulls, file=file.path(save.dir.nulls,
                           sprintf('%s-alpha.Rdata', type)))



## ************************************************************
## occurrence nulls
## ************************************************************
load(file=file.path(save.dir.comm,
                    sprintf('%s-abund.Rdata', type)))

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
## individuals nulls
## ************************************************************
## load(file=file.path(save.dir.comm,
##        sprintf('%s-abund.Rdata', type)))
## nulls <- rapply(comm$comm, swap.web, N=nnull, how="replace")
## save(nulls, file=file.path(save.dir.nulls,
##               sprintf('%s-indiv.Rdata', type)))
