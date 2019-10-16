library(bipartite, quietly = TRUE)
library(lme4, quietly = TRUE)
library(lmerTest, quietly = TRUE)
source('src/misc.R')
source('src/calcPca.R')
source('src/calcSpec.R')

load('../data/specimens/spec.Rdata')
load('../data/specimens/nets.Rdata')
save.path <- 'saved'

save.dir.comm <- "saved/communities"
save.dir.nulls <- "saved/nulls"

if(this.script == "nulls"){
    args <- commandArgs(trailingOnly=TRUE)
    if(length(args) != 0){
        type <- args[1]
        nnull <- args[2]
    } else{
        type <- "pol"
        nnull <- 999
    }
    species.type="GenusSpecies"
    species.type.int="PlantGenusSpecies"
}
