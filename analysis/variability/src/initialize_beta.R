library(vegan)
source('src/misc.R')
source('src/beta.R')

args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 0){
    type <- args[1]
    occ <- args[2]
} else{
    type <- "pol"
    occ <- "abund"
}

if(type == "pol"){
    speciesType <- "pollinator"
} else{
    speciesType <- "plants"
}


if(occ == "abund"){
    binary <- FALSE
    dis.method <- "chao"
    load(file=file.path('saved/communities',
                        sprintf('%s-abund.Rdata', type)))
    load(file=file.path('saved/nulls',
                        sprintf('%s-alpha.Rdata', type)))
}

if(occ == "occ"){
    occ <- "occ"
    binary <- TRUE
    dis.method <- "jaccard"
    load(file=file.path('saved/communities',
                        sprintf('%s-abund.Rdata', type)))
    load(file=file.path('saved/nulls',
                        sprintf('%s-occ.Rdata', type)))
}

if(type=="pol"){
    ylabel <- "Pollinator species turnover"
}
