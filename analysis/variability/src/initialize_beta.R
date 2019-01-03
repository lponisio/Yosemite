library(vegan)
source('src/misc.R')
source('src/beta.R')

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

if(type=="pols"){
    ylabel <- "Pollinator species turnover"
}
if(type=="ints"){
    ylabel <- "Interaction turnover"
}
if(type=="plants"){
    ylabel <- "Plant species turnover"
}
