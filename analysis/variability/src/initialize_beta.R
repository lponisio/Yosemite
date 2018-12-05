library(lme4)
library(vegan)
library(lmerTest)
source('src/misc.R')
source('src/beta.R')

if(type == "pols"){
    speciesType <- "pollinator"
} else{
    speciesType <- "plants"
}

if(occ == "abund"){
  dis.method <- "chao"
  load(file=file.path('saved/communities',
         sprintf('%s-abund.Rdata', type)))
  load(file=file.path('saved/nulls',
         sprintf('%s-alpha.Rdata', type)))
}

if(occ == "occ"){
  occ <- "occ"
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
