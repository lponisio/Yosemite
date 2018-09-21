library(lme4)
library(vegan)
library(lmerTest)
library(nlme)
source('src/misc.R')
source('src/beta.R')

if(type == "pols"){
    speciesType <- "pollinator"
} else{
    speciesType <- "plants"
}

if(!binary & alpha){
  occ <- "abund"
  dis.method <- "chao"
  load(file=file.path('saved/communities',
         sprintf('%s-abund.Rdata', type)))
  load(file=file.path('saved/nulls',
         sprintf('%s-alpha.Rdata', type)))
}

if(!binary & !alpha){
  occ <- "indiv"
  dis.method <- "chao"
  load(file=file.path('saved/communities',
         sprintf('%s-abund.Rdata', type)))
  load(file=file.path('saved/nulls',
         sprintf('%s-indiv.Rdata', type)))
}

if(binary){
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

if(only.baci){
  baci <- c("MullerB", "Sperandio", "Barger", "Butler", "Hrdy")
  comm$comm <- comm$comm[names(comm$comm) %in% baci]
  comm$sites <- comm$sites[comm$sites %in% baci]
  comm$years <- comm$years[names(comm$years) %in% baci]
  comm$status <- comm$status[names(comm$status) %in% baci]
  nulls <- nulls[names(nulls) %in% baci]
}

traits <- read.csv(file=file.path('../../data', "traitsPhenAb.csv"))



