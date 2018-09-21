rm(list=ls())
library(vegan)
setwd('~/Dropbox/Yosemite/analysis/data')
source('src/misc.R')
load('specimens/spec.Rdata')
load('veg/vegSp.Rdata')
cond <-
  read.csv("../../data/relational/relational/tables/conditions.csv")
cond$Date <- as.Date(cond$Date, format='%m/%d/%y')
extra.round <- cond$Site == 'L21' & cond$Date == '2014-07-01'
cond <- cond[!extra.round,]


cond$doy <- as.numeric(strftime(cond$Date, format="%j"))
samp.sr <- data.frame(doy=cond$doy,
                      Site=cond$Site,
                      Year=cond$Year,
                      Richness=0,
                      Abund=0,
                      Div=0)
samp.sr <- unique(samp.sr, MARGIN=1)

## abundance, diversity, and richness at each site
prep.site <- function(dats,
                      save.path,
                      samps,
                      type="GenusSpecies",
                      load.func=FALSE,
                      FUN=calc.site.level,
                      abund.type="mean"){
  by.site <- aggregate(dats[,type],
                       list(Site=dats$Site,
                            Year=dats$Year,
                            doy=dats$doy),
                       function(x) FUN(x, abund.type=abund.type))
  by.site$Richness <- by.site$x[,1]
  by.site$Abund <- by.site$x[,2]
  by.site$Div <- by.site$x[,3]
  by.site <- by.site[, -4]
  samps$key <- paste(samps$Site, samps$doy, samps$Year)
  by.site$key <-  paste(by.site$Site, by.site$doy, by.site$Year)
  key.match <- match(samps$key, by.site$key)
  mets <- c("Richness", "Abund", "Div")
  samps[,mets] <- by.site[key.match, mets]
  samps$SiteStatus <- dats$SiteStatus[match(samps$Site,
                                            dats$Site)]
  if(load.func){
    load(file.path(save.path, 'funcDiv.Rdata'))
    samps <- cbind(samps,
                   func.div.stats[match(paste(samps$Site,
                                              samps$Year),
                                        paste(func.div.stats$Site,
                                              func.div.stats$Year)),
                                  4:7])
  }
  by.site <- samps
  by.site[is.na(by.site)] <- 0
  ## by.site$doy <- scale(by.site$doy)
  by.site$Year <- as.factor(by.site$Year)
  save(by.site,
       file=file.path(save.path, 'bySite.Rdata'))
}

## only net and only bees
prep.site(spec[spec$NetPan == 'net' &
               spec$GeneralID == 'Bee',],
          save.path='siteLevel/net/bees',
          load.func=TRUE,
          samps=samp.sr)

## only net and all
prep.site(spec[spec$NetPan == 'net',],
          save.path='siteLevel/net/all',
          samps=samp.sr)


## only all and all
prep.site(spec,
          save.path='siteLevel/combined/all',
          samps=samp.sr)

## net and pan and only bees
prep.site(spec[spec$GeneralID == 'Bee',],
          save.path='siteLevel/combined/bees',
          load.func=TRUE,
          samps=samp.sr)

## flowers
prep.site(vegsp,
          save.path='siteLevel/floral',
          load.func=FALSE,
          samps=samp.sr,
          type="PlantGenusSpecies",
          abund.type="sum")



## interactions
prep.site(spec[spec$NetPan == 'net' &
               spec$GeneralID == 'Bee',],
          save.path='siteLevel/net/interactions',
          load.func=FALSE,
          samps=samp.sr,
          type="Int",)

