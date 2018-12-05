rm(list=ls())
library(vegan)
library(fields)
library(bipartite)
library(fossil)

setwd('~/Dropbox/Yosemite/data')
source('speciesIDs/src/AssignSpecies.R')

setwd('~/Dropbox/Yosemite/data/relational/relational')
source('../src/relational_prep.R')
source('../src/relational_make.R')
source('../src/make_traditional.R')
sr.sched <- read.csv('tables/conditions.csv')
spec <- read.csv('traditional/specimens-complete.csv')
veg <- read.csv('traditional/veg-complete.csv')
ref <- read.csv('../original/YoseReference.csv')

setwd('~/Dropbox/Yosemite/analysis/data')
source('src/misc.R')
traits <-  read.csv('functionalTraits/bees.csv')

bee.fams <- c("Halictidae", "Andrenidae", "Apidae", "Megachilidae",
              "Colletidae")

spec <- spec[spec$Family %in% bee.fams,]

##******************************************************
## prep reference data
##******************************************************

ref <- ref[!grepl("sp.", ref$PlantGenusSpecies),]
ref <- ref[!is.na(ref$PlantGenusSpecies),]
ref$GenusSpecies <-  fix.white.space(paste(ref$Genus, ref$Species))
to.drop <- c("Non_bee Syrphidae",
             "Agapostemon angelicus / texanus",
             "Hylaeus rudbeckiae / granulatus")
ref <- ref[!ref$GenusSpecies %in% to.drop,]

counts <- c(ref$Females, ref$Males)
plants <- rep(ref$PlantGenusSpecies, 2)
pols <- rep(ref$GenusSpecies, 2)
sex <- rep(c("f","m"), each=nrow(ref))
ref.comm <- data.frame(Abund=counts,
                   PlantGenusSpecies=plants,
                   GenusSpecies=pols,
                   Sex=sex)

ref.comm$GenusSpeciesSex <- fix.white.space(paste(ref.comm$GenusSpecies,
                                              ref.comm$Sex))

net <- samp2site.spp(ref.comm$PlantGenusSpecies,
                      ref.comm$GenusSpeciesSex, ref.comm$Abund)

d.yose <- specieslevel(net, index='degree')
rare.pols.degree <- apply(net, 2, chao1)

save(d.yose, file="specimens/dYose.Rdata")

##******************************************************
## add traits to specimens database
##******************************************************
## correct date format
spec$Date <- as.Date(spec$Date, format='%m/%d/%y')
spec$doy <- as.numeric(strftime(spec$Date, format='%j'))
spec$Year <- as.factor(spec$Year)
## get specimen data ready
extra.round <- spec$Site == 'L21' & spec$Date == '2014-07-01'
spec <- spec[!extra.round,]

## create genus species columns
spec$GenusSpecies <- fix.white.space(paste(spec$Genus,
                                           spec$Species,
                                           spec$SubSpecies))

spec$PlantGenusSpecies <-  fix.white.space(paste(spec$PlantGenus,
                                                 spec$PlantSpecies,
                                                 spec$PlantVar,
                                                 spec$PlantSubSpecies))
## spec <- spec[!spec$GenusSpecies == "Apis mellifera",]
spec <-  spec[spec$GenusSpecies != '',]


## match trait data
keep.trait <- c('Sociality',
                'NestLoc',
                'Excavate',
                'Lecty',
                'Wood',
                'FireKill')
traits$GenusSpecies <-  fix.white.space(paste(traits$Genus,
                                                       traits$Species,
                                                       traits$SubSpecies))
spec <- cbind(spec, traits[match(spec$GenusSpecies,
                                 traits$GenusSpecies), keep.trait])

## interactions
spec$Int <-  fix.white.space(paste(spec$GenusSpecies,
                                   spec$PlantGenusSpecies))
spec$IntGen <-  fix.white.space(paste(spec$Genus,
                                      spec$PlantGenus))

## create plant by pollinator matrix to calculate specialization
prep.comm <- aggregate(spec$GenusSpecies,
                       list(PlantGenusSpecies=spec$PlantGenusSpecies,
                            PolGenusSpecies=spec$GenusSpecies,
                            Year=spec$Year),
                       length)

spec <- match.d(prep.comm,
                "PolSpec", "PlantSpec",
                spec, traits)
spec <- match.d(prep.comm[prep.comm$Year == "2013",],
                "PolSpec.2013", "PlantSpec.2013",
                spec, traits)
spec <- match.d(prep.comm[prep.comm$Year == "2014",],
                "PolSpec.2014", "PlantSpec.2014",
                spec, traits)

traits$PolSpec <-  spec$PolSpec[match(traits$GenusSpecies,
                                      spec$GenusSpecies)]
traits$PolSpec.2013 <-  spec$PolSpec.2013[match(traits$GenusSpecies,
                                      spec$GenusSpecies)]
traits$PolSpec.2014 <-  spec$PolSpec.2014[match(traits$GenusSpecies,
                                      spec$GenusSpecies)]


## reference data
load(file="specimens/dYose.Rdata")
spec$GenusSpeciesSex <- fix.white.space(paste(spec$Genus,
                                              spec$Species,
                                              spec$Sex))
spec$YoseDegree <- d.yose$'higher level'$'degree'[match(spec$GenusSpeciesSex,
                                           rownames(d.yose$'higher level'))]

d.yose$'higher level'$Sex <-
  sapply(strsplit(rownames(d.yose$'higher level'),' '),
         function(x) x[3])
d.yose$'higher level'$GenusSpecies <-
  sapply(strsplit(rownames(d.yose$'higher level'),' '),
         function(x) paste(x[1], x[2]))
d.yose$'higher level'$Genus <-
  sapply(strsplit(rownames(d.yose$'higher level'),' '),
         function(x) x[1])
genus.mean <- tapply(d.yose$'higher level'$degree,
                     d.yose$'higher level'$Genus, mean)

spec$YoseDegree[is.na(spec$YoseDegree)] <-
  d.yose$'higher level'$'degree'[match(spec$GenusSpecies[is.na(spec$YoseDegree)],
       d.yose$'higher level'$GenusSpecies)]

spec$YoseDegree[is.na(spec$YoseDegree)] <-
  genus.mean[match(spec$Genus[is.na(spec$YoseDegree)],
                   rownames(genus.mean))]
spec$YoseDegree[spec$Genus == "Hylaeus"] <-
  genus.mean[rownames(genus.mean) =="Hylaeus"]

traits$YoseDegree <-  spec$YoseDegree[match(traits$GenusSpecies,
                                      spec$GenusSpecies)]

traits$YoseRareDegree <- rare.pols.degree[match(traits$GenusSpecies, sapply(strsplit(names(rare.pols.degree),' '),
         function(x) paste(x[1], x[2])))]

save(traits, file='functionalTraits/bees.Rdata')
write.csv(spec, 'specimens/spec.csv', row.names=FALSE)
save(spec, file='specimens/spec.Rdata')


##******************************************************
## abundance of each species at each site (net and pan)
##******************************************************
## site-date by species matrix for all species
sr.sched$Date <- as.Date(sr.sched$Date, format='%m/%d/%y')
extra.round <- sr.sched$Site == 'L21' & sr.sched$Date == '2014-07-01'
sr.sched <- sr.sched[!extra.round,]

save.path <- '~/Dropbox/yosemite/analysis/data/matrices/'
## net and pan, all species
make.by.species(spec=spec,
                sr.sched=sr.sched,
                path.dir=
                file.path(save.path, 'combined/all'))

## net and pan, only bees
make.by.species(spec=spec[spec$GeneralID == 'Bee',],
                sr.sched=sr.sched,
                path.dir=
                file.path(save.path, 'combined/bees'))

## net only, all species
make.by.species(spec=spec[spec$NetPan == 'net',],
                sr.sched=sr.sched,
                path.dir=
                file.path(save.path, 'net/all'))

## net and only, only bees
make.by.species(spec=spec[spec$GeneralID == 'Bee' &
                  spec$NetPan == 'net',],
                sr.sched=sr.sched,
                path.dir=
                file.path(save.path, 'net/bees'))


## net, interactions
make.by.species(spec=spec[spec$GeneralID == 'Bee' &
                  spec$NetPan == 'net',],
                sr.sched=sr.sched,
                type="Int",
                path.dir=
                file.path(save.path, 'net/interactions'))

##******************************************************
## veg data
##******************************************************
## correct date format
veg$Date <- as.Date(veg$Date, format='%m/%d/%y')
veg$Year <- format(veg$Date, format='%Y')
veg$doy <- as.numeric(strftime(veg$Date, format='%j'))

## drop data from incomplete sampling round
extra.round <- veg$Site == 'L21' & veg$Date == '2014-07-01'
veg <- veg[!extra.round,]

veg$FlowerNum[is.na(veg$FlowerNum)] <- 0
veg$Occ <- veg$FlowerNum
veg$Occ[veg$Occ > 0] <- 1

veg$logFlowerNum <- veg$FlowerNum
veg$logFlowerNum[veg$logFlowerNum == 2] <- 10
veg$logFlowerNum[veg$logFlowerNum == 3] <- 100
veg$logFlowerNum[veg$logFlowerNum == 4] <- 1000
veg$logFlowerNum[veg$logFlowerNum == 5] <- 10000

veg$PlantGenusSpecies <-  fix.white.space(paste(veg$PlantGenus,
                                                 veg$PlantSpecies,
                                                 veg$PlantVar,
                                                 veg$PlantSubSpecies))
veg$SiteStatus <- spec$SiteStatus[match(veg$Site,
                                        spec$Site)]
write.csv(veg, 'veg/veg.csv', row.names=FALSE)
save(veg, file='veg/veg.Rdata')

##******************************************************
## site level data
##******************************************************
source('~/Dropbox/yosemite/analysis/data/src/siteLevelPrep.R')

