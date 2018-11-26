setwd("~/Dropbox/Yosemite/data")
source('SpeciesIDs/src/AddData.R')

spec <- read.csv('relational/original/specimens/cleaned/specimens.csv')
spec$Order <-
  spec$Family <-
  spec$Genus <-
  spec$Subgenus <-
  spec$Species <-
  spec$Subspecies <-
  spec$Sex <-
  spec$Determiner <-
  spec$DateDetermined <-
  spec$Author <-
  spec$GeneralID <- NA

write.csv(spec,
          file='relational/original/specimens/cleaned/specimens.csv',
          row.names=FALSE)

source('SpeciesIDs/IDs/Bees/Andrenidae.R')
add.to.data(sp.ids=sp.ids,
            case='bee', "Andrenidae","2013-2015")

source('SpeciesIDs/IDs/Bees/Anthophoridae.R')
add.to.data(sp.ids=sp.ids,
            case='bee', "Apidae","2013-2015")

source('SpeciesIDs/IDs/Bees/Apidae.R')
add.to.data(sp.ids=sp.ids,
            case='bee', "Apidae","2013-2015")

source('SpeciesIDs/IDs/Bees/Colletidae.R')
add.to.data(sp.ids=sp.ids,
            case='bee', "Colletidae","2013-2015")

source('SpeciesIDs/IDs/Bees/Halictidae.R')
add.to.data(sp.ids=sp.ids,
            case='bee', "Halictidae","2013-2015")

source('SpeciesIDs/IDs/Bees/Megachilidae.R')
add.to.data(sp.ids=sp.ids,
            case='bee',"Megachilidae","2013-2015")

source('SpeciesIDs/IDs/Wasps/Chrysidids.R')
add.to.data(sp.ids=sp.ids,
            case='wasp',"Chrysididae","2015")

source('SpeciesIDs/IDs/Wasps/Vespids.R')
add.to.data(sp.ids=sp.ids,
            case='wasp',"Vespidae","2015")

source('SpeciesIDs/IDs/Wasps/Crabronidae.R')
add.to.data(sp.ids=sp.ids,
            case='wasp',"Crabronidae","2015")

source('SpeciesIDs/IDs/Leps/Butterflies.R')
add.to.data(sp.ids=sp.ids,
            case='lep',"","2015")

source('SpeciesIDs/IDs/Beetles/Beetles.R')
add.to.data(sp.ids=sp.ids,
            case='beetle',"","2015")

source('SpeciesIDs/IDs/Flies/syrphidae.R')
add.to.data(sp.ids=sp.ids,
            case='fly',"Syrphidae","2015")

source('SpeciesIDs/IDs/Flies/bombyliidae.R')
add.to.data(sp.ids=sp.ids,
            case='fly',"","2015")

source('SpeciesIDs/IDs/Flies/misc.R')
add.to.data(sp.ids=sp.ids,
            case='fly',"","2015")

