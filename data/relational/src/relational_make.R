## *******************************************************
## create relational database
## *******************************************************
library(RSQLite)

specimens <- read.csv('../preped_data/specimen.csv', as.is=TRUE)
conditions <- read.csv('../preped_data/weather.csv', as.is=TRUE)
geo <- read.csv('../preped_data/geography.csv',
                as.is=TRUE)
veg <- read.csv('../preped_data/veg.csv', as.is=TRUE)

## *******************************************************
##  import the conditions information
## *******************************************************

## check that if there is already a database, remove it
if(file.exists('yose.db')) file.remove('yose.db')
con <- dbConnect(dbDriver('SQLite'), dbname='yose.db')

## *******************************************************
## 1. Geographic infomation
## *******************************************************

keep <- c('Site', 'Country', 'State', 'County', 'Park', 'Locality',
          'Lat', 'Long', 'SiteStatus', "FirePerim")
geography <- unique(geo[keep])
## next sort into alphabetical order
geography <- geography[match(sort(geography$Site), geography$Site),]

## generate primary geography key
geography <- cbind(GeographyPK=seq_len(nrow(geography)), geography)
rownames(geography) <- NULL
dbWriteTable(con, 'tblGeography', geography, row.names=FALSE)

## Propagate geography key to the conditions table.
conditions$GeographyFK <-
  geography$GeographyPK[match(conditions$Site, geography$Site)]
## Propagate geography key to the specimens table.
specimens$GeographyFK <- geography$GeographyPK[match(specimens$Site,
                                                     geography$Site)]
## Propagate geography key to the veg table.
veg$GeographyFK <- geography$GeographyPK[match(veg$Site,
                                               geography$Site)]

## write a .csv version of this table (just for ease of viewing)
write.csv(dbReadTable(con, 'tblGeography'),
          file='tables/geography.csv', row.names=FALSE)

dbListTables(con)
## *******************************************************
## 2. Conditions
## *******************************************************

## Temporarily identify unique combinations:
keep <- c('GeographyFK', 'Date', 'NetPan', 'NetNumber')
conditions$cond.code <- apply(conditions[keep], 1, paste, collapse=';')
specimens$cond.code <- apply(specimens[keep], 1, paste, collapse=';')

## make table
keep <- c('Site', 'Year', 'Date', 'SampleRound','NetNumber', 'NetPan',
          'StartTime', 'EndTime', 'TempStart', 'TempEnd', 'WindStart',
          'WindEnd', 'SkyStart', 'SkyEnd', 'GeographyFK', 'cond.code')

cond <- unique(conditions[keep])
rownames(cond) <- NULL
cond <- cbind(ConditionsPK=seq_len(nrow(cond)), cond)
## Don't upload the cond.code column
dbWriteTable(con, 'tblConditions', cond[-ncol(cond)], row.names=FALSE)

## Propagate conditions key to the conditions table.
conditions$ConditionsFK <-
  cond$ConditionsPK[match(conditions$cond.code, cond$cond.code)]
## Propagate conditions key to the specimens table.
specimens$ConditionsFK <-
  cond$ConditionsPK[match(specimens$cond.code, cond$cond.code)]

write.csv(dbReadTable(con, 'tblConditions'),
          file='tables/conditions.csv', row.names=FALSE)

## *******************************************************
## 3. Insect species:
## *******************************************************

keep <- c( 'Order', 'Family', 'Genus',
          'SubGenus', 'Species', 'SubSpecies', 'Determiner', 'Author',
          'GeneralID', 'Sex')

insects <- specimens[keep]
insects <- unique(insects)

insects$gen.sp <- paste(insects$Order,
                        insects$Family,
                        insects$Genus,
                        insects$SubGenus,
                        insects$Species,
                        insects$SubSpecies, sep=';')
insects <- cbind(InsectPK=seq_len(nrow(insects)), insects)
rownames(insects) <- NULL

dbWriteTable(con, 'tblInsect', insects[-ncol(insects)],
             row.names=FALSE)

## Propagate insect key to the specimens table.
specimens$gen.sp <- paste(specimens$Order,
                          specimens$Family,
                          specimens$Genus,
                          specimens$SubGenus,
                          specimens$Species,
                          specimens$SubSpecies, sep=';')
specimens$InsectFK <- insects$InsectPK[match(specimens$gen.sp,
                                             insects$gen.sp)]

write.csv(dbReadTable(con, 'tblInsect'),
          file='tables/insect.csv', row.names=FALSE)

## *******************************************************
## 4. Plant species:
## *******************************************************

keep <- c('FinalPlantSp')
plants <- specimens[keep][[1]]
plants.veg <- veg[keep][[1]]
plants <- c(plants, plants[!plants %in% plants.veg])
plants <- sort(unique(plants))

PlantGenus <- sapply(strsplit(plants, ' '), function(x) x[1])
PlantGenus[is.na(PlantGenus)] <- ''
PlantSpecies <- sapply(strsplit(plants, ' '), function(x) x[2])
PlantSpecies[is.na(PlantSpecies)] <- ''
PlantVar <- sapply(strsplit(plants, ' '), function(x) x[3])
PlantVar[is.na(PlantVar)] <- ''
PlantSubSpecies <- sapply(strsplit(plants, ' '), function(x) x[4])
PlantSubSpecies[is.na(PlantSubSpecies)] <- ''

plants <- data.frame(PlantPK=seq_along(PlantGenus),
                     PlantGenus, PlantSpecies, PlantVar, PlantSubSpecies)
rownames(plants) <- NULL
dbWriteTable(con, 'tblPlant', plants, row.names=FALSE)

## Propagate plant key to the specimens table.
specimens.plant.sp <- specimens[keep][[1]]
veg.plant.sp <- veg[keep][[1]]

plants.plant.sp <- fix.white.space(paste(plants$PlantGenus,
                                         plants$PlantSpecies,
                                         plants$PlantVar,
                                         plants$PlantSubSpecies))

## propogate plant key to specimens
specimens$PlantFK <- plants$PlantPK[match(specimens.plant.sp,
                                          plants.plant.sp)]

## propogate plant key to veg
veg$PlantFK <- plants$PlantPK[match(veg.plant.sp,
                                    plants.plant.sp)]

write.csv(dbReadTable(con, 'tblPlant'),
          file='tables/plant.csv', row.names=FALSE)

## *******************************************************
## 5. Pan info:
## *******************************************************

keep <- c('PanColor',
          'PanID')
pans <- unique(specimens[keep])
pans$PanPK <- seq_len(nrow(pans))
rownames(pans) <- NULL
dbWriteTable(con, 'tblPan', pans, row.names=FALSE)

## pan key to the specimens table.
pans.pan.info <- paste(pans$PanColor,
                       pans$PanID, sep=';')
specimens.pan.info <- paste(specimens$PanColor,
                            specimens$PanID, sep=';')
specimens$PanFK <- pans$PanPK[match(specimens.pan.info, pans.pan.info)]

write.csv(dbReadTable(con, 'tblPan'),
          file='tables/pan.csv', row.names=FALSE)

## *******************************************************
## 6. Specimens:
## *******************************************************

keep <- c('EMEC',
          'UniqueID',
          'TempID',
          'Collector',
          'InsectFK',
          'PlantFK',
          'PanFK',
          'ConditionsFK',
          'GeographyFK')

specimens <- specimens[keep]
specimens <- unique(specimens)
rownames(specimens) <- NULL

dbWriteTable(con, 'tblSpecimens', specimens, row.names=FALSE)

write.csv(dbReadTable(con, 'tblSpecimens'),
          file='tables/specimens.csv', row.names=FALSE)

print(paste("before traditional, dim=", nrow(specimens)))
## *******************************************************
## 7. veg:
## *******************************************************

keep <- c('Site',
  'Date',
          'BloomStatus',
          'FlowerNum',
          'PlantFK')

veg <- veg[keep]
veg <- unique(veg)
rownames(veg) <- NULL

dbWriteTable(con, 'tblVeg', veg, row.names=FALSE)

write.csv(dbReadTable(con, 'tblVeg'),
          file='tables/veg.csv', row.names=FALSE)

## *******************************************************
## close connection to database
## *******************************************************
dbDisconnect(con)
