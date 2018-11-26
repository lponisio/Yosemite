library(RSQLite)
setwd('~/Dropbox/Yosemite/data/relational/relational')
## connect to the relational database
con <- dbConnect(dbDriver('SQLite'), dbname='yose.db')

## **************************************************
## specimens
## **************************************************
## make a table containing everything
sql <- paste('SELECT * FROM tblSpecimens',
             'JOIN tblInsect',
             'ON tblSpecimens.InsectFK = tblInsect.InsectPK',
             'JOIN tblPlant',
             'ON tblSpecimens.PlantFK = tblPlant.PlantPK',
             'JOIN tblPan',
             'ON tblSpecimens.PanFK = tblPan.PanPK',
             'JOIN tblConditions',
             'ON tblSpecimens.ConditionsFK = tblConditions.ConditionsPK',
             'JOIN tblGeography',
             'ON tblSpecimens.GeographyFK = tblGeography.GeographyPK')
res.complete <- dbGetQuery(con, sql)

## check dimensions of dataset
print(paste("after traditional, dim=", nrow(res.complete)))

## drop unwanted columns
drop1 <- c('InsectPK', 'InsectFK',
          'GeographyPK', 'GeographyFK',
          'PlantPK', 'PlantFK',
          'PanPK', 'PanFK',
          'ConditionsPK', 'ConditionsFK')
res.complete <- res.complete[!colnames(res.complete) %in% drop1]
drop2 <- c('GeographyFK..44', "Site..46")
res.complete <- res.complete[!colnames(res.complete) %in% drop2]
print(paste("after traditional, dim=", nrow(res.complete)))

## set NA to blanks
res.complete[is.na(res.complete)] <- ''

## write full table to csv
write.csv(res.complete, file='traditional/specimens-complete.csv',
          row.names=FALSE)

## **************************************************
## veg
## **************************************************
## make a table containing everything
sql <- paste('SELECT * FROM tblVeg',
             'JOIN tblPlant',
             'ON tblVeg.PlantFK = tblPlant.PlantPK')
res.complete <- dbGetQuery(con, sql)

## check dimensions of dataset
dim(res.complete)

## drop unwanted columns
drop3<- c('PlantPK', 'PlantFK')
res.complete <- res.complete[!colnames(res.complete) %in% drop3]

## set NA to blanks
res.complete[is.na(res.complete)] <- ''

## write full table to csv
write.csv(res.complete, file='traditional/veg-complete.csv',
          row.names=FALSE)

## close connection to database
dbDisconnect(con)
