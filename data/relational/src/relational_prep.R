## *******************************************************
## make files for use in relational database
## *******************************************************
## load data
D <- read.csv("../original/specimens/cleaned/specimens.csv")

## *******************************************************
## create insect specimen database
spec <- data.frame(EMEC=D$EMEC,
                   UniqueID=D$UniqueID,
                   TempID=D$TempID,
                   Order=D$Order,
                   Family=D$Family,
                   Genus=D$Genus,
                   SubGenus=D$Subgenus,
                   Species=D$Species,
                   SubSpecies=D$Subspecies,
                   GeneralID=D$GeneralID,
                   Sex=D$Sex,
                   NetPan=D$NetPan,
                   Site=D$Site,
                   Year=D$Year,
                   Collector=D$Collector,
                   SampleRound=D$SampleRound,
                   Date=D$Date,
                   PanColor=D$PanColor,
                   PanID=D$PanID,
                   NetNumber=D$NetNumber,
                   FinalPlantSp=D$PlantSp,
                   DateDetermined=D$DateDetermined,
                   Determiner=D$Determiner,
                   Author=D$Author)

write.csv(spec, file="../preped_data/specimen.csv",
                   row.names=FALSE)
## *******************************************************

## *******************************************************
## create weather file

W <- read.csv('../original/weather.csv')

weather <- data.frame(NetPan=W$NetPan,
                   Site=W$Site,
                   SampleRound=W$SampleRound,
                   Date=W$Date,
                   StartTime=W$StartTime,
                   EndTime=W$EndTime,
                   TempStart=W$StartTemp,
                   TempEnd=W$EndTemp,
                   WindStart=W$StartWind,
                   WindEnd=W$EndWind,
                   SkyStart=W$StartSky,
                   SkyEnd=W$EndSky,
                   Collector=W$Collector,
                   NetNumber =W$NetNumber,
                   Year=W$Year)

## write unique data to a table
write.csv(weather, file='../preped_data/weather.csv',
                   row.names=FALSE)
## *******************************************************

## *******************************************************
## create site attributes file

C <- read.csv('../original/siteCharacteristics.csv')

char <- data.frame(Park=C$Park,
                   Site=C$Site,
                   Lat=C$Lat,
                   Long=C$Long,
                   State=C$State,
                   Country=C$Country,
                   County=C$County,
                   Locality =C$Locality,
                   HeatLoad=C$HeatLoad,
                   TimesBurnt=C$TimesBurnt,
                   SiteStatus=C$SiteStatus,
                   Aspect=C$Aspect,
                   DomSp=C$DomSp,
                   Slope=C$Slope,
                   FirePerim=C$FirePerim)

## write unique data to a table
write.csv(unique(char), file='../preped_data/geography.csv',
                   row.names=FALSE)


## *******************************************************

## *******************************************************
## create plant survey database

V <- read.csv('../original/veg.csv')

veg <- data.frame(Site=V$Site,
                   Collector=V$Collector,
                   Date=V$Date,
                   FinalPlantSp=V$PlantSp,
                   BloomStatus=V$BloomStatus,
                   FlowerNum=V$NumFlower)

write.csv(veg, file='../preped_data/veg.csv',
                   row.names=FALSE)
## *******************************************************

## *******************************************************
