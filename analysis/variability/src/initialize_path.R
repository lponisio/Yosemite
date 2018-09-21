traits <- read.csv(file=file.path('../../data', "traitsPhenAb.csv"))
## separating bee and syrphid

beeNonbee <- data.frame(GenusSpecies = spec$GenusSpecies, BeeNonbee = spec$BeeNonbee)
beeNonbee <- beeNonbee[!duplicated(beeNonbee),]
type.bees <- beeNonbee$GenusSpecies[beeNonbee$BeeNonbee==type]


#transforming lecty into numeric
lecty <- data.frame("GenusSpecies" = traits$GenusSpecies,
                    "Lecty" = as.numeric(traits$Lecty)-1)
