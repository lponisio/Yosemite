## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
f.path <- 'figures/diagnostics'
type <- "all"

load('../../data/nestedContribution.Rdata')

sd.contr.nodf <- aggregate(
  list(contr.nodf = contr.nodf$nestedcontribution),
  list(Site = contr.nodf$Site, GenusSpecies = contr.nodf$GenusSpecies), sd)

sd.contr.nodf <- sd.contr.nodf[sd.contr.nodf$Site %in% BACI,]
sd.contr.nodf <- sd.contr.nodf[!is.na(sd.contr.nodf$contr.nodf),]
## pollinators

traits.contr.nodf <- merge(sd.contr.nodf, traits.ab.phen, "GenusSpecies")

print("number of pollinators")
length(unique(traits.contr.nodf$GenusSpecies[traits.contr.nodf$speciesType=="pollinator"]))

only.pols <- traits.contr.nodf[traits.contr.nodf$speciesType=="pollinator", ]
only.pols.cnodf <- aggregate(only.pols$contr.nodf, by=list(GenusSpecies=only.pols$GenusSpecies), FUN = mean)

print('species with the greatest values')
only.pols.cnodf[order(only.pols.cnodf$x)[1:5],]

print('species with the smallest values')
only.pols.cnodf[order(only.pols.cnodf$x, decreasing = TRUE)[1:5],]


## linear model of pca varaible and species traits
pol.mod.contr.nodf <- lmer(contr.nodf  ~ scale(d) +
                        scale(log(median.days)) +
                        scale(log(median.abund))  +
                        (1|GenusSpecies) + (1|Site),
                        data=traits.contr.nodf[
                            traits.contr.nodf$speciesType=="pollinator",])

summary(pol.mod.contr.nodf)
vif.mer(pol.mod.contr.nodf)

pdf(file=file.path(f.path,
                  sprintf("%s.pdf", paste(type, "density", sep=""))),
    width=6, height=9)
layout(matrix(1:4))
plot(density(residuals(pol.mod.contr.nodf)))

## vif for degree and days > 2 so seperate
## take out degree
abund.pol.mod.contr.nodf <- lmer(contr.nodf ~   scale(log(median.days)) +
                              scale(log(median.abund))  +
                              (1|GenusSpecies) + (1|Site),
                          data=traits.contr.nodf[
                              traits.contr.nodf$speciesType=="pollinator",])

plot(density(residuals(abund.pol.mod.contr.nodf)))
summary(abund.pol.mod.contr.nodf)
vif.mer(abund.pol.mod.contr.nodf)

## take out days
degree.pol.mod.contr.nodf <- lmer(contr.nodf ~   scale(log(median.abund)) +
                              scale(log(r.degree))  +
                              (1|GenusSpecies) + (1|Site),
                          data=traits.contr.nodf[
                              traits.contr.nodf$speciesType=="pollinator",])

plot(density(residuals(degree.pol.mod.contr.nodf)))
summary(degree.pol.mod.contr.nodf)
vif.mer(degree.pol.mod.contr.nodf)


## *********************************************************************
## plants
## *********************************************************************
plant.mod.contr.nodf <- lmer(contr.nodf  ~ scale(log(r.degree)) +
                        scale(log(median.days)) +
                        scale(log(median.abund))  +
                        (1|GenusSpecies) + (1|Site),
                        data=traits.contr.nodf[
                            traits.contr.nodf$speciesType=="plant",])

summary(plant.mod.contr.nodf)
vif.mer(plant.mod.contr.nodf)

plot(density(residuals(plant.mod.contr.nodf)))

## vif for degree and abund > 2 so seperate
## take out degree
abund.plant.mod.contr.nodf <- lmer(contr.nodf ~   scale(log(median.days)) +
                              scale(log(median.abund))  +
                              (1|GenusSpecies) + (1|Site),
                          data=traits.contr.nodf[
                              traits.contr.nodf$speciesType=="plant",])

plot(density(residuals(abund.plant.mod.contr.nodf)))
summary(abund.plant.mod.contr.nodf)
vif.mer(abund.plant.mod.contr.nodf)

## take out abund
degree.plant.mod.contr.nodf <- lmer(contr.nodf ~   scale(log(median.days)) +
                              scale(log(r.degree))  +
                              (1|GenusSpecies) + (1|Site),
                          data=traits.contr.nodf[
                              traits.contr.nodf$speciesType=="plant",])

plot(density(residuals(degree.plant.mod.contr.nodf)))
summary(degree.plant.mod.contr.nodf)
vif.mer(degree.plant.mod.contr.nodf)

save(abund.pol.mod.contr.nodf, degree.pol.mod.contr.nodf,
     abund.plant.mod.contr.nodf, degree.plant.mod.contr.nodf,
     traits.contr.nodf,
     file="../../../speciesRoles_saved/contrNodf.Rdata")