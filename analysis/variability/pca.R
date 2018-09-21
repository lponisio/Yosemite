## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
f.path <- 'figures/diagnostics'
type <- "all"

## subseting only the species from the baci sites
species.roles <- species.roles[species.roles$Site %in% BACI,]

## vector of pca loadings of interest
loadings <- c(1)
metrics <- c("degree", "betweenness", "closeness",
             "niche.overlap", "species.strength")

## the metrics used in the PCA
var.method <- sd
ave.method <- mean

## pollinators
pol <- species.roles[species.roles$speciesType == "pollinator",]
pol.traits <- traits.ab.phen[traits.ab.phen$speciesType == "pollinator",]

pol.pca.scores <- calcPcaMeanVar(species.roles=pol,
                                 var.method=var.method,
                                 metrics= metrics,
                                 loadings=loadings,
                                 ave.method=ave.method,
                                 traits=pol.traits, na.rm=TRUE,
                                 agg.col = "Site")

print('number of species')
length(unique(pol.pca.scores$pca.var$GenusSpecies[which(!is.na(pol.pca.scores$pca.var$var.pca1))]))

print('species with the greatest values')
pol.pca.scores$pca.var$GenusSpecies[order(pol.pca.scores$pca.var$var.pca1, decreasing=TRUE)[1:5]]

print('species with the smalest values')
pol.pca.scores$pca.var$GenusSpecies[order(pol.pca.scores$pca.var$var.pca1)[1:5]]

## linear model of pca varaible and species traits
pol.mod.pca <- lmer(sqrt(var.pca1) ~ scale(d) +
                        scale(median.days) +
                        scale(log(median.abund))  +
                        (1|GenusSpecies) + (1|Site),
                    data=pol.pca.scores$pca.var)

summary(pol.mod.pca)
vif.mer(pol.mod.pca)

pdf(file=file.path(f.path,
                  sprintf("%s.pdf", paste(type, "density", sep=""))),
    width=6, height=9)
layout(matrix(1:4))
plot(density(residuals(pol.mod.pca)))

## vif for degree and days > 2 so seperate
## take out degree
abund.pol.mod.pca <- lmer(sqrt(var.pca1) ~   scale(median.days) +
                              scale(log(median.abund))  +
                              (1|GenusSpecies) + (1|Site),
                          data=pol.pca.scores$pca.var)
plot(density(residuals(abund.pol.mod.pca)))
summary(abund.pol.mod.pca)
vif.mer(abund.pol.mod.pca)

## take out days
degree.pol.mod.pca <- lmer(sqrt(var.pca1) ~   scale(log(r.degree)) +
                               scale(log(median.abund))  +
                               (1|GenusSpecies) + (1|Site),
                           data=pol.pca.scores$pca.var)

plot(density(residuals(degree.pol.mod.pca)))
summary(degree.pol.mod.pca)
vif.mer(degree.pol.mod.pca)

## *********************************************************************
## plants
## *********************************************************************

plants <- species.roles[species.roles$speciesType == "plant",]
traits.plan <- traits.ab.phen[traits.ab.phen$speciesType == "plant",]
plant.pca.scores <- calcPcaMeanVar(species.roles=plants,
                                   var.method=var.method, metrics= metrics,
                                   loadings=loadings, ave.method=ave.method,
                                   traits=traits.plan)

## linear model of pca varaible and species traits
plant.mod.pca <- lmer(var.pca1 ~ scale(d) + scale(log(median.days))
                      + scale(log(median.abund)) +
                          (1|GenusSpecies) + (1|Site),
                      data=plant.pca.scores$pca.var)
plot(density(residuals(plant.mod.pca)))
summary(plant.mod.pca)
vif.mer(plant.mod.pca)

dev.off()

## vif for degree and abund > 2 so seperate
## take out degree
plant.mod.pca <- lmer(var.pca1 ~  scale(log(median.days))
                      + scale(log(median.abund)) +
                        (1|GenusSpecies) + (1|Site),
                      data=plant.pca.scores$pca.var)
plot(density(residuals(plant.mod.pca)))
summary(plant.mod.pca)
vif.mer(plant.mod.pca)


## take out abundance
degree.plant.mod.pca <- lmer(var.pca1 ~  scale(log(median.days))
                      + scale(log(r.degree)) +
                        (1|GenusSpecies) + (1|Site),
                      data=plant.pca.scores$pca.var)
plot(density(residuals(degree.plant.mod.pca)))
summary(degree.plant.mod.pca)
vif.mer(degree.plant.mod.pca)


dev.off()


save(plant.mod.pca, pol.mod.pca, degree.pol.mod.pca,
     abund.pol.mod.pca, plant.pca.scores, pol.pca.scores,
     file="../../../speciesRoles_saved/pcaScores.Rdata")

## diagnostics
source('src/diagnostics_centrality.R')

type <- "all"
pdf.f(box.site, file=file.path(f.path,
                  sprintf("%s.pdf", paste(type, "box", sep=""))),
      width=9, height=6)


pdf.f(resid.plot, file=file.path(f.path,
                    sprintf("%s.pdf", paste(type, "resid", sep=""))),
      width=6, height=9)

dev.off()
