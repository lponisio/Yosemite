library(lmerTest)
library(lme4)
library(vegan)
library(fields)
library(linkcomm)
library(picante)
library(raster)
library(bipartite)

library(dplyr)
library(reshape2)
library(sem)
library(mvnormtest)
library(lavaan)
library(semPlot)
library(ggplot2)

source('src/misc.R')
source('src/cvCalc.R')
source('src/calcPca.R')
load('../../data/networks/allSpecimens.Rdata')
load('../../data/networks/all_networks_years.Rdata')
load('../../data/species_roles.Rdata')

traits <- read.csv("~/Dropbox/speciesRoles/data/traits.csv")
traits.ab.phen <- read.csv("~/Dropbox/speciesRoles/data/traitsPhenAb.csv")


species.roles$SiteYr <- paste(species.roles$Site,
                              species.roles$Year, sep='.')

## combine GenusSpecies + site + year
species.roles$sp.site.year <- paste(species.roles$GenusSpecies,
                                    species.roles$SiteYr, sep='.')

species.roles$SiteStatus <-
    spec$SiteStatus[match(species.roles$SiteYr, paste(spec$Site, spec$Year, sep="."))]


BACI <- c("Barger", "Butler", "MullerB", "Sperandio", "Hrdy")
species.roles$SiteStatus[species.roles$Site %in% BACI] <- "hedgerow"

species.roles$SiteStatus[species.roles$SiteStatus == "mature" |
                         species.roles$SiteStatus == "maturing" |
                         species.roles$SiteStatus == "forb"] <- "hedgerow"
