setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')

load('saved/results/partnerVar.Rdata')
load('saved/results/pcaVar.Rdata')


mean.partner.var <- aggregate(beta.dist$dist,
                              list(GenusSpecies=beta.dist$GenusSpecies,
                                   Year=beta.dist$Year),
                              FUN=mean, na.rm=TRUE)

flexibility <- merge(mean.partner.var, pol.pca.scores$pca.var)

flexibility.pre.drought <- flexibility[flexibility$Year == 2013,]



spec$Doy <- as.numeric(strftime(spec$Date, format = "%j"))

spec$DoyPoly <- poly(spec$Doy, degree=2)

spec <- spec[spec$NetPan =="net",]
spec.abund <- aggregate(list(Abund=spec$GenusSpecies),
                        list(GenusSpecies=spec$GenusSpecies,
                            Site=spec$Site,
                        Doy=spec$Doy,
                        Year=spec$Year,
                        SiteStatus=spec$SiteStatus),
                        length)


spec.abund$DoyPoly <- poly(spec.abund$Doy, degree=2)
spec.abund$DoyPoly1 <- spec.abund$DoyPoly[,'1']
spec.abund$DoyPoly2 <- spec.abund$DoyPoly[,'2']

mod.var <- glmer(Abund~ scale(DoyPoly1)*Year + scale(DoyPoly2)*Year + (1|Site) +
                     (scale(DoyPoly1)|GenusSpecies) +  (scale(DoyPoly2)|GenusSpecies),
                 data = spec.abund,
                 family="poisson")
summary(mod.var)
