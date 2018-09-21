## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
f.path <- 'figures/diagnostics'
args <- commandArgs(trailingOnly=TRUE)

binary <- FALSE
alpha <- TRUE
## ints or pols
#type <- "pols"
if(length(args) == 0){
    type <- "pols"
} else{
    type <- args[1]
}

only.baci <- TRUE

source('src/initialize_beta.R')

## ************************************************************
## beta diversity as variation between years,
## centroid for each site
## ************************************************************

## @knitr external_beta_div
dis <- mapply(function(a, b, c, d)
    calcBetaStatus(comm= a, ## observed communities
                   status= b, ## vector of site types
                   dis.method, ## dissimilarity metric
                   nulls=c, ## null communities
                   occ=binary, ## binary or abundance weighted?
                   years=d, ## calculate beta div within?
                   sub=type,
                   zscore=FALSE), ## use Chase method not zscores
    a=comm$comm,
    b=comm$status,
    c= nulls,
    d= comm$comm,
    SIMPLIFY=FALSE)

## @knitr external_beta_div_end
dats <- makeBetaDataPretty()

dats.type <- dats[dats$speciesType == speciesType,]

print('number of species')
length(unique(dats.type$species))

print('species with the greatest values')
dats.type$species[order(dats.type$dist, decreasing=TRUE)[1:5]]

print('species with the smalest values')
dats.type$species[order(dats.type$dist)[1:3]]


type.mod.beta <- lmer(dist ~ scale(log(median.days)) +
                          scale(log(median.abund)) +
                          scale(d) +
                          (1|species) + (1|site)
                 + (1|year), data=dats.type)

summary(type.mod.beta)
vif.mer(type.mod.beta)


pdf(file=file.path(f.path,
                  sprintf("%s.pdf", paste(type, "density", sep=""))),
    width=6, height=9)
layout(matrix(1:3))
plot(density(residuals(type.mod.beta)))


## super colinear
## remove degree
abund.type.mod.beta <- lmer(dist ~ scale(log(median.days)) +
                          scale(log(median.abund)) +
                          (1|species) + (1|site)
                 + (1|year), data=dats.type)

summary(abund.type.mod.beta)
vif.mer(abund.type.mod.beta)
plot(density(residuals(abund.type.mod.beta)))

## remove abundance
degree.type.mod.beta <- lmer(dist ~ scale(log(median.days)) +
                          scale(log(r.degree)) +
                          (1|species) + (1|site)
                 + (1|year), data=dats.type)

summary(degree.type.mod.beta)
vif.mer(degree.type.mod.beta)
plot(density(residuals(degree.type.mod.beta)))

dev.off()

save(dats.type, type.mod.beta, degree.type.mod.beta, abund.type.mod.beta,
     file= file.path('saved/speciesTurnover',
                     sprintf('%s.Rdata', paste(dis.method, alpha, occ, type,
                                             sep='_'))))

## diagnostics
source('src/diagnostics_beta-div.R')

pdf.f(box.site, file=file.path(f.path,
                  sprintf("%s.pdf", paste(type, "box", sep=""))),
      width=6, height=9)


pdf.f(resid.plot, file=file.path(f.path,
                    sprintf("%s.pdf", paste(type, "resid", sep=""))),
      width=6, height=9)

