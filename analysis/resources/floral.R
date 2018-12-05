rm(list=ls())
setwd('~/Dropbox/Yosemite/analysis/resources')
type <- "all"
d.to.keep <- "150"
binary <- FALSE
source('src/initialize.R')

## *******************************************************************
## species level
## *******************************************************************
ys.sp <- c("logFlowerNum")
fam.sp <- c("gaussian")
formulas.sp <-  as.formula(paste(ys.sp, "~",
                                 paste("s.doy",
                                       "I(s.doy^2)",
                                       "Year*SiteStatus",
                                       "(1|Site)",
                                       "(1|PlantGenusSpecies)",
                                       sep="+")))

out.mods.sp <- mod(forms= formulas.sp,
                   fam= fam.sp,
                   ys=ys.sp,
                   dats=veg)

save(out.mods.sp, file="saved/SpMods.Rdata")

## *******************************************************************
## plant community level
## *******************************************************************

ys <- c("Richness",  "Div")
fams <- c("poisson", "gaussian")

## create formulas
formulas <- lapply(ys, function(x) {
  as.formula(paste(x, "~",
                   paste("Year*SiteStatus",
                         "s.doy",
                         "I(s.doy^2)",
                         "(1|Site)",
                         sep="+")))
})


out.mods <- mapply(function(a, b, c)
                   mod(forms= a,
                       fam= b,
                       ys=c,
                       dats=by.site),
                   a=formulas,
                   b=fams,
                   c=ys,
                   SIMPLIFY=FALSE)

names(out.mods) <- ys
lapply(out.mods, AIC)

save(out.mods, file="saved/mods.Rdata")
