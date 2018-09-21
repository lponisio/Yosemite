## setwd('~/Dropbox/speciesRoles')
rm(list=ls())

setwd('analysis/variability')
source('plotting/src/predictIntervals.R')
source('plotting/src/CIplotting.R')
source('plotting/src/plotPanels.R')
source('src/initialize.R')

type <- "bee" #bees or non-bee ## To separate bees and syrphids
source('src/initialize_path.R')

results.path.lecty <- list()
fig.path <- '~/Dropbox/speciesRoles_saved/figures/'

## Creating the models
## indirect effect through rdegree
path.model <- '

y ~ a*abund + d*phen + e*rdeg + f*lec
rdeg ~ b*abund + c*phen + g*lec

indirect.ab   := b*e
indirect.phe  := c*e
indirect.db  := g*e
direct        := e
total         := a + d + f + e + (b*e) + (c*e) + (g*e)
'
################################################
## partner variability ----
################################################
load(file='saved/speciesTurnover/chao_TRUE_abund_pols.Rdata') #Partner varialbility
y.variable <- "partner variability"
dats.type <- dats.type[dats.type$species %in% type.bees, ]

## it is suggested to have a dataframe with just the variables you'll use.
vars <- c("species", "dist", "median.abund", "median.days", "r.degree", "Lecty")
dats.type <- dats.type[,vars]

## renaming variables beacuse lavvan renames them and they become unintelligible
partners.path <- dats.type %>%
  plyr::rename(c("species"="spp", "dist"="y", "median.abund"="abund", "median.days"="phen", "r.degree"="rdeg", "Lecty"="lec")) %>%
  group_by(spp)  %>%
  mutate(y = mean(y))

var.mean.trait <- as.data.frame(partners.path[!duplicated(partners.path),])

################################################
### Path analysis
# getting the mean and creating a new dataframe

#logging numerical variables
var.mean.trait[,c("abund", "phen", "rdeg")] <- scale(log(var.mean.trait[,c("abund", "phen", "rdeg")]))
partner.path.model <- lavaan::sem(path.model, data=var.mean.trait)

summary(partner.path.model, standardized=TRUE, fit=TRUE, rsquare=TRUE)

#plotting and saving
pdf(file=file.path(fig.path,
                   sprintf("%s.pdf", paste("path_", type, "PartnerVar", sep=""))), width=10, height= 8)
semPaths(partner.path.model, what = 'std', whatLabels='std',style = "lisrel",
         layout = "tree", edge.label.bg=TRUE)#,
dev.off()

################################################
## Role variability pca ----
################################################
load(file="~/Dropbox/speciesRoles_saved/pcaScores.Rdata") #role variability

y.variable <- "Role variability"

role.vars <- pol.pca.scores$pca.var
role.vars <- role.vars[role.vars$GenusSpecies %in% type.bees, ]

## it is suggested to have a dataframe with just the variables you'll use.
vars <- c("GenusSpecies", "var.pca1", "median.abund", "median.days", "r.degree", "Lecty")
role.vars <- role.vars[,vars]

## renaming variables beacuse lavvan renames them and they become unintelligible
role.path <- role.vars %>%
  plyr::rename(c("GenusSpecies"="spp", "var.pca1"="y", "median.abund"="abund", "median.days"="phen", "r.degree"="rdeg", "Lecty"="lec")) %>%
  group_by(spp)  %>%
  mutate(y = mean(y, na.rm=TRUE))

role.path <- role.path[!is.na(role.path$y),]
role.path <- as.data.frame(role.path[!duplicated(role.path),])

## logging numerical variables
role.path[,c("abund", "phen", "rdeg")] <- scale(log(role.path[,c("abund", "phen", "rdeg")]))

role.path.model <- lavaan::sem(path.model, data=role.path)

summary(role.path.model, standardized=TRUE, fit=TRUE, rsquare=TRUE)
#plotting and saving
pdf(file=file.path(fig.path,
                   sprintf("%s.pdf", paste("path_", type, "RoleVar", sep=""))), width=10, height= 8)
semPaths(role.path.model, what = 'std', whatLabels='std',style = "lisrel",
         layout = "tree", edge.label.bg=TRUE)#,
text(0, 1.6, labels=paste(type, "-", y.variable))
dev.off()


################################################
## Contribution to nodf ----
################################################
load(file="~/Dropbox/speciesRoles_saved/contrNodf.Rdata") #contribution to nodf variabiity
y.variable <- "Nodf contribution"

contr.nodf <- traits.contr.nodf[traits.contr.nodf$GenusSpecies %in% type.bees, ]

## it is suggested to have a dataframe with just the variables you'll use.
vars <- c("GenusSpecies", "contr.nodf", "median.abund", "median.days", "r.degree", "Lecty")
contr.nodf <- contr.nodf[,vars]

## renaming variables beacuse lavvan renames them and they become unintelligible
contr.nodf <- contr.nodf %>%
  plyr::rename(c("GenusSpecies"="spp", "contr.nodf"="y", "median.abund"="abund", "median.days"="phen", "r.degree"="rdeg", "Lecty"="lec")) %>%
  group_by(spp)  %>%
  mutate(y = mean(y, na.rm=TRUE))

contr.nodf <- contr.nodf[!is.na(contr.nodf$y),]
contr.nodf <- as.data.frame(contr.nodf[!duplicated(contr.nodf),])

################################################
### Path analysis
#logging numerical variables
contr.nodf[,c("abund", "phen", "rdeg")] <-
    scale(log(contr.nodf[,c("abund", "phen", "rdeg")]))
contr.nodf.model <- lavaan::sem(path.model, data=contr.nodf)
summary(contr.nodf.model, standardized=TRUE, fit=TRUE, rsquare=TRUE)

#plotting and saving
pdf(file=file.path(fig.path,
                   sprintf("%s.pdf", paste("path_", type, "NodfVar", sep=""))), width=10, height= 8)
semPaths(contr.nodf.model, what = 'std', whatLabels='std',style = "lisrel",
         layout = "tree", edge.label.bg=TRUE)#,
text(0, 1.6, labels=paste(type, "-", y.variable))
dev.off()

save(partner.path.model, role.path.model, contr.nodf.model,
     file= sprintf('%s.Rdata', "pathResults"))

load("~/Dropbox/speciesRoles/analysis/variability/pathResults.Rdata")
summary(partner.path.model, standardized=TRUE, fit=TRUE, rsquare=TRUE)


#### getting the values of each analysis to put in the results
print("number of spp - partner")
length(unique(var.mean.trait$spp))
print("number of spp - role")
length(unique(role.path$spp))
print("number of spp - nodf")
length(unique(contr.nodf$spp))
