rm(list=ls())
setwd('~/Dropbox/yosemite/analysis/nestedness')
binary <- TRUE
alpha <- FALSE

src.dir <- '../beta-div/saved'
source('src/initialize.R')
## ************************************************************
## nestedness of each year
## ************************************************************
## ************************************************************
## across statuses bees
## ************************************************************
nodf.bee <- sapply(comm.bee$comm, FUN=function(data){
  nestednodf(data, weighted=weighted)$statistic["NODF"]
})

nodf.nulls.bee <- rapply(nulls.bee, f=function(data){
  nestednodf(data, weighted=weighted)$statistic["NODF"]
}, how="replace")

nodf.nulls.bee <- lapply(nodf.nulls.bee, unlist)

z.bee <- mapply(function(a, b)
                zvals(stat= a,
                      null.stats= b),
                a=nodf.bee,
                b=nodf.nulls.bee)

## almost sig. in 2013
p.bee <- mapply(function(a, b)
                pvals(stat= a,
                      null.stats= b),
                a=nodf.bee,
                b=nodf.nulls.bee)


## ************************************************************
## within a status bees
## ************************************************************

LOW <- calc.by.status(comm.bee$comm,
                      nulls.bee,
                      comm.bee$status,
                      "LOW", weighted)
save(LOW, file="saved/LOW_indiv.Rdata")

MOD <- calc.by.status(comm.bee$comm,
                      nulls.bee,
                      comm.bee$status,
                      "MOD", weighted)
save(MOD, file="saved/MOD_indiv.Rdata")

HIGH <- calc.by.status(comm.bee$comm,
                       nulls.bee,
                       comm.bee$status,
                       "HIGH", weighted)
save(HIGH, file="saved/HIGH_indiv.Rdata")


## ************************************************************
## across statuses flowers
## ************************************************************
nodf.flower <- sapply(comm.flower$comm, FUN=function(data){
  nestednodf(data, weighted=weighted)$statistic["NODF"]
})

nodf.nulls.flower <- rapply(nulls.flower, f=function(data){
  nestednodf(data, weighted=weighted)$statistic["NODF"]
}, how="replace")

nodf.nulls.flower <- lapply(nodf.nulls.flower, unlist)

z.flower <- mapply(function(a, b)
                   zvals(stat= a,
                         null.stats= b),
                   a=nodf.flower,
                   b=nodf.nulls.flower)

## not sig. nested
p.flower <- mapply(function(a, b)
                   pvals(stat= a,
                         null.stats= b),
                   a=nodf.flower,
                   b=nodf.nulls.flower)

## ************************************************************
## within a status flowers
## ************************************************************

LOW.flower <- calc.by.status(comm.flower$comm,
                             nulls.flower,
                             comm.flower$status,
                             "LOW", weighted)
save(LOW.flower, file="saved/LOW_indiv_flower.Rdata")

MOD.flower <- calc.by.status(comm.flower$comm,
                             nulls.flower,
                             comm.flower$status,
                             "MOD", weighted)
save(MOD.flower, file="saved/MOD_indiv_flower.Rdata")

HIGH.flower <- calc.by.status(comm.flower$comm,
                              nulls.flower,
                              comm.flower$status,
                              "HIGH", weighted)
save(HIGH.flower, file="saved/HIGH_indiv_flower.Rdata")



## write.table(t(round(HIGH, 3)), sep=" & ", row.names=FALSE,
##               file="~/Dropbox/hedgerow_network/beta-div-ms/tables/nest_HIGH.txt")
##   write.table(t(round(MOD, 3)), sep=" & ", row.names=FALSE,
##               file="~/Dropbox/hedgerow_network/beta-div-ms/tables/nest_MOD.txt")
##   write.table(t(round(LOW, 3)), sep=" & ", row.names=FALSE,
##               file="~/Dropbox/hedgerow_network/beta-div-ms/tables/nest_LOWs.txt")
