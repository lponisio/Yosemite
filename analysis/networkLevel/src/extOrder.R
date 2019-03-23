

getOrder <- function(i, sites, nets, ext.order){
    this.site <- sites[i]
    these.plants <- rownames(nets[[i]])
    this.order <- ext.order[[this.site]]
    this.order <- this.order[this.order %in% these.plants]
    if(length(this.order) == 0){
        return(order(rowSums(nets[[i]])))
    }
    rank.order <- match(these.plants, this.order)
    not.in.2013 <- is.na(rank.order)
    num.not.in.2013 <- sum(not.in.2013)
    if(num.not.in.2013 > 0){
        rank.order[not.in.2013] <- seq(max(rank.order, na.rm=TRUE) + 1,
                                       length.out=num.not.in.2013)
    }
    return(rank.order)
}

getLogAbund <- function(abund){
    abund.2013 <- abund[abund$Year == "2013",]
    abund.2014 <- abund[abund$Year == "2014",]

    abund.2013$abund.drought <-
        abund.2014$abund[match(paste0(abund.2013$Site, abund.2013$PlantGenusSpecies),
                               paste0(abund.2014$Site, abund.2014$PlantGenusSpecies))]

    abund.2013$abund.drought[is.na(abund.2013$abund.drought)] <- 0
    abund.2013$lrabund <- log(abund.2013$abund.drought +
                              1)/log(abund.2013$abund + 1)
    return(abund.2013)
}


getExtinctionOrder <- function(veg.visit.degree,## visit/veg/degree
                               by.abund, ## "abund" or "LR abund"
                               nets,
                               spec,
                               veg){
    ## function for simulating the extinction order of plants.
    ## if by visit, then abundaunce is calculated from networks,
    ## otherwise it is calculated from veg data.
    ## If by.abund = TRUE, the plants are ranked from least to most
    ## abundant in 2013. Otherwise they are ranked by their log ratio
    ## change in abundance between 2013 and 2014.
    ## order by difference in drought abund
    if(veg.visit.degree == "visit"){
        abund <- aggregate(list(abund=spec$PlantGenusSpecies),
                           list(Year=spec$Year,
                                Site=spec$Site,
                                PlantGenusSpecies=spec$PlantGenusSpecies),
                           length)
    } else if(veg.visit.degree == "veg"){
        abund <- aggregate(list(abund =veg$logFlowerNum),
                           list(Year=veg$Year,
                                Site=veg$Site,
                                PlantGenusSpecies=veg$PlantGenusSpecies),
                           mean)
    } else if(veg.visit.degree == "degree"){
        abund <- aggregate(list(abund=spec$GenusSpecies),
                           list(Year=spec$Year,
                                Site=spec$Site,
                                PlantGenusSpecies=spec$PlantGenusSpecies),
                           function(x)length(unique(x)))
    }
    abund.dats <- getLogAbund(abund)
    abund.dats <- split(abund.dats, abund.dats$Site)

    if(by.abund == "abund"){
        ord <- lapply(abund.dats, function(x){
            out <-  x[order(x$abund),]
            return(out)
        })
    } else if(by.abund == "LR abund"){
        ord <- lapply(abund.dats, function(x){
            x[is.na(x)] <- 0
            out <-  x[order(x$lrabund),]
            out[out$lrabund == 0,] <-
                out[out$lrabund == 0,][order(out$abund[out$lrabund == 0]),]
            return(out)
        })
    }
    gen.sp.order <- lapply(ord, function(x) x$PlantGenusSpecies)
    sites <- sapply(strsplit(names(nets),  "\\."), function(x) x[[1]])

    ext.rows <- lapply(1:length(nets), getOrder,
                       sites, nets, gen.sp.order)
    names(ext.rows) <- names(nets)
    return(ext.rows)
}
