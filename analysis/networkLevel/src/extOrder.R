

getOrder <- function(i, sites, nets, ext.by.site){
    this.site <- sites[i]
    these.plants <- rownames(nets[[i]])
    this.order <- ext.by.site[[this.site]]
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

getExtinctionOrder <- function(by.degree, ##T/F
                               nets,
                               spec, ...){
    if(by.degree){
        ## order by degree
        ext.rows <- lapply(nets, function(x){
            plant.degree <- rowSums(x)
            ext.row <- order(plant.degree,
                             ...)
            return(ext.row)
        })
    } else{
        ## order by difference in drought abund
        abund <- aggregate(list(abund=spec$PlantGenusSpecies),
                           list(Year=spec$Year,
                                Site=spec$Site,
                                GenusSpecies=spec$PlantGenusSpecies),
                           length)

        abund.2013 <- abund[abund$Year == "2013",]
        abund.2014 <- abund[abund$Year == "2014",]

        abund.2013$abund.drought <-
            abund.2014$abund[match(paste0(abund.2013$Site,
                                          abund.2013$GenusSpecies),
                                   paste0(abund.2014$Site,
                                          abund.2014$GenusSpecies))]
        abund.2013$abund.drought[is.na(abund.2013$abund.drought)] <- 0
        abund.2013$lrabund <- log(abund.2013$abund.drought +
                                  1)/log(abund.2013$abund + 1)


        by.site <- split(abund.2013, abund.2013$Site)
        sites <- sapply(strsplit(names(nets),  "\\."), function(x)
            x[[1]])

        by.site <- lapply(by.site, function(x){
            out <-  x[order(x$lrabund),]
            out[out$lrabund == 0,] <-
                out[out$lrabund == 0,][order(out$abund[out$lrabund == 0]),]
            return(out)
        })

        ext.by.site <- lapply(by.site, function(x) x$GenusSpecies)
        sites <- sapply(strsplit(names(nets),  "\\."), function(x) x[[1]])

        ext.rows <- lapply(1:length(nets), getOrder,
                           sites, nets, ext.by.site)
        names(ext.rows) <- names(nets)
    }
    return(ext.rows)
}
