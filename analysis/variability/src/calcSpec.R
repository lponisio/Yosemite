library(fossil)

getSpec <- function(species.lev, names.net, seps="[.]"){
    ## extract specialization scores from specieslevel function and
    ## return data frame
    n.pp <- sapply(species.lev, nrow)
    pp <- c(unlist(sapply(species.lev, rownames)))
    names(pp) <- NULL
    all.pp <- do.call(rbind, species.lev)
    rownames(all.pp) <- NULL
    try(all.pp$GenusSpecies <- pp)
    all.pp$speciesType <- c(rep("pollinator", n.pp[1]),
                            rep("plant", n.pp[2]))
    all.pp$Site <- strsplit(names.net, seps)[[1]][1]
    all.pp$Date <- strsplit(names.net, seps)[[1]][2]
    all.pp$Year <- format(as.Date(all.pp$Date), "%Y")
    return(all.pp)
}


calcSpec <- function(nets, spec, dist.metric){
    ## applies specieslevel from bipartite to networks
    ## calculates the species roles from a network and returns a dataframe
    ## with site status and ypr
    ## takes networks and specimen data

    species.lev <- lapply(nets, function(x){
        sl <- specieslevel(x)
        sl$'higher level'$tot.int <- colSums(x)
        sl$'lower level'$tot.int <- rowSums(x)
        sl$'higher level'$mean.k <- mean(sl$'higher level'$degree)
        sl$'lower level'$mean.k <- mean(sl$'lower level'$degree)
        sl$'higher level'$sd.k <- sd(sl$'higher level'$degree)
        sl$'lower level'$sd.k <- sd(sl$'lower level'$degree)
        sl$'higher level'$k <- (sl$'higher level'$degree -
                                sl$'higher level'$mean.k)/sl$'higher level'$sd.k
        sl$'lower level'$k <- (sl$'lower level'$degree -
                               sl$'lower level'$mean.k)/sl$'lower level'$sd.k
        plants.niche.overlap <- as.matrix(vegdist(x,
                                                  method=dist.metric))
        diag(plants.niche.overlap) <- NA
        sl$'lower level'$niche.overlap <- apply(plants.niche.overlap, 1, mean,
                                                na.rm=TRUE)

        pol.niche.overlap <- as.matrix(vegdist(t(x),
                                               method=dist.metric))
        diag(pol.niche.overlap) <- NA
        sl$'higher level'$niche.overlap <- apply(pol.niche.overlap, 1, mean,
                                                 na.rm=TRUE)
        sl$'higher level'$rare.degree <- apply(x, 2, chao1)
        sl$'lower level'$rare.degree <- apply(x, 1, chao1)
        return(sl)
    })

    ## extract the values and make a dataframe
    specs  <-  mapply(function(a, b)
        getSpec(species.lev = a,
                names.net = b,
                seps="[.]"),
        a = species.lev,
        b = names(nets),
        SIMPLIFY = FALSE)

    specs <- do.call(rbind, specs)
    rownames(specs) <- NULL
    specs$SiteStatus <- spec$SiteStatus[match(specs$Site,
                                              spec$Site)]
    specs$SpSiteYear <- paste0(specs$GenusSpecies, specs$Site, specs$Year)
    return(specs)
}
