zscore <- function(x){
    y <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
    return(y)
}

calcSpecABund <- function(spec){
    ## format spec data and subset to net data
    spec$Doy <- as.numeric(strftime(spec$Date, format = "%j"))
    spec <- spec[spec$NetPan =="net",]

    ## calculate abundances for each species at each sample data
    spec.abund <- aggregate(list(Abund=spec$GenusSpecies),
                            list(GenusSpecies=spec$GenusSpecies,
                                 Site=spec$Site,
                                 Year=spec$Year,
                                 SiteStatus=spec$SiteStatus),
                            length)

    spec.abund$SiteStatus <- factor(spec.abund$SiteStatus,
                                    levels=c("LOW", "MOD", "HIGH"))
    return(spec.abund)
}


getScorePrepDrought <- function(pol.pca.scores,
                                var.beta.dist,
                                spec.abund){
    ## selects pre drought pca and beta dist scores (from 2013) and
    ## add to data from the objects outputted by 2partner.R and
    ## 3role.R.
    pcas.2013 <- pol.pca.scores$'2013'$pcas
    spec.abund <- merge(spec.abund, pcas.2013, all.x=TRUE)

    ## add partner varaibility data from 2013
    spec.abund$beta.dist <- var.beta.dist[match(spec.abund$GenusSpecies,
                                                 names(var.beta.dist))]
    return(spec.abund)
}
