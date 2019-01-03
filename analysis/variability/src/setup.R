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
                                 ## Doy=spec$Doy,
                                 Year=spec$Year,
                                 SiteStatus=spec$SiteStatus),
                            length)

    ## calculate orthoganol polynomials for doy
    ## spec.abund$DoyPoly <- poly(spec.abund$Doy, degree=2)
    ## spec.abund$DoyPoly1 <- spec.abund$DoyPoly[,'1']
    ## spec.abund$DoyPoly2 <- spec.abund$DoyPoly[,'2']
    ## spec.abund$DoyPoly <- NULL
    spec.abund$SiteStatus <- factor(spec.abund$SiteStatus,
                                    levels=c("LOW", "MOD", "HIGH"))
    return(spec.abund)
}


getScorePrepDrought <- function(pol.pca.scores, beta.dist, spec.abund,
                                var.method=cv){

    ## match pca var and pca mean scores from 2013 (for var)
    pca.var <- pol.pca.scores$pca.var[pol.pca.scores$pca.var$Year ==
                                      "2013",]
    pca.var$Year <- NULL
    pca.mean <- pol.pca.scores$pca.mean[pol.pca.scores$pca.mean$Year ==
                                        "2013",]
    pca.mean$Year <- NULL
    spec.abund <- merge(spec.abund, pca.var)
    spec.abund <- merge(spec.abund, pca.mean)

    ## add partner varaibility data from 2013
    mean.beta.dist <- tapply(beta.dist$dist[beta.dist$Year == "2013"],
                             beta.dist$GenusSpecies[beta.dist$Year == "2013"],
                             var.method)
    ## mean.beta.dist <- sapply(mean.beta.dist, median, na.rm=TRUE)
    spec.abund$beta.dist <- mean.beta.dist[match(spec.abund$GenusSpecies,
                                                 names(mean.beta.dist))]
    return(spec.abund)
}
