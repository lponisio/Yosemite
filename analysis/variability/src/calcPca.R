calcNetworkPca <- function(y, species.roles, agg.col,
                           ave.method, var.method, loadings){
    ## this function calculates a pca of network roles, then takes the
    ## mean, and var (methods passed in as arguments) for each
    ## species. It returns the mean and variable for each species, as
    ## well as the pca loadings
    this.site <- species.roles[species.roles[,agg.col] == y,]
    this.site <- this.site[!apply(this.site[,metrics], 1,
                                  function(x) any(is.na(x))),]
    mets.only <- this.site[, metrics]
    ## runs the pca
    all.pca <- prcomp(mets.only, scale. = TRUE, center = TRUE)
    ## make a nice dataframe
    all.spp <- this.site[, c("Site", "Year",
                             "GenusSpecies",
                             "SpSiteYear")]
    all.spp <- cbind(all.spp,
                     pca=all.pca$x[,loadings])
    ## calculate the variance/mean of the pcX scores
    ## variance is across all sites/samplings rounds
    pca.var <- tapply(all.spp$pca, all.spp$GenusSpecies,
                      var.method)
    ## mean is site-specific
    pcas <- aggregate(list(mean.pca1= all.spp$pca),
                      by = list(GenusSpecies = all.spp$GenusSpecies,
                                Site = all.spp$Site,
                                Year = all.spp$Year),
                      FUN=ave.method, na.rm=TRUE)
    pcas$var.pca1 <- pca.var[match(pcas$GenusSpecies,
                                   names(pca.var))]
    return(list(pcas=pcas,
                pca.loadings = all.pca))
}

calcPcaMeanVar <- function(species.roles, var.method, metrics,
                           loadings, ave.method, agg.col="Year"){
    ## this function applies over the "sites" (which can be with sites
    ## for taking the mean/variance of PCx scores across years within
    ## a site, or years for taking the mean/varaince of PCx scores
    ## across sites within a year
    sites <- unique(species.roles[,agg.col])
    out.pca <- lapply(sites, calcNetworkPca,
                      species.roles, agg.col,
                      ave.method, var.method, loadings)
    names(out.pca) <- sites
    return(out.pca)
}


