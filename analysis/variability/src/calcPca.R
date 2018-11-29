calcPcaMeanVar <- function(species.roles, var.method, metrics,
                           loadings, ave.method, agg.col="Year", ...){
    ## to select spp in the same site / creating lists to store the results
    sites <- unique(species.roles[,agg.col])
    all.pca <- list()
    all.spp <- list()
    var.pca <- list()
    ## standardizing variables (zscore) and doing the pca
    for(y in 1:length(sites)){
        this.site <- species.roles[species.roles[,agg.col] == sites[y],]
        this.site <- this.site[!apply(this.site[,metrics], 1, function(x) any(is.na(x))),]
        ## for each site, across years
        ## calculate z scores (xi - mean(x))/sd)
        zs <- apply(this.site[, metrics], 2,
                    function(x){
                        (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
                    })
        ## if all the scores are zero, cannot divide by zero, just set
        ## to zero
        all.na <- apply(zs, 2, function(x) all(!is.finite(x)))
        zs[, all.na] <- 0

        ## runs the pca
        all.pca[[y]] <- prcomp(zs)
        ## make a nice dataframe
        all.spp[[y]] <- this.site[, c("Site", "Year", "GenusSpecies", "SpSiteYear")]
        names(all.pca)[y] <- sites[y]
        names(all.spp)[y] <- sites[y]
        all.spp[[y]] <- cbind(all.spp[[y]],
                              pca=all.pca[[y]]$x[,loadings])

        ## calculate the variance of the pc1 scores within a site
        ## across years
        var.pca[[y]] <- tapply(all.spp[[y]]$pca,
                               all.spp[[y]]$GenusSpecies, var.method,...)

        names(var.pca)[y] <- sites[y]
    }
    ## create a dataframe for the variance for pc1 values per site
    genus.sp <- sapply(var.pca, names)
    pca.var <- data.frame(var.pca1 = do.call(c, var.pca))
    pca.var$GenusSpecies <- unlist(genus.sp)
    if(agg.col == "Site"){
        pca.var$Site <- sapply(strsplit(rownames(pca.var), "[.]"),
                               function(x) x[1])
        pca.var$SiteStatus <- species.roles$SiteStatus[match(pca.var$Site, species.roles$Site)]
    }
    if(agg.col == "Year"){
        pca.var$Year <- sapply(strsplit(rownames(pca.var), "[.]"),
                               function(x) x[1])
    }
    pca.mean <- do.call(rbind, all.spp)
    pca.mean <- aggregate(list(mean.pca1=pca.mean$pca),
                          by = list(GenusSpecies = pca.mean$GenusSpecies,
                                    Site = pca.mean$Site,
                                    Year = pca.mean$Year),
                          FUN=mean, na.rm=TRUE)
    rownames(pca.mean) <- NULL
    rownames(pca.var) <- NULL

    return(list(pca.mean=pca.mean, pca.var=pca.var, pca.loadings = all.pca))
}
