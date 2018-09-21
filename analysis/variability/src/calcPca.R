calcPcaMeanVar <- function(species.roles, var.method, metrics,
                           loadings, ave.method, traits, agg.col="Site", ...){
    ## to select spp in the same site / creating lists to store the results
    sites <- unique(species.roles[,agg.col])
    all.pca <- list()
    all.spp <- list()
    var.pca <- list()
    ## standardizing variables (zscore) and doing the pca
    for(y in 1:length(sites)){
        this.site <- species.roles[species.roles[,agg.col] == sites[y],]
        ## for each site, across years
        ## calculate z scores (xi - mean(x))/sd)
        zs <- apply(this.site[, metrics], 2,
                    function(x){
                        (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
                    })
        ## if all the scores are zero, cannot divide by zero, just set
        ## to zero
        all.na <- apply(zs, 2, function(x) all(is.na(x)))
        zs[, all.na] <- 0

        ## runs the pca
        all.pca[[y]] <- prcomp(zs)

        ## make a nice dataframe
        all.spp[[y]] <- this.site[, c("Site", "Year", "GenusSpecies", "sp.site.year")]
        names(all.pca)[y] <- sites[y]
        names(all.spp)[y] <- sites[y]
        all.spp[[y]] <- cbind(all.spp[[y]],
                              pca=all.pca[[y]]$x[,loadings])

        ## calculate the variance of the pc1 scores within a site
        ## across years
        var.pca[[y]] <- tapply(all.spp[[y]]$pca,
                               all.spp[[y]]$GenusSpecies, var.method,
                               ...)

        names(var.pca)[y] <- sites[y]
    }
    ## create a dataframe for the variance for pc1 values per site
    pca.var <- data.frame(var.pca1 = do.call(c, var.pca))
    pca.var$Site <- sapply(strsplit(rownames(pca.var), "[.]"),
                           function(x) x[1])
    pca.var$SiteStatus <- species.roles$SiteStatus[match(pca.var$Site, species.roles$Site)]
    pca.var$GenusSpecies <- sapply(strsplit(rownames(pca.var), "[.]"),
                                   function(x) x[2])
    pca.var <- merge(pca.var, traits, by="GenusSpecies")

    ## calculate a mean across all sites for each species for to look for
    ## phylogenetic signal
    pca.mean <- do.call(rbind, all.spp)
    pca.mean <- aggregate(pca.mean$pca,
                          by = list(GenusSpecies = pca.mean$GenusSpecies, 
                                    Site = pca.mean$Site),
                          FUN=mean, na.rm=TRUE)
  
    return(list(pca.mean=pca.mean, pca.var=pca.var))
}
