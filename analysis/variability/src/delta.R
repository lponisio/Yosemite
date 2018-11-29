calcYearDiff <- function(spec.abund){
    ## create an empty matrix of all the sites and species
    empty.mat <- matrix(0, nrow=length(levels(spec.abund$Site)),
                        ncol=length(unique(spec.abund$GenusSpecies)))
    rownames(empty.mat) <- levels(spec.abund$Site)
    colnames(empty.mat) <- unique(spec.abund$GenusSpecies)

    abund.yr.one <- spec.abund[spec.abund$Year == "2013",]
    abund.yr.one <- samp2site.spp(abund.yr.one$Site,
                                  abund.yr.one$GenusSpecies,
                                  abund.yr.one$Abund)

    abund.yr.two <- spec.abund[spec.abund$Year == "2014",]
    abund.yr.two <- samp2site.spp(abund.yr.two$Site,
                                  abund.yr.two$GenusSpecies,
                                  abund.yr.two$Abund)
    year.one <- year.two <- empty.mat
    year.one <-
        abund.yr.one[match(rownames(year.one),
                           rownames(abund.yr.one)),
                     match(colnames(year.one),
                           colnames(abund.yr.one))]

    year.two <- abund.yr.two[match(rownames(year.two),
                                   rownames(abund.yr.two)),
                             match(colnames(year.two),
                                   colnames(abund.yr.two))]

    colnames(year.two) <- colnames(empty.mat)
    year.two[is.na(year.two)] <- 0
    year.one[is.na(year.one)] <- 0
    ## delta.year <- log(year.two + 1)/log(year.one + 1)
    delta.year <- year.one - year.two
    ## in sites where the species was never detected, NA
    delta.year[year.two == 0 & year.one == 0] <- NA
    delta <- convertMatrix2Sample(delta.year)
    return(delta)
}


prepDeltaVar <- function(delta, spec.abund, site.char){
    spec.abund <- spec.abund[spec.abund$Year == "2013",]
    delta.sp.char <- merge(delta, spec.abund)

    ## extrat the useful site variables
    site.data <- unique(data.frame(Site=site.char$Site,
                                   simpson.div=site.char$simpson.div,
                                   SiteStatus=site.char$SiteStatus,
                                   Year=site.char$Year,
                                   FloralRichness=site.char$FloralRichness,
                                   Richness=site.char$Richness))

    ## take the average across a year at a site for floral richness
    ## and pollinator richness
    site.data <- aggregate(list(FloralRichness=site.data$FloralRichness,
                                Richness=site.data$Richness),
                           list(Site=site.data$Site,
                                simpson.div=site.data$simpson.div,
                                SiteStatus=site.data$SiteStatus,
                                Year=site.data$Year),
                           mean)

    site.data.2013 <- site.data[site.data$Year == "2013",]
    site.data.2014 <- site.data[site.data$Year == "2014",]

    ## calculate the change in floral richness
    site.data.2013$deltaFloralRichness <-
        log(site.data.2014$FloralRichness)/log(site.data.2013$FloralRichness)

    delta.site.sp.char <- merge(delta.sp.char, site.data.2013)

    delta.site.sp.char <- merge(delta.site.sp.char, traits)
    delta.site.sp.char$SiteStatus <- factor(delta.site.sp.char$SiteStatus,
                               level=c("LOW", "MOD", "HIGH"))
    return(list(delta=delta.site.sp.char, site.data= site.data.2013))

}
