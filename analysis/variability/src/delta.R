calcDelta <- function(yr, spec.abund){
    print(yr)
    ## sites sampled both years
    sites.yr1 <- unique(spec.abund$Site[spec.abund$Year == yr])
    sites.yr2 <- unique(spec.abund$Site[spec.abund$Year == yr + 1])
    sites.both.yrs <- sites.yr1[sites.yr1 %in% sites.yr2]

    ## species found in the first year
    sp.yr1 <- unique(spec.abund$GenusSpecies[spec.abund$Year == yr &
                                 spec.abund$Site %in% sites.both.yrs])

    empty.mat <- matrix(0, nrow=length(sites.both.yrs),
                        ncol=length(sp.yr1))

    rownames(empty.mat) <- sites.both.yrs
    colnames(empty.mat) <- unique(sp.yr1)

    ## create a community matrix of species abundances by site for yr1
    ## and yr2 to ensure alignment for log ratio
    abund.yr.one <- spec.abund[spec.abund$Year == yr,]
    abund.yr.one <- samp2site.spp(abund.yr.one$Site,
                                  abund.yr.one$GenusSpecies,
                                  abund.yr.one$Abund)

    abund.yr.two <- spec.abund[spec.abund$Year == (yr + 1),]
    abund.yr.two <- samp2site.spp(abund.yr.two$Site,
                                  abund.yr.two$GenusSpecies,
                                  abund.yr.two$Abund)

    ## fill in matrix
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
    ## fix naming of species not found in year to that were found in
    ## year 1
    colnames(year.two) <- colnames(empty.mat)
    year.two[is.na(year.two)] <- 0
    year.one[is.na(year.one)] <- 0

    ## calculate the log ratio of abundances
    delta.year <- log(year.two + 1)/log(year.one + 1)
    ## in sites where the species was never detected, NA
    delta.year[year.two == 0 & year.one == 0] <- NA
    delta <- convertMatrix2Sample(delta.year)
    delta$Years <- paste0(yr, "-", yr +1)
    return(delta)
}



calcYearDiff <- function(spec.abund){
    ## function for calculating the log ratio of pollinator abundance
    ## between any arbitrary number of years
    years <- as.numeric(as.character(unique(spec.abund$Year)))
    years <- years[-length(years)]
    deltas <- lapply(years, calcDelta, spec.abund=spec.abund)
    names(deltas) <- years
    return(deltas)
}

prepDeltaVar <- function(delta, spec.abund, site.char, veg){
    ## function to prep data to be have the varaibles of interest for
    ## the statistical models
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
    ## floral richness delta
    site.data.2013 <- site.data[site.data$Year == "2013",]
    site.data.2014 <- site.data[site.data$Year == "2014",]
    ## calculate the change in floral richness
    site.data.2013$deltaFloralRichness <-
        log(site.data.2014$FloralRichness)/log(site.data.2013$FloralRichness)
    delta.site.sp.char <- merge(delta.sp.char, site.data.2013)

    ## floral abundance delta
    veg.ave <- aggregate(list(logFlowerNum=veg$logFlowerNum),
                         list(Site=veg$Site,
                              Year=veg$Year),
                         mean)

    veg.2013 <- veg.ave[veg.ave$Year == "2013",]
    veg.2014 <- veg.ave[veg.ave$Year == "2014",]
    ## calculate the change in floral richness
    veg.2013$deltaFloralAbund <-
        veg.2014$logFlowerNum/veg.2013$logFlowerNum
    delta.site.sp.char <- merge(delta.site.sp.char, veg.2013)
    ## delta.site.sp.char <- merge(delta.site.sp.char, traits)
    delta.site.sp.char$SiteStatus <- factor(delta.site.sp.char$SiteStatus,
                                            level=c("LOW", "MOD", "HIGH"))
    return(list(delta=delta.site.sp.char, site.data= site.data.2013))

}
