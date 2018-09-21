
make.struct <- function(spec.dat, type, method= "all"){
  ## prep site by species matrix
  prep.comm <- aggregate(spec.dat[, type],
                         list(site= spec.dat$Site,
                              year= spec.dat$Year,
                              ## status= spec.dat$SiteStatus,
                              sp= spec.dat[, type]), length)
  
    prep.comm$site_yr <- paste(prep.comm$site, prep.comm$year, sep= '_')
    comm <-  samp2site.spp(site= prep.comm$site_yr,
                           spp= prep.comm$sp, abund=
                           prep.comm$x)
    sites <- as.factor(sapply(strsplit(rownames(comm), '_'),
                              function(x) x[1]))
    years <- as.factor(sapply(strsplit(rownames(comm), '_'),
                              function(x) x[2]))
    status <- spec.dat$SiteStatus[match(rownames(comm),
                                        paste(spec.dat$Site,
                                              spec.dat$Year,
                                              sep="_"))]
    byYr <- split(prep.comm, prep.comm$year)
    comm <- lapply(byYr, function(y) {
      samp2site.spp(y$site, y$sp, y$x)
    })
    comm <- comm[sapply(comm, nrow) > 1]
    comm <- lapply(comm, empty)
    sites <- lapply(comm, rownames)
    nsite <- lapply(sites, length)
    years <- rep(names(comm), nsite)
    site.year <- vector("list", length(sites))
    for(i in 1:length(sites)){
      site.year[[i]] <- paste(sites[[i]], names(comm)[i], sep="_")
    }

      status <- lapply(site.year , function(x){
        factor(spec.dat$SiteStatus[match(x, paste(spec.dat$Site,
                                                  spec.dat$Year,
                                                  sep="_"))],
               levels= c('LOW', 'MOD', 'HIGH'))
      })
  return(list(comm=comm,
              years=years,
              sites=sites,
              status=status))
}
