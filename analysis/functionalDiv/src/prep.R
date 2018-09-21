## prepare data
## bee.all is either "all" or "bees"
prep.dat <- function(sites,
                     bee.all,
                     drop.na,
                     drop.parasites,
                     cts.traits,
                     cat.traits,
                     occ.abun,
                     net.combined) {
  ## load [site x date x species] matrix
  data.dir <- '~/Dropbox/Yosemite/analysis/data'
  load(sprintf('%s/matrices/%s/%s/sp_array.RData',
               data.dir,
               net.combined,
               bee.all))
  date <- dimnames(mat)[['date']]
  year <- format(as.Date(date, format='%Y-%m-%d'), format='%Y')

  if(bee.all=='bee') {
    if(drop.parasites) para.dir <- 'parasites'
    if(!drop.parasites) para.dir <- 'no_parasites'
    save.dir <- file.path('analysis/functionalDiv/saved',
                          bee.all,
                          occ.abun,
                          para.dir)
  }
  if(bee.all=='all') {
    save.dir <- file.path('analysis/functionalDiv/saved',
                          bee.all,
                          occ.abun)
  }

  ## drop sites or samples to make sample number consistent within
  ## each year
  ##
  ## 2014
  mat['L21',         '2014-07-01',] <- NA
  focal.sites <- site.list(sites)

  mat[!(dimnames(mat)$site %in% names(focal.sites$sites)),,] <- NA
  for(site in names(focal.sites$sites))
    mat[site,!(year %in% focal.sites$sites[[site]]),] <- NA
  ## load functional traits
  if(bee.all != "interactions"){
    fn.traits <- sprintf('%s/functionalTraits/%s.Rdata',
                         data.dir, bee.all)
    load(fn.traits)
    
    
    ## drop parasitic species
    if(drop.parasites)
      traits <- traits[traits$Sociality!='parasite',]
    
    ## drop unwanted columns
    rownames(traits) <- traits$GenusSpecies
    traits <- traits[,c(cts.traits, cat.traits), drop=FALSE]

    ## standardize cts traits
    ## for(trait in cts.traits)
    ##   traits[,trait] <- standardize(traits[,trait])
    
    ## drop species with missing trait values
    if(drop.na) {
      keep <- apply(traits, 1, function(x) !any(is.na(x)))
      traits <- traits[keep,,drop=FALSE]
    }

    ## subset matrix and traits file down to just species in both
    in.both <- sort(intersect(dimnames(mat)[['species']],
                              rownames(traits)))

    cat(sprintf('%d species dropped due to missing trait data\n',
                sum(!dimnames(mat)[['species']] %in% in.both)))
    
    mat <- mat[,,match(in.both, dimnames(mat)[['species']])]
    traits <- traits[match(in.both, rownames(traits)),]
  } else{
    traits <- NA
    cts.traits <- NA
    cat.traits <- NA
  }

  ## create [site x year x species] matrix
  by.year <- sapply(id(year), function(x)
                    apply(mat[,year==x,], c(1,3), sum, na.rm=TRUE),
                    simplify='array')
  names(dimnames(by.year))[3] <- 'year' 
  sampled <- sapply(id(year), function(x)
                    apply(mat[,year==x,], c(1,3),
                          function(x) any(!is.na(x))),
                    simplify='array')
  by.year[!sampled] <- NA
  mat.y <- aperm(by.year, c(1,3,2))


  ## by.date <- sapply(id(date), function(x)
  ##                   apply(mat[,date==x,], c(1,2), sum, na.rm=TRUE),
  ##                   simplify='array')
  ## names(dimnames(by.date))[3] <- 'date' 
  ## sampled <- sapply(id(date), function(x)
  ##                   apply(mat[,date==x,], c(1,2),
  ##                         function(x) any(!is.na(x))),
  ##                   simplify='array')
  ## by.date[!sampled] <- NA
  ## mat.y <- aperm(by.date, c(1,3,2))

  ## switch to occurrence
  if(occ.abun=='occ') {
    mat[mat > 0] <- 1
    mat.y[mat.y > 0] <- 1
  }

  ## drop species never observed and sites never sampled
  site.sampled <- apply(mat, 1, sum, na.rm=TRUE) > 0
  sp.present   <- apply(mat, 3, sum, na.rm=TRUE) > 0 
  mat <- mat[site.sampled,,sp.present]
  mat.y <- mat.y[site.sampled,,sp.present]
  if(!all(is.na(traits)))  traits <- traits[sp.present,]

  ## create matrix with the number of samples for each site per year
  num.samples <- sapply(id(year), function(x)
                        apply(mat[,year==x,], c(1,3),
                              function(x) sum(!is.na(x))),
                        simplify='array')[,1,]
  names(dimnames(num.samples))[2] <- 'year'

  load(sprintf('%s/specimens/spec.RData', data.dir))
  status <- spec$SiteStatus[match(dimnames(mat)$site,
                                  spec$Site)]
  
  dd.model <- list(mat.d=mat,
                   mat.y=mat.y,
                   num.samples=num.samples,
                   dd.traits=as.data.frame(traits),
                   dd.statuses = status,
                   traits=list(cts=cts.traits, cat=cat.traits))
  dd.model
}
