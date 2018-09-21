## site by species matrix
samp2site.spp <- function(site,spp,abund) { 
  x <- tapply(abund, list(site=site,spp=spp), sum)
  x[is.na(x)] <- 0
  
  return(x)
}


## occurrence matrix
comm.mat2sample <-  function (z) {
  temp <- data.frame(expand.grid(dimnames(z))[1:2],
                     as.vector(as.matrix(z)))
  temp <- temp[sort.list(temp[, 1]), ]
  data.frame(Site = temp[, 1], Abund = temp[, 3],
             GenusSpecies = temp[, 2])
}

## function to clean up white-space in a column of data (replaces all
## instances of white-space with " " and empty cells with ""
fix.white.space <- function(d) {
  d <- as.character(d)
  remove.first <- function(s) substr(s, 2, nchar(s))
  d <- gsub("      ", " ", d, fixed=TRUE)
  d <- gsub("     ", " ", d, fixed=TRUE)
  d <- gsub("    ", " ", d, fixed=TRUE)
  d <- gsub("   ", " ", d, fixed=TRUE)
  d <- gsub("  ", " ", d, fixed=TRUE)
  
  tmp <- strsplit(as.character(d), " ")
  d <- sapply(tmp, function(x) paste(x, collapse=" "))

  first <- substr(d, 1, 1)
  d[first==" "] <- remove.first(d[first==" "])
  d
}

## calculate site-level statistics
calc.site.level <- function(dats, abund.type="sum"){
  rich <- length(unique(dats))
  dats <- as.character(dats)
  div <-  diversity(table(dats), index="simpson")
  if(abund.type == "median"){
    abund <- median(table(dats))
  } else {
    abund <- length(dats)
  }
  return(c(rich, abund, div))
}

## return sorted unique values
id <- function(x) unique(sort(x))


## abundance of each species for all site-date combinations
make.by.species <- function(spec, sr.sched,
                            path.dir,
                            type="GenusSpecies"){
  ## number of species at each site-date
  all.sp <- aggregate(list(Abund=spec[, type]),
                      list(Site=spec$Site,
                           Date=spec$Date,
                           GenusSpecies=spec[,type]),
                      length)
  ## all site-date species combinations
  sp <- expand.grid(
    SiteDate = unique(paste(sr.sched$Site,
      sr.sched$Date,
      sep=';')),
    GenusSpecies=unique(spec[,type]),
    Abund=0)

  ## fill in the matrix of all combinations
  sp$Abund[match(paste(all.sp$Site,
                       all.sp$Date,
                       all.sp$GenusSpecies,
                       sep=';'),
                 paste(sp$SiteDate,
                       sp$GenusSpecies,
                       sep=';'))] <-
                         all.sp$Abund
  ## occurrence
  sp$Occ <- sp$Abund
  sp$Occ[sp$Occ > 1] <- 1
  ## create site, date, genus etc. columns
  sp$Site <- sapply(strsplit(as.character(sp$SiteDate), ";"),
                    function(x) x[1])
  sp$Date <- as.Date(sapply(strsplit(as.character(sp$SiteDate), ";"),
                            function(x) x[2]))
  sp$Genus <- sapply(strsplit(as.character(sp$GenusSpecies), " "),
                     function(x) x[1])
  sp$Year <- format(sp$Date, '%Y')
  sp$SiteStatus <- spec$SiteStatus[match(sp$Site,
                                         spec$Site)]
  sp$SiteStatus <- factor(sp$SiteStatus,
                          levels=c("LOW", "MOD", "HIGH"))
  sp$FirePerim <- spec$FirePerim[match(sp$Site,
                                       spec$Site)]
  traits.2.keep <- c(keep.trait, "PolSpec", "YoseSpec")
  sp <- cbind(sp, spec[, traits.2.keep][match(paste(sp$GenusSpecies),
                                              paste(spec[,type])),])
  apis <- sp[sp$GenusSpecies == "Apis mellifera",]
  ## create honeybee abundance column
  sp$HBabund <- apis$Abund[match(
    sp$SiteDate,
    apis$SiteDate)]
  ## doy columns
  sp$doy <- as.numeric(strftime(sp$Date, format="%j"))
  sp$Site <- as.factor(sp$Site)
  ## drop site-date column
  sp <- sp[,-1]

  ## make a site by date by species arrary
  d <- data.frame(pollinator=spec[,type],
                  site=spec$Site,
                  date=spec$Date)
  null.mat <- tapply(rep(0, nrow(sr.sched)),
                     list(sites=paste(sr.sched$Site),
                          dates=sr.sched$Date), sum)
  pollinator.id <- id(d$pollinator)
  mats <- make.mats(pollinator.id,
                    null.mat,
                    pollinator=as.vector(d$pollinator), 
                    var1=as.vector(d$site),
                    var2=d$date)
  sites <- rownames(mats[[1]])
  dates <- colnames(mats[[1]])
  species <- names(mats)
  mat <- array(unlist(mats),
               dim=c(dim(mats[[1]]),
                 length(mats)))
  dimnames(mat) <- list(site=sites,
                        date=dates,
                        species=species)

  write.csv(sp, file=file.path(path.dir, 'sp.csv'),
            row.names=FALSE)
  save(sp, file= file.path(path.dir, 'sp.Rdata'))
  save(mat, file= file.path(path.dir, 'sp_array.Rdata'))
}


## function to make pollinator visitation matrices
make.mats <- function(pollinator.id,
                      null.mat,
                      pollinator,
                      var1, var2) {
  make.mat <- function(P) {
    var1 <- var1[pollinator==P]
    var2 <- var2[pollinator==P]
    m <- tapply(rep(1, length(var1)),
                list(sites=var1, dates=var2), sum)

    null.mat[rownames(m), colnames(m)][!is.na(m)] <- m[!is.na(m)]
    null.mat
  }
  mats <- lapply(pollinator.id, function(x) make.mat(x))
  names(mats) <- pollinator.id
  mats
}


## calculate and match d prime values


match.d <- function(prep.comm, col.name, col.name.plant, spec, traits){
  prep.comm <- prep.comm[prep.comm$PlantGenusSpecies != '',]
  comm <- samp2site.spp(prep.comm$PlantGenusSpecies,
                        prep.comm$PolGenusSpecies, prep.comm$x)

  d <- specieslevel(comm, index='d')
  spec[, col.name] <- d$'higher level'$'d'[match(spec$GenusSpecies,
                                                 rownames(d$'higher level'))]
  spec[, col.name.plant] <- d$'lower level'$'d'[match(spec$PlantGenusSpecies,
                                                 rownames(d$'lower level'))]
  return(spec)
}
