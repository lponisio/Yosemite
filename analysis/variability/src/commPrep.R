
calcSiteBeta <- function(x, species.type, spec, species.type.int,
                         date.cut.off = 3,
                         observation.cut.off = 3){

    this.spec <- spec[spec$Year == x,]
    prep.comm <- aggregate(list(Abund=this.spec[, species.type]),
                           list(GenusSpecies=this.spec[, species.type],
                                InterGenusSpecies=this.spec[, species.type.int],
                                Site=this.spec$Site,
                                Date=this.spec$Date),
                           length)
    prep.comm$SiteDate <- paste(prep.comm$Site, prep.comm$Date,
                                sep=":")
    prep.comm <- prep.comm[!prep.comm$InterGenusSpecies == "",]
    by.species <- split(prep.comm, prep.comm$GenusSpecies)

    num.observations <- sapply(by.species, function(x) sum(x$Abund))
    ## subset to species seen in > 3 years and at least 5 times
    ## (defaults)
    by.species <- by.species[num.observations >= observation.cut.off]
    num.dates <- sapply(by.species, function(x) length(unique(x$SiteDate)))
    by.species <- by.species[num.dates >= date.cut.off]
    by.species <- by.species[!sapply(by.species, is.null)]

    ## year plant combinations
    empty.matrix <- matrix(0, nrow=length(unique(prep.comm$SiteDate)),
                           ncol=length(unique(prep.comm$InterGenusSpecies)))

    rownames(empty.matrix) <- sort(unique(prep.comm$SiteDate))
    colnames(empty.matrix) <-
        sort(unique(prep.comm$InterGenusSpecies))

    if(length(by.species) != 0){
        comm <- vector("list", length=length(by.species))
        for(i in 1:length(by.species)){
            comm[[i]] <- empty.matrix
            this.by.species <- by.species[[i]]
            for(j in 1:nrow(this.by.species)){
                this.row <- this.by.species[j,]
                comm[[i]][match(this.row["SiteDate"], rownames(comm[[i]])),
                          match(this.row["InterGenusSpecies"],colnames(comm[[i]]))] <-
                    as.numeric(this.row[["Abund"]])
            }
            comm[[i]] <- comm[[i]][rowSums(comm[[i]]) > 0,]
            ## if(!is.matrix(comm[[i]])) comm[[i]] <- NA
        }
        names(comm) <- names(by.species)
        ## comm <- comm[!sapply(comm, function(x) all(is.na(x)))]
        return(list(comm=comm, year=x))
    }
}

makePretty <- function(comms, spec){
    year <- sapply(comms, function(x) x$year)
    comms <- lapply(comms, function(x) x$comm)
    comms <- comms[!sapply(year, is.null)]
    year <- unlist(year[!sapply(year, is.null)])
    names(comms) <- year
    site.date <- lapply(comms, function(x) sapply(x, rownames))

    comm.pp <- list(comm=comms,
                    site.date=site.date,
                    years= rep(names(comms),
                               sapply(comms, length)))
    return(comm.pp)
}
