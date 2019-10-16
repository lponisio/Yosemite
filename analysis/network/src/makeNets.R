
makeFilledNets <- function(spec, nets){
    split.pol <- split(spec, spec$GenusSpecies)
    all.plant <- lapply(split.pol, function(x) unique(x$PlantGenusSpecies))

    filled.nets <- lapply(nets, function(y){
        y[y > 1] <- 1
        for(sp in colnames(y)){
            this.int <- y[,sp]
            this.ref <- all.plant[[sp]]
            in.ref <- names(this.int) %in% this.ref
            y[,sp][in.ref] <- 1
        }
        return(y)
    })
    return(filled.nets)
}


makeInBothNets <- function(spec, nets){
    plants.2013 <- lapply(unique(spec$Site), function(x){
        unique(spec$PlantGenusSpecies[spec$Year == "2013" & spec$Site == x])
    })
    names(plants.2013) <- unique(spec$Site)


    plants.2014 <- lapply(unique(spec$Site), function(x){
        unique(spec$PlantGenusSpecies[spec$Year == "2014" & spec$Site == x])
    })
    names(plants.2014) <- unique(spec$Site)

    in.both <- mapply(function(a, b)
        a[a %in% b],
        a=plants.2013,
        b=plants.2014,
        SIMPLIFY=FALSE)


    sites <- sapply(strsplit(names(nets),  "\\."), function(x) x[[1]])
    in.both.nets <- list()
    for(i in 1:length(sites)){
        these.nets <- nets[[i]]
        these.plants <- in.both[[sites[i]]]
        out.nets <- these.nets[rownames(these.nets) %in% these.plants,]
        in.both.nets[[i]] <- out.nets
    }
    names(in.both.nets) <- names(nets)
    in.both.nets <-  in.both.nets[sapply(in.both.nets,
                                         function(x) all(dim(x) > 2))]
    in.both.nets <-  in.both.nets[sapply(in.both.nets, class) == "matrix"]
    return(in.both.nets)
}
