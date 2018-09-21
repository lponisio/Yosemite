
calcBetaStatus <- function(comm,
                           status,
                           dis.method,
                           nulls,
                           sub= "pol",
                           occ= FALSE,
                           years,
                           zscore=FALSE){ ## calculate zscores?
    ## computes dispersion of community matrices, returns output of
    ## vegan function
    ## create community dissimilarity matrix
    comm.dis <-  lapply(comm, function(x) {
        as.matrix(vegdist(x, method= dis.method, diag= TRUE, binary= occ))
    })
    ## null dissimilarity matrices
    null.dis <- vector("list", length(nulls))
    for(i in 1:length(nulls)){
        this.null <- nulls[[i]]
        null.dis[[i]] <- lapply(this.null, function(x) {
            as.matrix(vegdist(x, method= dis.method, diag=TRUE))
        })
        null.dis[[i]][[length(nulls[[i]]) + 1]] <- comm.dis[[i]]

    }
    beta.disper.result <- vector("list", length(comm))
    for(i in 1:length(comm)){
        arr <- array(unlist(null.dis[[i]]), c(dim(comm.dis[[i]])[1],
                                              dim(comm.dis[[i]])[2],
                                              length(nulls[[i]]) + 1))
        ## standardize dissimilarities
        if(!zscore){
            less.than  <-   apply(arr, 1:2, function(x){
                sum(x[length(null.dis[[i]])] > x)
            })
            equal.2  <-   apply(arr, 1:2, function(x){
                sum(x[length(null.dis[[i]])] == x)
            })
            cor.dis <- as.dist((less.than + 0.5*equal.2)/
                               length(null.dis[[i]]), diag= TRUE)
        }else{
            cor.dis  <-  (comm.dis[[i]] -
                          apply(arr , 1:2 , mean))/
                (apply(arr , 1:2 , sd) + 10^-10)
            cor.dis <-  as.dist(((cor.dis - min(cor.dis))/diff(range(cor.dis))),
                                diag= TRUE)
        }
        ## run model
        beta.disper.result[[i]] <- betadisper(cor.dis, rep("all", nrow(as.matrix(cor.dis))),
                                              type="centroid")
    }
    return(beta.disper.result)
}



makeBetaDataPretty <- function(){
    sites <- sapply(comm$comm, length)
    nyears <- sapply(comm$comm, function(x){
        sapply(x, nrow)}
        )
    nrep.years <- sapply(nyears, sum)
    name.years <- sapply(comm$comm, function(x){
        sapply(x, rownames)}
        )

    distances <- lapply(dis, function(x){
        lapply(x, function(y) y$distances)}
        )

    species.names <- rep(unlist(sapply(nyears, names)), unlist(nyears))

    dats <- data.frame(site=rep(names(sites), nrep.years),
                       year=unlist(name.years),
                       species=species.names,
                       dist=unlist(distances))

    rownames(dats) <- NULL

    #dats <- merge(dats, abund, by.x="species", by.y="GenusSpecies")
    #dats <- merge(dats, phen, by.x="species", by.y="GenusSpecies")
    #dats <- merge(dats, traits, by.x="species", by.y="GenusSpecies")
    dats <- merge(dats, traits, by.x="species", by.y="GenusSpecies")
    return(dats)
}
