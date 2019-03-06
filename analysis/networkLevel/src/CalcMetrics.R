
mut.adj <- function(x) {
    nr <- dim(x)[1]
    nc <- dim(x)[2]
    to.fill <- matrix(0, ncol=nc + nr, nrow=nc + nr)
    to.fill[1:nr,(nr+1):(nc+nr)] <- x
    adj.mat <- graph.adjacency(to.fill, mode= "upper", weighted=TRUE)
    return(adj.mat)
}

calc.mod <- function(dat.web){
    ## converts a p-a matrix to a graph for modularity computation
    graph <- mut.adj(dat.web)
    weights <- as.vector(dat.web)
    weights <- weights[weights != 0]

    ## if matrix is binary, modularity calculate is not affected by
    ## the weights
    greedy <- modularity(graph,
                         membership(fastgreedy.community(graph,
                                                         weights=weights)),
                         weights=weights)

    random.walk <-  modularity(graph,
                               membership(walktrap.community(graph,
                                                             weights=weights)),
                               weights=weights)
    return(c(G=greedy,
             R=random.walk))
}

calcMetric <- function(dat.web, ...) {
    ## calculates modularity
    dat.web <- as.matrix(empty(dat.web))
    ## matrix of all the same number
    if(min(dat.web) == max(dat.web)){
        return(c(mets=rep(0, 1),
                 mod.met=rep(0,2)))
    } else{
        mets <-  networklevel(dat.web, ...)
        spec.mets <- specieslevel(dat.web, c("d", "normalised degree"))
        mets <- c(mets, d.HL=mean(spec.mets$`higher level`$d,
                                  na.rm=TRUE))
        mets <- c(mets, ND.HL=mean(spec.mets$`higher level`$normalised.degree,
                                  na.rm=TRUE))
        mets <- c(mets, d.LL=mean(spec.mets$`lower level`$d,
                                  na.rm=TRUE))
        mets <- c(mets, ND.LL=mean(spec.mets$`lower level`$normalised.degree,
                                  na.rm=TRUE))

    }
    mod.met <- calc.mod(dat.web)
    return(c(mets, mod.met= mod.met))

}


## function to simulate 1 null, and calculate statistics on it
calcNullStat <- function(dat.web,
                         null.fun= vaznull.fast,...) {
    sim.web <- null.fun(dat.web)
    return(calcMetric(sim.web, ...))
}

##  function that computes summary statistics on simulated null matrices
##  (nulls simulated from web N times)
calcNetworkMetrics <- function (dat.web, N,
                                H2_integer=TRUE,
                                index= c("H2",
                                         "connectance",
                                         "weighted connectance",
                                         "niche overlap",
                                         "number of species",
                                         "mean number of shared partners",
                                         "vulnerability",
                                         "partner diversity",
                                         "functional complementarity",
                                         "weighted NODF",
                                         "interaction evenness",
                                         "links per species"),
                                dist="chao") {
    ## calculate pvalues
    pvals <- function(stats, nnull){
        rowSums(stats >= stats[, rep(1, ncol(stats))])/(nnull + 1)
    }
    ## calculate zvalues two different ways
    zvals <-function(stats){
        z.sd <- (stats[,1] -
                 apply(stats, 1, mean, na.rm = TRUE))/
            apply(stats, 1, sd, na.rm = TRUE)
        z.sd[is.infinite(z.sd)] <- NA
        return(z.sd)
    }
    ## check that matrix is proper format (no empty row/col and no NAs)
    if(all(is.na(dat.web) == FALSE)) {
        ## drop empty rows and columns
        dat.web <- as.matrix(empty(dat.web))
        ## check to make sure emptied matrix is large enough
        ## to calculate statistics on
        if(is.matrix(dat.web)){
            if(all(dim(dat.web) >= 2)) {
                ## calculate null metrics
                null.stat <- replicate(N, calcNullStat(dat.web,
                                                       H2_integer=H2_integer,
                                                       index=index,
                                                       dist=dist),
                                       simplify=TRUE)
                ## calculate metrics from data
                true.stat <- calcMetric(dat.web, H2_integer=H2_integer,
                                        index=index)
                out.mets <- cbind(true.stat, null.stat)
                ## compute z scores
                zvalues <- zvals(out.mets)
                names(zvalues) <- paste("z", names(true.stat), sep="")
                ## compute p-values
                pvalues <- pvals(out.mets, N)
                names(pvalues) <- paste("p", names(true.stat), sep="")
                out <- c(true.stat, zvalues, pvalues)
                return(out)
            }
        }
    }
    return(rep(NA, (length(index) + 4)*3))
}

prepDat <- function(cor.stats, spec.dat){
    dats <- do.call(rbind, cor.stats)
    out <- data.frame(dats)
    out$Site <- sapply(strsplit(names(cor.stats), "\\."),
                       function(x) x[1])
    out$Date <-  sapply(strsplit(names(cor.stats), "\\."),
                        function(x) x[2])
    out$Year <- format(as.Date(out$Date), "%Y")
    rownames(out) <- NULL
    return(out)
}
