SimSecondExtinction <- function (web, participant = "higher", method = "abun", nrep = 100,
                                 details = FALSE, ext.row = NULL,
                                 ext.col = NULL){
    ## function modified from bipartite package to replicate
    ## extinction sequence because sveral species have the same
    ## abundance, leading to different estimates for robustness
    ## depending on the seed
    if (participant == "both" & method == "external")
        stop("Sorry, that won't work. When you specify the sequence, you have to choose one of the two levels. 'both' won't work.")
    if (!is.null(ext.row) & length(ext.row) != NROW(web))
        stop("The length of the external row vector is different from the numbers of rows in the network!")
    if (!is.null(ext.col) & length(ext.col) != NCOL(web))
        stop("The length of the external col vector is different from the numbers of cols in the network!")
    if (participant == "higher" & method == "external" & is.null(ext.col))
        stop("You need to provide an external sequence of extinction for the higher trophic level!")
    if (participant == "lower" & method == "external" & is.null(ext.row))
        stop("You need to provide an external sequence of extinction for the lower trophic level!")
    one.second.extinct <- function(web = web, participant = participant,
                                   method = method, ext.row = ext.row, ext.col = ext.col) {
        dead <- matrix(nrow = 0, ncol = 3)
        colnames(dead) <- c("no", "ext.lower", "ext.higher")
        m2 <- web
        i <- 1
        repeat {
            n <- extinction(m2, participant = participant, method = method,
                            ext.row = ext.row, ext.col = ext.col)
            dead <- rbind(dead, c(i, attributes(m2 <- empty(n,
                                                            count = TRUE))$empty))
            if (participant == "lower" & NROW(m2) < 2)
                break
            if (participant == "higher" & NCOL(m2) < 2)
                break
            if (participant == "both" & min(dim(m2)) < 2)
                break
            if (any(dim(n) == 1))
                break
            if (method == "external") {
                ext.col[ext.col > ext.col[1]] <- ext.col[ext.col >
                                                         ext.col[1]] - 1
                ext.row[ext.row > ext.row[1]] <- ext.row[ext.row >
                                                         ext.row[1]] - 1
                ext.row <- ext.row[-1]
                ext.col <- ext.col[-1]
            }
            i <- i + 1
        }
        dead2 <- rbind(dead, c(NROW(dead) + 1, NROW(m2), NCOL(m2)))
        if (participant == "lower" & method == "degree") {
            if (length(table(dead[, 2])) > 1)
                dead2[, 2] <- 1
        }
        if (nrow(dead) + 1 != nrow(dead2))
            stop("PANIC! Something went wrong with the extinct sequence! Please contact the author to fix this!!")
        if (participant == "lower")
            supposed.length <- NROW(web)
        if (participant == "higher")
            supposed.length <- NCOL(web)
        if (participant == "both")
            supposed.length <- NROW(dead2)
        if (NROW(dead2) != supposed.length) {
            missing <- supposed.length - NROW(dead2)
            addit1 <- (NROW(dead2) + 1):(NROW(dead2) + missing)
            addit2n3 <- rep(0, times = missing)
            dead2 <- rbind(dead2, as.matrix(data.frame(addit1,
                                                       addit2n3, addit2n3)))
        }
        return(dead2)
    }
    if (is.vector(method))
        sequence = method
    ## if (pmatch(method, c("abundance", "random", "degree", "external")) %in%
    ##     c(1, 3, 4)) {
    ##     out <- one.second.extinct(web = web, participant = participant,
    ##                               method = method, ext.row = ext.row, ext.col = ext.col)
    ## }
    o <- replicate(nrep, one.second.extinct(web = web, participant = participant,
                                            method = method, ext.row = ext.row, ext.col = ext.col),
                   simplify = FALSE)
    if (details) {
        out <- o
    }
    else {
        lengths <- sapply(o, nrow)
        z <- o[[which.max(lengths)]]
        z[, 2:3] <- 0
        for (k in 1:length(o)) {
            nr <- nrow(o[[k]])
            z[1:nr, ] <- z[1:nr, ] + o[[k]]
            rm(nr)
        }
        out <- z/length(o)
        out[, 1] <- 1:max(lengths)
    }

    class(out) <- "bipartite"
    attr(out, "exterminated") <- c("both", "lower", "higher")[pmatch(participant,
                                                                     c("both", "lower", "higher"))]
    out
}




simExtinction <- function(nets,
                          extinction.method,
                          spec,
                          participant){
    ## calculates the robustness of a network using Memmot et al.'s method
    ## takes the adjacency matrix, whether to drop species by abundance or
    ## degree and whther to drop the "higer" or "lower" level of the
    ## network
    ## returns a data frame with the site, robustness score and merges
    ## with pyrodiv dataset
    ext <-  list()
    for(i in 1:length(nets)){
        if(all(dim(nets) > 3)){
            this.ext <- try(SimSecondExtinction(nets[[i]], participant=participant,
                                                method=extinction.method, nrep=100))
            if(inherits(this.ext, "try-error")){
                print(i)
                next
            }else{
                ext[[i]] <- this.ext
            }
        }
    }
    net.names <-  names(nets)[!sapply(ext, is.null)]
    ext <- ext[!sapply(ext, is.null)]

    ## ext <- lapply(nets, second.extinct,
    ##               participant=participant,
    ##               method=extinction.method)
    rob <- sapply(ext, robustness)
    sites <- sapply(strsplit(net.names, "[.]"), function(x) x[1])
    dates <-  sapply(strsplit(net.names, "[.]"),
                     function(x) x[2])
    years <- format(as.Date(dates), "%Y")

    dats <- data.frame(Site= sites,
                       Year=years,
                       Date=dates,
                       Robustness=rob)
    rownames(dats) <- NULL
    dats <- merge(dats, spec)
    return(dats)
}

