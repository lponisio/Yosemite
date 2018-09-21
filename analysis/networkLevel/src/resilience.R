
simExtinction <- function(nets,
                          extinction.method,
                          spec,
                          participant="lower"){
    ## calculates the robustness of a network using Memmot et al.'s method
    ## takes the adjacency matrix, whether to drop species by abundance or
    ## degree and whther to drop the "higer" or "lower" level of the
    ## network
    ## returns a data frame with the site, robustness score and merges
    ## with pyrodiv dataset

    ext <- lapply(nets, second.extinct,
                  participant="lower",
                  method=extinction.method)

    rob <- sapply(ext, robustness)
    sites <- sapply(strsplit(names(rob), "[.]"), function(x) x[1])
    dates <-  sapply(strsplit(names(rob), "[.]"),
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
