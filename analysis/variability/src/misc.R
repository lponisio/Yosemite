
## standardize a vector
standardize <- function(x)
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)


getNetData <- function(nets){
    sites <- sapply(strsplit(nets, "[.]"),
                       function(x) x[1])
    dates <-  sapply(strsplit(nets, "[.]"),
                        function(x) x[2])
    years <- format(as.Date(dates), "%Y")
    return(data.frame(Date=dates,
                      Site=sites,
                      Year=years))
}

add.alpha <- function(col, alpha=0.2){
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
              rgb(x[1], x[2], x[3],
                  alpha=alpha))
}


pdf.f <- function(f, file, ...) {
    cat(sprintf('Writing %s\n', file))
    pdf(file, ...)
    on.exit(dev.off())
    f()
}

samp2site.spp <- function(site,spp,abund) {
    x <- tapply(abund, list(site=site,spp=spp), sum)
    x[is.na(x)] <- 0

    return(x)
}

vif.mer <- function (fit) {
    ## variance inflation factor
    ## adapted from rms::vif

    v <- vcov(fit)
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    return(v)
}

cv <- function(x) sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)

corCv <- function(x,...){
    cv(x)*(1 + (1/4*length(x)))
}

convertMatrix2Sample <- function(z){
    temp <- data.frame(expand.grid(dimnames(provideDimnames(z)))[1:2],
                       as.vector(as.matrix(z)))
    temp <- temp[!is.na(temp[, 3]), ]
    temp <- temp[sort.list(temp[, 1]), ]
    data.frame(Site = temp[, 1], deltaAbund = temp[, 3],
               GenusSpecies = temp[, 2])
}

