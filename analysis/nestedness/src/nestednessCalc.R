

zvals <- function(stat, null.stats){
  z.sd <- (stat - mean(c(null.stats, stat)))/
           sd(c(null.stats, stat))
  return(z.sd)
}

pvals <- function(stat, null.stats){
  pval <- sum(stat <= c(null.stats, stat))/
    length(c(null.stats, stat))
  return(pval)
}

calc.by.status <- function(commz, nulls,
                           statusez,
                           statuz,
                           weighted){
  extract.stat <- function(comm,
                           statuses,
                           status){
    out <- empty(comm[statuses == status,])
    return(out)
  }
  commz <- commz[sapply(statusez, function(x){
    statuz %in% unique(x)
  })]
  nulls <- nulls[sapply(statusez, function(x){
    statuz %in% unique(x)
  })]
  statusez <-  statusez[sapply(statusez, function(x){
    statuz %in% unique(x)
  })]
  pol <- mapply(function(a, b)
                extract.stat(comm= a,
                             statuses= b,
                             status=statuz),
                a=commz,
                b=statusez)
  pol.nulls <- mapply(function(a, b)
                      lapply(a, function(c){
                        extract.stat(comm= c,
                                     statuses= b,
                                     status=statuz)}),
                      a=nulls,
                      b=statusez,
                      SIMPLIFY=FALSE)

  nodf.pol <- sapply(pol, FUN=function(data){
    nestednodf(data, weighted=weighted)$statistic["NODF"]
  })

  nodf.nulls.pol <- rapply(pol.nulls, f=function(data){
    nestednodf(data, weighted=weighted)$statistic["NODF"]
  }, how="replace")

  nodf.nulls.pol <- lapply(nodf.nulls.pol, unlist)
  z <- mapply(function(a, b)
              zvals(stat= a,
                    null.stats= b),
              a=nodf.pol,
              b=nodf.nulls.pol)

  p <- mapply(function(a, b)
              pvals(stat= a,
                    null.stats= b),
              a=nodf.pol,
              b=nodf.nulls.pol)
  return(rbind(z, p))
}
