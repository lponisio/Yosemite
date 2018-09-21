## from year specific communities generates a single community across
## years (i.e. rows are site-year combinations)
make.all.comm <- function(comm){
  comm.all <- empty(do.call(rbind,
                            lapply(dimnames(comm$mat.y)[2]$year,
                                   function(i){
                                     comm$mat.y[, i, ]})))
  rownames(comm.all) <- paste(rownames(comm.all),
                              comm$years, sep="_")
  return(comm.all)
}

## takes a community separated by years, generates year-sepcific
## nulls, and returns a mat all site-year combinations and species
nulls.yr.strata <- function(nulls.yr, comm.all, years, path.save){
  all.comms <- comm.all
  fill.mat <- function(i, all.comm){
    for(year in years) {
      mat <- nulls.yr[[year]][[i]]
      s1 <- paste(dimnames(mat)[[1]], year, sep='_')
      s2 <- dimnames(mat)[[2]]
      all.comm[s1, s2] <- mat 
    }
    return(all.comm)
  }
  nulls <- lapply(1:length(nulls.yr[[1]]), fill.mat,
                  all.comm=all.comms)
  save(nulls, file=path.save)
}



## calculates null and observed trait values and plots
null.traits <- function(trait,
                        comm,
                        comm.all,
                        nulls,
                        type='GenusSpecies',
                        sub='all',
                        binary=TRUE){
  ## calculate empirical pvalues
  pvals <- function(null.func, obs.func, N){
    nulls <- cbind(null.func, obs.func)
    h.pvals <- rowSums(apply(nulls, 2, function(x)
                             obs.func <= x))/(N + 1)
    l.pvals <- rowSums(apply(nulls, 2, function(x)
                             obs.func >= x))/(N + 1)
    return(rbind(h.pvals, l.pvals))
  }
  mean.func <- function(comm,
                        traits,
                        statuses,
                        type='obs'){
    if(binary){
      comm[comm > 1] <- 1
    }
    if(is.numeric(traits)){
      ## take the mean of the traits by site
      specs <- comm %*% diag(traits)
      specs[comm==0] <- NA
      trait.sums <- rowSums(specs, na.rm=TRUE)
      abunds <- rowSums(comm, na.rm=TRUE)
      bysite <- trait.sums/abunds
    } else{
      ## calculate the diversity of traits,
      ## take the mean for each site
      rep.traits <- matrix(rep(traits, each=nrow(comm)),
                           nrow= nrow(comm), byrow=FALSE)
      specs <- comm
      specs[specs > 0] <- rep.traits[specs > 0]
      counts.trait <- apply(comm, 1, function(x){
        x <- tapply(x, traits, sum, na.rm=TRUE)
      })
      bysite <- apply(counts.trait, 2, function(x) {
        diversity(x, index='simpson')
      })
    }

    bystatus <- tapply(bysite, statuses, mean)
    if(type == 'obs'){ ## for the observed communities
      return(list(dats=bystatus, traits=bysite))
    } else{ ## for the null communities
      return(bystatus)
    }
  }
  traits <- comm$traits[, trait][match(colnames(comm.all),
                                       rownames(comm$traits))]
  obs.func <- mean.func(comm=comm.all,
                        traits=traits,
                        statuses=unlist(comm$status))
  null.func <- sapply(nulls, mean.func,
                      traits=traits,
                      statuses=unlist(comm$status),
                      type='null')
  
  ## calculate empirical pvalues
  out.pvals <- pvals(null.func,
                     obs.func$dats, length(nulls))
  quantile <- 1 - out.pvals[2,]
  return(list(pvals=out.pvals,
              quantile=quantile,
              obs=obs.func,
              null.func=null.func))
}
