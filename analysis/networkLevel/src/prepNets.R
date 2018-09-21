##the purpose of this function is to break up data with many
##sites/years and prepare it for network analysis.

break.net <- function(spec.dat, date="Date", unlist.data=TRUE){
  samp2site.spp <- function(site, spp, abund) { 
    x <- tapply(abund, list(site= site, spp= spp), sum)
    x[is.na(x)] <- 0
    return(x)
  }
  sites <- split(spec.dat, spec.dat$Site)
  networks <- lapply(sites, function(x){
    lapply(split(x, f=x[, date]), as.matrix)
  })
  ## formats data matrices appropriate for network analysis 
  comms <- rapply(networks, function(y){
    samp2site.spp(site=y[,"PlantGenusSpecies"],
                  spp=y[,"GenusSpecies"],
                  abund=rep(1, nrow(y)))
  }, how="replace")
  ## puts data together in a list and removes empty matrices
  drop.net <- function(z)
    z[!sapply(z, FUN=function(q){
      any(dim(q) < 3)
    })]
  adj.mat <- lapply(comms, drop.net)
  if(unlist.data) adj.may <- unlist(adj.mat, recursive=FALSE)
  return(adj.mat) 
}

