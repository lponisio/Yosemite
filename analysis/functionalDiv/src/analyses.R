## ************************************************************
## compute metrics on actual data
run.analysis <- function(dd.model) {
  
  compute.metric <- function(mat) {
  
    mat <- mat[!is.na(rowSums(mat)),]
    traits <- dd.model$dd.traits
    drop.sp <- which(colSums(mat)==0)
    drop.site <- which(rowSums(mat)==0)
    if(length(drop.sp) > 0) {
      traits <- traits[-drop.sp,]
      mat <- mat[,-drop.sp]
    }
    if(length(drop.site) > 0){
      mat <- mat[-drop.site,]
    }
    ## convert character traits to factors
    for(i in dd.model$traits$cat){
      traits[,i] <- as.factor(traits[,i])
    }
    dbFD(traits, mat)
  }
  metrics <- apply(dd.model$mat.y, 'year', compute.metric)
  analysis <- function(scores) {
    lengths <- sapply(scores, length)
    site <- dimnames(dd.model$mat.y)$site
    year <- rep(names(scores), lengths)
    vals <- unlist(scores)
    status <- as.character(dd.model$dd.statuses)
    dd <- data.frame(site=site, status=status, year=year, vals=vals,
                     row.names=NULL)
    model.out <- lmer(vals ~ status*year + (1|site), data=dd)
    return(list(dd=dd, model.out=model.out)) 
  }

  f <- function(x)
    analysis(mapply(function(a, b) a[[b]], a=metrics, b=x,
                    SIMPLIFY=FALSE))
  
  cases <- c('FRic', 'FEve', 'FDiv', 'FDis')
  res <- lapply(cases, f)
  names(res) <- cases
  res
}

