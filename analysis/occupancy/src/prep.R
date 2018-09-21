source('src/make-matrix.R')

prep <- function(nzero, threshold, phen, save.dir) {
  ## create big site x date x species matrix
  load('~/Dropbox/yosemite/analysis/data/spec.Rdata')
  sr.sched <- read.csv('data/conditions.csv', as.is=TRUE)
  sr.sched$Date <- as.Date(sr.sched$Date, format='%m/%d/%y')
  spec$Date <- as.Date(spec$Date, format='%m/%d/%y')

  null.mat <- tapply(rep(0, nrow(sr.sched)),
                     list(sites=paste(sr.sched$Site),
                          dates=sr.sched$Date), sum)

  d <- data.frame(pollinator=spec$GenusSpecies,
                  site=spec$Site,
                  date=spec$Date)

  pollinator.id <- id(d$pollinator)
  mats <- make.mats(pollinator.id,
                    null.mat,
                    pollinator=as.vector(d$pollinator),
                    var1=as.vector(d$site),
                    var2=d$date)
  sites <- rownames(mats[[1]])
  dates <- colnames(mats[[1]])
  species <- names(mats)
  mat <- array(unlist(mats), dim=c(dim(mats[[1]]), length(mats)))
  dimnames(mat) <- list(site=sites, date=dates, species=species)

  mm <- make.mat(mat, threshold, nzero)
  mat <- mm$mat
print(mat)
  ## make 4D matrix
  mats <- lapply(1:dim(mat)[2], function(x) mat[,x,])
  mats.split <- split(mats, dimnames(mat)$date)
  yr.table <- table(dimnames(mat)$date)
  X <- array(NA, dim=c(dim(mat)[1], length(yr.table),
                   max(yr.table), dim(mat)[3]))
  dimnames(X) <- list(site=dimnames(mat)$site,
                      year=unique(dimnames(mat)$date),
                      rep=1:max(yr.table),
                      species=dimnames(mat)$species)

  null.mat <- matrix(NA, dim(mat)[1], dim(mat)[3],
                     dimnames=dimnames(X)[c('site', 'species')])
  f <- function(i) {
    missing <- max(yr.table)-yr.table[i]
    if(missing==0) return(mats.split[[i]])
    c(mats.split[[i]], lapply(1:missing, function(x) null.mat))
  }
  tmp <- lapply(seq_along(yr.table), f)

  for(i in 1:length(yr.table))
    for(j in 1:max(yr.table))
      X[,i,j,] <- tmp[[i]][[j]]

  ## only keep sites with some positive number of reps
  no.reps <- apply(mat, 1, function(x) sum(x>=0, na.rm=TRUE))==0
  X <- X[!no.reps,,,,drop=FALSE]

  make.date.mat <- function(dates) {
    date.mat <- array(NA, dim=dim(X)[1:3], dimnames=dimnames(X)[1:3])
    for(i in seq_along(dates)) {
      year <- as.numeric(format(as.Date(dates[[i]], format='%Y-%m-%d'),
                                format = '%Y'))
      lengths <- rle(as.vector(year))$lengths
      ind <- cbind(rep(i, sum(lengths)),
                   match(year, dimnames(X)$year),
                   as.vector(unlist(sapply(lengths, seq_len))))
      date.mat[ind] <- strptime(dates[[i]], '%Y-%m-%d')$yday+1
    }
    date.mat
  }
  date.mats <- lapply(mm$dates, make.date.mat)
  dm <- array(unlist(date.mats),
              dim=c(dim(date.mats[[1]]), length(date.mats)))
  dimnames(dm) <- dimnames(X)

  ## function to re-arrange replicate dimension
  compress <- function(x) {
    if(!any(is.na(x))) return(x)
    return(c(x[!is.na(x)], x[is.na(x)]))
  }
  X <- aperm(apply(X, c(1,2,4), compress), c(2,3,1,4))
  names(dimnames(X)) <- c("site", "year", "rep", "species")
  dm <- aperm(apply(dm, c(1,2,4), compress), c(2,3,1,4))

  ## inits
  ## specify the initial values
  ## z.init <- apply(X, c(1,2,4),
  ##                 function(x) (sum(x,na.rm=TRUE)>0)*1)
  ## z.init[apply(X, c(1,2,4), function(x) !any(!is.na(x)))] <- NA
  z.init <- X
  ## z.init[z.init==0] <- 1

  save.path <- file.path('saved',
                         sprintf('%s-%d.RData', threshold, nzero))
  sev <- sev.site[,2][match(dimnames(X)$site, sev.site[,1])]
  list(X=X,
       sev= as.numeric(as.factor(sev)),
       day=standardize(dm),
       nrep=apply(X, c(1,2,4),
         function(x) sum(x>=0,na.rm=TRUE)),
       z.init=z.init,
       nsp=dim(X)[4],
       nsite=dim(X)[1],
       nyear=dim(X)[2],
       file.name=save.path)
}

analyse.ms.ms <- function(d, case, ...) {

  file.name <- d$file.name
  z.init <- d$z.init
  my.inits <- function() {
    list(Z=z.init,
         omega=1)
  }

  d <- d[!names(d) %in% c('z.init', 'file.name')]

  dd <- list(data=d, inits=my.inits, params=get.params())
  res <- list(data=d, bugs=ms.ms(dd,...))
  summary <- list(data=d, bugs=res$bugs$BUGSoutput$summary)
  save(summary, file=sprintf('%s-summary.RData',
                  strsplit(file.name,split='.RData')[[1]]))
  save(res, file=file.name)
  NULL
}
