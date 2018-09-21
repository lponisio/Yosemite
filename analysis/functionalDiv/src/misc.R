## plotting
add.alpha <- function(col, alpha=0.2){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))  
}

## return sorted unique values
id <- function(x) unique(sort(x))

## standardize a vector
standardize <- function(x)
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)

## convert a character vector into an integer vector
make.integer <- function(x)
  as.integer(as.factor(x))-1

## expit and logit functions
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

## standard error function
se <- function(x) sqrt(var(x)/(length(x)-1))

## function to clean up white-space in a column of data (replaces all
## instances of white-space with " " and empty cells with ""
fix.white.space <- function(d) {
  d <- as.character(d)
  remove.first <- function(s) substr(s, 2, nchar(s))
  d <- gsub("      ", " ", d, fixed=TRUE)
  d <- gsub("     ", " ", d, fixed=TRUE)
  d <- gsub("    ", " ", d, fixed=TRUE)
  d <- gsub("   ", " ", d, fixed=TRUE)
  d <- gsub("  ", " ", d, fixed=TRUE)
  
  tmp <- strsplit(as.character(d), " ")
  d <- sapply(tmp, function(x) paste(x, collapse=" "))

  first <- substr(d, 1, 1)
  d[first==" "] <- remove.first(d[first==" "])
  d
}

## nice little pdf function
pdf.f <- function(f, file, ...) {
  cat(sprintf("Writing %s\n", file))
  pdf(file, ...)
  on.exit(dev.off())
  f()
}

## load and return loaded object
load.local <- function(file) {
 v <- load(file)
 stopifnot(length(v) == 1)
 get(v)
}

## function to make pollinator visitation matrices
make.mats <- function(pollinator.id, null.mat, pollinator, var1, var2) {
  make.mat <- function(P) {
    var1 <- var1[pollinator==P]
    var2 <- var2[pollinator==P]
    m <- tapply(rep(1, length(var1)),
                list(sites=var1, dates=var2), sum)

    null.mat[rownames(m), colnames(m)][!is.na(m)] <- m[!is.na(m)]
    null.mat
  }
  mats <- lapply(pollinator.id, function(x) make.mat(x))
  names(mats) <- pollinator.id
  mats
}

mut.adj <- function(x) {
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  to.fill <- matrix(0, ncol=nc + nr, nrow=nc + nr)
  to.fill[1:nr,(nr+1):(nc+nr)] <- x
  rownames(to.fill) <- c(rownames(x), rep(NA, nrow(to.fill)-nrow(x)))
  colnames(to.fill) <- c(colnames(x), rep(NA, ncol(to.fill)-ncol(x)))
  adj.mat <- graph.adjacency(to.fill, mode= "undirected",
                             weighted=TRUE,
                             add.rownames="code", add.colnames="names")
  return(adj.mat)
}


samp2site.spp <- function(site,spp,abund) { 
  x <- tapply(abund, list(site=site,spp=spp), sum)
  x[is.na(x)] <- 0
  return(x)
}

comm.mat2sample <-  function (z) {
  temp <- data.frame(expand.grid(dimnames(z))[1:2],
                     as.vector(as.matrix(z)))
  temp <- temp[sort.list(temp[, 1]), ]
  data.frame(Site = temp[, 1], Abund = temp[, 3],
             GenusSpecies = temp[, 2])
}

## This functions takes site-species-abundance data and creates a
## matrix where the sites are columns and the rows are species. 

##this function takes three vectors: 1) the site species are found in.
## For plant animal networks this is generally the pollinators
## 2) the species found in those sites.
## In the case of p-a data is this usally the plants and
## 3) the adbundance of those species. In p-a data this is the
## frequency of interactions 

