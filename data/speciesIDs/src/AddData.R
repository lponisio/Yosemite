add.to.data <- function(sp.ids, case, family, date) {
  lengths <- sapply(sp.ids, function(x) length(x$temp.id))
  cats <- names(sp.ids[[1]])[-length(sp.ids[[1]])]

  ## make into data-frame
  dd <- sapply(cats, function(cat)
               rep(sapply(sp.ids, function(x) x[[cat]]), lengths))
  TempID <- unlist(sapply(sp.ids, function(x) x$temp.id))

  ## check for duplicates
  if(sum(table(TempID)>1)>0) {
    cat('!!!Duplicate TempID found!!!\n')
    print(table(TempID)[table(TempID)>1])
  }

  ## lood data:
  D <- read.csv('relational/original/specimens/cleaned/specimens.csv',
                as.is=TRUE)
  ## check that no IDs are already present in main data-set
  if(any(TempID %in% D$TempID[!is.na(D$Species)])) {
    cat('!!!TempID already present!!!\n')
    print(TempID[TempID %in% D$TempID[!is.na(D$Species)]])
  }
  ind <- match(TempID, D$TempID)

  if(case=='bee') {
    D$Order[ind] <- 'Hymenoptera'
    D$GeneralID[ind] <- 'Bee'
  }
  if(case=='wasp') {
    D$Order[ind] <- 'Hymenoptera'
    D$GeneralID[ind] <- 'Wasp'
  }
  ## if(case=='syr') {
  ##   D$Order[ind] <- 'Diptera'
  ##   D$Family[ind] <- 'Syrphidae'
  ##   D$GeneralID[ind] <- 'Fly'
  ## }
  if(case=='lep') {
    D$Order[ind] <- 'Lepidoptera'
    D$GeneralID[ind] <- 'Lep'
  }
  if(case=='beetle') {
    D$Order[ind] <- 'Coleoptera'
    D$GeneralID[ind] <- 'Beetle'
  }
  if(case=='fly') {
    D$Order[ind] <- 'Diptera'
    D$GeneralID[ind] <- 'Fly'
  }
  D[ind,cats] <- dd[,cats]
  D$DateDetermined[ind] <- date

  if(case != "beetle"  & case != "fly"){
    D$Family[ind] <- family
  }
  write.csv(D,
            file='relational/original/specimens/cleaned/specimens.csv',
            row.names=FALSE)
}

