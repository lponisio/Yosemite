
pdf.f <- function(f, file, ...) {
  cat(sprintf('Writing %s\n', file))
  pdf(file, ...)
  on.exit(dev.off())
  f()
}

## This functions takes site-species-abundance data and creates a
## matrix where the sites are columns and the rows are species.

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
             GenSp = temp[, 2])
}


add.alpha <- function(col, alpha=0.2){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
        rgb(x[1], x[2], x[3],
            alpha=alpha))  
}



mod <- function(forms, fam, ys, dats, return.sum=FALSE){
  if(fam =="poisson"){
    print(ys)
    mod <- glmer(forms,
                 family=fam,
                 data=dats,
                 ## nAGQ = 10L,
                 control=glmerControl(optimizer="bobyqa",
                   optCtrl=list(maxfun=1e9)))
    ifelse(return.sum,
           return(summary(mod)),
           return(mod))
  }else if(fam =="nbinom"){
    print(ys)
    mod <- glmer.nb(forms,
                    data=dats,
                    control=glmerControl(optimizer="bobyqa",
                      optCtrl=list(maxfun=1e9),  tolPwrss=1e-5))
  }else if(fam =="znbinom"){
    print(ys)
    mod <- glmmadmb(forms,
                    data=dats,
                    fam="nbinom",
                    zeroInflation=TRUE,
                    extra.args="-ndi 60000")
  } else if(fam == "gaussian"){
    print(ys)
    mod <- do.call(lmer,
                   list(formula=forms,
                        data=dats))
    ifelse(return.sum,
           return(summary(mod)),
           return(mod))
  } 
}

## function to check for overdispersion
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



