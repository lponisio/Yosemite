## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize-delta.R')

spec.abund <- calcSpecABund(spec)
spec.abund <- getScorePrepDrought(pol.pca.scores,
                                  beta.dist,
                                  spec.abund,
                                  var.method=cv)


delta <- calcYearDiff(spec.abund)
delta <- prepDeltaVar(delta=delta,
                      spec.abund=spec.abund,
                      site.char=dat.mods,
                      veg=veg)

xvars <- c("beta.dist", "var.pca1")
delta$delta <- delta$delta[!apply(delta$delta[, xvars], 1, function(x)
    any(is.na(x))),]

## drop extreme role var value
delta$delta <- delta$delta[delta$delta$var.pca1 !=
                           min(delta$delta$var.pca1),]

delta$delta <- delta$delta[delta$delta$var.pca1 !=
                           min(delta$delta$var.pca1),]

## ************************************************************
## delta abund
## ************************************************************
print("******** delta abund **********")
mods <- lmer(deltaAbund ~ scale(beta.dist)*scale(simpson.div) +
                 scale(var.pca1)*scale(simpson.div) +
                 scale(deltaFloralAbund) +
                 (1|GenusSpecies), na.action = "na.fail",
             data=delta$delta[delta$delta$deltaAbund != 0,])
vif.mer(mods)
summary(mods)

plotHist <- function(){
      par(oma=c(4, 4, 0.5, 0.5),
      mar=c(2, 2, 2, 2),
      cex.axis=1.5)
    hist(delta$site.data$simpson.div,
         xlab="",
         ylab="",
         las=1,
         col="grey",
         main="",
         lwd=1.5)
    mtext("Frequency", 2, line=4.5, cex=1.5)
    mtext("Fire history diversity", 1, line=3, cex=1.5)
}

pdf.f(plotHist, file= file.path("figures/div.pdf"),
      width=10, height=7)

