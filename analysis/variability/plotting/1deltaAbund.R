
rm(list=ls())
setwd('analysis/variability')
source('plotting/src/initialize.R')
load('saved/mods/drought.Rdata')


## ************************************************************
## historgram for manucript figure 1
## ************************************************************

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
    mtext("Pyrodiversity", 1, line=3, cex=1.5)
}

pdf.f(plotHist, file= file.path("figures/div.pdf"),
      width=10, height=7)

## ************************************************************
## delta abund interaction plot
## ************************************************************
delta <- delta$delta
## network role
dd.abund.var.pca <- expand.grid(var.pca1=seq(
                                    from= min(delta$var.pca1),
                                    to=max(delta$var.pca1),
                                    length=10),
                                simpson.div = mean(delta$simpson.div),
                                beta.dist=mean(delta$beta.dist),
                                deltaFloralAbund=
                                    mean(delta$deltaFloralAbund),
                                mean.pca1=
                                    mean(delta$mean.pca1),
                                deltaAbund=0)
abund.var.pca.pi <- predict.int(mod= mods,
                                dd=dd.abund.var.pca,
                                y="deltaAbund",
                                family="guassian")

## partner
dd.abund.beta.dist <- expand.grid(beta.dist=seq(
                                      from= min(delta$beta.dist),
                                      to=max(delta$beta.dist),
                                      length=10),
                                  mean.pca1=
                                      mean(delta$mean.pca1),
                                  simpson.div = mean(delta$simpson.div),
                                  var.pca1=mean(delta$var.pca1),
                                  deltaFloralAbund=
                                      mean(delta$deltaFloralAbund),
                                  deltaAbund=0)
abund.beta.dist.pi <- predict.int(mod= mods,
                                  dd=dd.abund.beta.dist,
                                  y="deltaAbund",
                                  family="guassian")
pdf.f(plotDeltaDiag,
      file="figures/diagnostics/deltaAbund.pdf",
      height=9, width=3)

plotInteractionsPyroDiv()


## ************************************************************
## species metrics
## ************************************************************

pdf.f(makeMetricBySppPlot, file= file.path("figures/sppMets.pdf"),
      width=13, height=9)

