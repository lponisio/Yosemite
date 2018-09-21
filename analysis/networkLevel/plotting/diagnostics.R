## rm(list=ls())
library(lme4)
library(lmerTest)
library(RColorBrewer)
setwd('~/Dropbox/Yosemite/analysis/spatialData/config')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")
source("plotting/src/floralPlotPanels.R")
## ************************************************************
## landscape stats for top models
## ************************************************************
## type <- "all"
## d.to.keep <- "150"

load(file=sprintf("saved/data/%s%s.Rdata",
                 type, d.to.keep))

load(file=sprintf("saved/mods/%s/%s%sLOW2013.Rdata",
       xfolder, type, d.to.keep))

if(type=="all") xlabel="Pyrodiversity"
if(type=="heat") xlabel="Heat load diversity"
if(type=="slope") xlabel="Slope diversity"
if(type=="water") xlabel="Topographic wettness diversity"

cols <- rainbow(18)

## ************************************************************
## goodness of fit 
## ************************************************************
box.site <- function(){
  layout(matrix(1:2, nrow=2))

  boxplot(residuals(out.mods[[1]]) ~ Site,
          data = dat.mods, main = "Site",
          ylab = "Residuals")

  boxplot(residuals(out.mods[[2]]) ~ Site,
          data = dat.mods, main = "Site",
          ylab = "Residuals")

  ## boxplot(residuals(out.mods[[3]]) ~ Site,
  ##         data = dat.mods, main = "Site",
  ##         ylab = "Residuals")
}

resid.plot <- function(){
  layout(matrix(1:2, nrow=2))
  plot(fitted(out.mods[[1]]), residuals(out.mods[[1]]),
       xlab = "Fitted Values", ylab = "Residuals", main="Richness")
  abline(h=0, lty=2)
  lines(smooth.spline(fitted(out.mods[[1]]),
                      residuals(out.mods[[1]])))

  plot(fitted(out.mods[[2]]),
       residuals(out.mods[[2]]),
       xlab = "Fitted Values", ylab = "Residuals", main="Floral Richness")
  abline(h=0, lty=2)
  lines(smooth.spline(fitted(out.mods[[2]]),
                      residuals(out.mods[[2]])))

  ## plot(fitted(out.mods[[3]]), residuals(out.mods[[3]]),
  ##      xlab = "Fitted Values", ylab = "Residuals",
  ##      main="Floral Richness")
  ## abline(h=0, lty=2)
  ## lines(smooth.spline(fitted(out.mods[[3]]),
  ##                     residuals(out.mods[[3]])))
}


bysite.plot <- function(){
  layout(matrix(1:4, nrow=2))
     par(oma=c(2, 3, 1, 1),
        mar=c(4, 4, 2, 2), cex.axis=1.5)
  ## floral richness
  plot(dat.mods$FloralRichness~dat.mods$FuncDis,
       xlab=xlabel,
       ylab="Floral Richness")

  lapply(unique(dat.mods$Site), function(x) {
    points(dat.mods$FloralRichness[dat.mods$Site == x]~
           dat.mods$FuncDis[dat.mods$Site == x],
           col=cols[x], pch=16)
  })

  ## pollinator rihcness
  plot(dat.mods$Richness~dat.mods$FuncDis,
       xlab=xlabel,
       ylab="Bee Richness")

  lapply(unique(dat.mods$Site), function(x) {
    points(dat.mods$Richness[dat.mods$Site == x]~
           dat.mods$FuncDis[dat.mods$Site == x],
           col=cols[x], pch=16)
  })

  ## pollinator abundance
  plot(dat.mods$Abund ~ dat.mods$FuncDis,
       xlab=xlabel,
       ylab="Bee abundance")

  lapply(unique(dat.mods$Site), function(x) {
    points(dat.mods$Abund[dat.mods$Site == x]~
           dat.mods$FuncDis[dat.mods$Site == x],
           col=cols[x], pch=16)
  })

  plot(1~1, xlab="", ylab="", xaxt="n", yaxt="n", col="white")
   legend("center", col=cols, legend=unique(dat.mods$Site),
          pch=16, ncol=3, cex=1.5, bty="n")

}


path <- 'figures/diagnostics' 
pdf.f(box.site, file=file.path(path,
                  sprintf("%s.pdf", paste(type, "box", sep=""))),
      width=6, height=9)


pdf.f(resid.plot, file=file.path(path,
                    sprintf("%s.pdf", paste(type, "resid", sep=""))),
      width=6, height=9)



pdf.f(bysite.plot, file=file.path(path,
                     sprintf("%s.pdf", paste(type, "site", sep=""))),
      width=9, height=9)

