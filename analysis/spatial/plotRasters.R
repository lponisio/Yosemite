library(raster)
library(spatstat)
library(RColorBrewer)
library(viridis)
library(plotrix)

setwd("~/Dropbox/Yosemite/")
setwd("analysis/spatial")
source("misc.R")
load('../data/pyrodiv_buffers/byFire.Rdata')
load('../data/pyrodiv_buffers/plots.Rdata')
load('../data/pyrodiv_buffers/allRast.Rdata')
### plot fires individually

fire.rasters <- fire.rasters[order(names(fire.rasters))]
cols <- viridis(length(unique(pyro.rast)) -1)


plot.fires <- function(){
    to.crop <-  extent(plots) + c(-4000, 4000, -6000, 6000)
    g.cells <- apply(bbox(to.crop),1, diff)/30
    par(oma=c(2, 0.5, 2, 6),
        mar=c(0.5, 0.5, 0.5, 0.5),
        cex.axis=1.5)

    for(fire in names(fire.rasters)){
        plot(crop(fire.rasters[[fire]], to.crop),
             col=c("white", rev(brewer.pal(4, "RdYlGn"))),
             yaxt="n",
             xaxt="n",
             legend=FALSE)
        grid(g.cells[1], g.cells[2])
    }

    plot(crop(pyro.rast, to.crop),
         col=c("white", cols),
         yaxt="n",
         xaxt="n",
         legend=FALSE)
    grid(g.cells[1], g.cells[2])

}

pdf.f(plot.fires, file= file.path("fires.pdf"),
      width=4, height=3)

plot.pyro <- function(){
  par(oma=c(0.5, 0.5, 0.5, 0.5),
      mar=c(2, 2, 2, 2),
      cex.axis=1.5)

  p2 <- apply(coordinates(plots), 1, disc, radius=150)
  p2 <- lapply(p2, function(x) {
    x <- as(x, 'SpatialPolygons')
    proj4string(x) <- CRS(proj4string(plots))
    return(x)
  })

  new.rast <- crop(pyro.rast, extent(plots) +2000)
  plot(new.rast,
       yaxt="n",
       xaxt="n",
       col=c("white", cols),
       legend=FALSE)

  gradient.rect(xleft=44000, ybottom=-40200,
                xright=46000, ytop=-40000,
                col=cols)
  points(plots, pch=16, cex=0.5, col="black")
  points(plots, cex=0.5, col="grey")

  lapply(p2, plot, add=TRUE, border="grey", lwd=1)
}

pdf.f(plot.pyro, file= file.path("pyrodiv.pdf"),
      width=6, height=5)


names(cols) <- unique(pyro.rast)[-1]

plotHist <- function(){
      par(oma=c(0.5, 4, 0.5, 0.5),
      mar=c(2, 2, 2, 2),
      cex.axis=1.5)
    hist(pyro.rast[pyro.rast != 0], breaks= unique(pyro.rast)[-1],
         col=cols,
         freq=TRUE,
         xlab="",
         ylab="",
         ## xaxt="n",
         las=1,
         main="")
    ## mtext("Frequency", 2, line=4.5, cex=1.5)
    ## mtext("Unique fire histories", 1, line=1, cex=1.5)
}

pdf.f(plotHist, file= file.path("classes.pdf"),
      width=20, height=7)
