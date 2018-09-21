plot.panels <- function(type, xabel, xs= "s.simpson.div"){
  f <- function(){
    col.lines <- rev(brewer.pal(4, "RdYlGn"))[c(2,3,4)]
    col.fill <- add.alpha(col.lines, alpha=0.3)
    treatments <- c("LOW", "MOD", "HIGH")

    layout(matrix(1:6, ncol=2, byrow=TRUE))
    par(oma=c(6, 7, 2, 1),
        mar=c(0.5, 0, 1, 1), cex.axis=1.5)

    ## abundance
    years <- c("2013", "2014")
    ## for(j in 1:length(years)){
    ##   year <- years[j]
    ##   plot.panel(dats=dat.mods,
    ##              new.dd=abund.pi,
    ##              xs=xs,
    ##              y1="Abund", y2="Abund",
    ##              treatments=treatments,
    ##              year=year,
    ##              col.lines=col.lines,
    ##              col.fill=col.fill,
    ##              legend.loc.year="topleft",
    ##              ylabel= "Bee Abundance",
    ##              plot.x=FALSE,
    ##              FUN=max)
    ## }
    ## legend("topright",
    ##        legend=c("Low", "Moderate","High"),
    ##        col=col.lines,
    ##        pch=16, bty="n", cex=1.9)
    ## richness
    for(j in 1:length(years)){
      year <- years[j]
      plot.panel(dats=dat.mods,
                 new.dd=richness.pi,
                 xs=xs,
                 y1="Richness", y2="Richness",
                 treatments=treatments,
                 year=year,
                 col.lines=col.lines,
                 col.fill=col.fill,
                 legend.loc.year="topleft",
                 ylabel= "Bee Richness",
                 plot.x=FALSE,
                 FUN=max)
    }
    legend("topright",
           legend=c("Low", "Moderate","High"),
           col=col.lines,
           pch=c(15, 16, 17), bty="n", cex=1.2)
    ## floral richness
    for(j in 1:length(years)){
      year <- years[j]
      plot.panel(dats=dat.mods,
                 new.dd=floralRichness.pi,
                 xs=xs,
                 y1="FloralRichness",
                 y2="FloralRichness",
                 treatments=treatments,
                 year=year,
                 col.lines=col.lines,
                 col.fill=col.fill,
                 ## legend.loc.year="topleft",
                 ylabel= "Floral Richness",
                 plot.x=FALSE,
                 FUN=max)
      ## axis(1, pretty(dat.mods[, xs], 4))
      ##   labels= round((pretty(dat.mods[, xs], 4)*
      ##     sd(dat.mods[,"simpson.div"])) +
      ## mean(dat.mods[,"simpson.div"]), 2))
    }

      for(j in 1:length(years)){
      year <- years[j]
      plot.panel(dats=dat.mods,
                 new.dd=InteractionRichness.pi,
                 xs=xs,
                 y1="InteractionRichness",
                 y2="InteractionRichness",
                 treatments=treatments,
                 year=year,
                 col.lines=col.lines,
                 col.fill=col.fill,
                 ylabel= "Interaction Richness",
                 plot.x=FALSE,
                 FUN=max)
      axis(1, pretty(dat.mods[, xs], 4))
    }

    mtext(xlabel, 1, line=3.5, cex=1.5, at=-1.8)
  }
  path <- 'figures/ms'
  pdf.f(f, file=file.path(path,
             sprintf("%s.pdf",
                     paste(type, strsplit(xs, '\\.')[[1]][2], sep="_"))),
        width=6, height=8)

}

