floral.plot.panels <- function(){
  f <- function(){
    col.lines <- rev(brewer.pal(4, "RdYlGn"))[c(2,3,4)]
    col.fill <- add.alpha(col.lines, alpha=0.2)
    treatments <- c("LOW", "MOD", "HIGH")

    layout(matrix(1:2, ncol=2, byrow=TRUE))
    par(oma=c(6, 7, 2, 1),
        mar=c(0.5, 0, 0.5, 1), cex.axis=1.5)

    ## abundance
    years <- c("2013", "2014")
    for(j in 1:length(years)){
      year <- years[j]
      plot.panel(dats=veg,
                 new.dd=abund.pi.sp,
                 xs="s.doy",
                 y1="logFlowerNum", y2="maxFlower",
                 treatments=treatments,
                 year=year,
                 col.lines=col.lines,
                 col.fill=col.fill,
                 legend.loc.year="topleft",
                 ylabel= "Floral Abundance (log)",
                 plot.x=FALSE)
      axis(1, pretty(by.site[, "s.doy"], 6),
           labels= round((pretty(by.site[, "s.doy"], 6)*
             sd(by.site[,"doy"])) +
             mean(by.site[,"doy"]), 0))
    }
         legend("topright",
               legend=c("Low", "Moderate","High"),
               col=col.lines, bg="white",
               pch=c(15, 16, 17), bty="n", cex=1.2)
    ## richness
    ## for(j in 1:length(years)){
    ##   year <- years[j]
    ##   plot.panel(dats=by.site,
    ##              new.dd=richness.pi,
    ##              xs="s.doy",
    ##              y1="Richness", y2="Richness",
    ##              treatments=treatments,
    ##              year=year,
    ##              col.lines=col.lines,
    ##              col.fill=col.fill,
    ##              ylabel= "Floral Richness",
    ##              plot.x=FALSE)
    ##     axis(1, pretty(by.site[, "s.doy"], 4),
    ##      labels= round((pretty(by.site[, "s.doy"], 4)*
    ##        sd(by.site[,"doy"])) +
    ##        mean(by.site[,"doy"]), 0))
    ## }

    mtext("Day of the Year", 1, line=3.5, cex=1.5, adj=-0.8)
  }
  path <- 'figures'
  pdf.f(f, file=file.path(path,
             sprintf("%s.pdf", "floral")),
        width=8, height=5)

}


