floral.plot.panels <- function(){
  f <- function(){
    col.lines <- c("darkolivegreen3",
                   "darkgoldenrod1", "brown3")
    col.fill <- add.alpha(col.lines, alpha=0.2)
    treatments <- c("LOW", "MOD", "HIGH")

    layout(matrix(1:2, ncol=2, byrow=TRUE))
    par(oma=c(6, 7, 2, 1),
        mar=c(0.5, 0, 0.5, 1), cex.axis=1.5)
    
    ## abundance
    years <- c("2013", "2014")
    ## for(j in 1:length(years)){
    ##   year <- years[j]
    ##   plot.panel(dats=dat.mods,
    ##              new.dd=abund.pi.fr,
    ##              xs="s.FloralRichness",
    ##              y1="Abund", y2="Abund",
    ##              treatments=treatments,
    ##              year=year,
    ##              col.lines=col.lines,
    ##              col.fill=col.fill,
    ##              legend.loc.year="topleft",
    ##              ylabel= "Bee Abundance",
    ##              plot.x=FALSE)
    ## }
    ## richness
    for(j in 1:length(years)){
      year <- years[j]
      plot.panel(dats=dat.mods,
                 new.dd=richness.pi.fr,
                 xs="s.FloralRichness",
                 y1="Richness", y2="Richness",
                 treatments=treatments,
                 year=year,
                 col.lines=col.lines,
                 col.fill=col.fill,
                 ylabel= "Bee Richness",
                 plot.x=FALSE)
        axis(1, pretty(dat.mods[, "s.FloralRichness"], 4),
         labels= round((pretty(dat.mods[, "s.FloralRichness"], 4)*
           sd(dat.mods[,"FloralRichness"])) +
           mean(dat.mods[,"FloralRichness"]), 0))
    }
    legend("topleft",
           legend=c("Low", "Moderate","High"),
           col=col.lines,
           pch=16, bty="n", cex=1.2)
    mtext("Floral Resources", 1, line=3.5, cex=1.5, adj=-1)
  }
  path <- 'figures/ms' 
  pdf.f(f, file=file.path(path,
             sprintf("%s.pdf", "floral")),
        width=7.5, height=4)

}


