plot.panel <- function(dats,
                       new.dd,
                       y1,
                       y2,
                       xs,
                       treatments,
                       year,
                       col.lines,
                       col.fill,
                       legend.loc.year=NA,
                       ylabel,
                       ag.col="SiteStatus",
                       pchs=c(15, 16, 17),...){
  plotting.loop <- function(){
    for(i in 1:length(treatments)){
      print(treatments[i])
      ## subset data to specific treatment
      sub.dd <- new.dd[new.dd[,ag.col]==treatments[i],]
      sub.dats <- dats[dats[,ag.col]==treatments[i],]
      ## subset to a specific year if year is TRUE
      if(!is.na(year)){
        sub.dd <- sub.dd[sub.dd$Year==year,]
        sub.dats <- sub.dats[sub.dats$Year==year,]
      }
      ## take means of ys for each plot
      ys <- aggregate(list(y=sub.dats[,y1]),
                      list(Site=sub.dats$Site,
                           x=sub.dats[,xs]),
                      mean)
      ## plots means
      points(x=jitter(ys$x, factor=0.25),
             y=ys$y,
             pch=pchs[i],
             col=col.lines[i],
             cex=1.5)
      ## plots CI
      lines(x=sub.dd[,xs],
            y=sub.dd[,y1],
            col=col.lines[i],
            lwd=2)
      lines(x=sub.dd[,xs],
            y=sub.dd$plo,
            col=col.lines[i],
            lty="dashed")
      lines(x=sub.dd[,xs],
            y=sub.dd$phi,
            col=col.lines[i],
            lty="dashed")
      ## add fill from ci.up to ci.lb
      polygon(c(sub.dd[,xs],
                rev(sub.dd[,xs])),
              c(sub.dd$phi,
                rev(sub.dd$plo)),
              col=col.fill[i], border=NA)
    }
  }
  plot(NA, xlim=range(dats[dats$Year == year, xs], na.rm=TRUE),
       ylim=range(c(new.dd$plo,
         new.dd$phi,
         mean(dats[, y1]),
         mean(dats[, y2]))),
       xlab="",
       ylab="",
       xaxt="n",
       yaxt="n",
       las=1,...)
  if(year == "2013" | is.na(year)){
    axis(2, pretty(c(new.dd[, "plo"], new.dd[, "phi"],
                     mean(dats[,y1]))), las=1)
    mtext(ylabel, 2, line=3.5, cex=1.5)
  }
  ## axis(1, pretty(new.dd[,xs], 4))
  plotting.loop()
  ## add year labels
  if(!is.na(year)){
    legend(legend.loc.year,
           legend=year,
           bty="n", cex=1.5)
  }
}


plot.predict.div <- function(new.dd,
                             ylabel,
                             xlabel="Day of the Year",
                             dats,
                             y1,
                             y2=y1,
                             xs,
                             legend.loc="bottomright",
                             legend.loc.year="topleft",
                             by.year=TRUE,
                             x.adj=-0.5,
                             width=8, height=5,...){
  plot.ci <- function(){
    col.lines <-  c("darkolivegreen3", "darkgoldenrod1", "brown3")
    col.fill <- add.alpha(col.lines, alpha=0.2)
    treatments <- c("LOW", "MOD", "HIGH")
    if(by.year){
      layout(matrix(1:2, ncol=2))
      par(oma=c(6, 5, 2, 1),
          mar=c(0.5, 0, 0.5, 1))
      years <- c("2013", "2014")
      for(j in 1:length(years)){
        year <- years[j]
        plot.panel(dats, new.dd, y1, y2, xs,
                   treatments,
                   year,
                   col.lines,
                   col.fill,
                   legend.loc.year,
                   ylabel,...)
        if(j==1){
          legend(legend.loc,
                 legend=c("Low", "Moderate", "High"),
                 col=col.lines,
                 pch=c(15, 16, 17), bty="n", cex=1.3)
        }
      }
    } else{
      layout(matrix(1, ncol=1))
      par(oma=c(6, 5, 2, 1),
          mar=c(0.5, 0, 0.5, 1))
      plot.panel(dats, new.dd, y1, y2, xs,
                 treatments,
                 year=NA,
                 col.lines,
                 col.fill,
                 legend.loc.year=NA,
                 ylabel,...)
    }
    mtext(xlabel, 1, line=3.5, cex=1.5, adj=x.adj)
    ## legend(legend.loc,
    ##        legend=c("Low", "Moderate", "High"),
    ##        col=col.lines,
    ##        pch=16, bty="n", cex=1.5)
  }
  path <- 'figures'
  pdf.f(plot.ci, file=file.path(path,
                   sprintf("%s.pdf", paste(
                     gsub(" ", "", ylabel),
                     "floral", sep="_"))),
        width=width, height=height)
}


box.sp <- function(by.site, by.sp){
  g <- function(){
    layout(matrix(1:2, ncol=1))
    par(oma=c(2,3.5,0.1,0.1), mar=c(0.5,0.5,0.3,0.3),
        mgp=c(0,1,0), cex.axis=1)
    boxplot(log(mean.sp$Abund) ~ mean.sp$Year,
            ylab="", xaxt="n")
    mtext("Floral Abundance (log)", 2, line=2.5, cex=1.2)
    boxplot(by.site$Richness ~ by.site$Year,
            ylab="")
    mtext("Floral Richness", 2, line=2.5, cex=1.2)
  }
  mean.sp <- aggregate(list(Abund=by.sp$logFlowerNum),
                       list(Year=by.sp$Year,
                            Site=by.sp$doy,
                            doy=by.sp$Site),
                       mean)
  file.name <- file.path('figures', "box_floral.pdf")
  pdf.f(g, file.name, height=5, width=3)
}
