library(RColorBrewer)

plot.panel <- function(dats,
                       new.dd,
                       y1,
                       y2=y1,
                       xs,
                       treatments,
                       year=NA,
                       col.lines,
                       col.fill,
                       legend.loc.year=NA,
                       ylabel,
                       ag.col="SiteStatus",
                       plot.x=TRUE,
                       plot.y = TRUE,
                       pchs=c(15, 16, 17)){
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
            ## take means of ys
            ys <- aggregate(list(y=sub.dats[,y1]),
                            list(x=sub.dats[,xs]),
                            mean, na.rm=TRUE)

            points(x=jitter(ys$x, factor=0.25),
                   y=ys$y,
                   pch=pchs[i],
                   col=col.lines[i],
                   cex=1)
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
    if(!is.na(year)){
        plot(NA, xlim=range(dats[dats$Year == year, xs]),
             ylim=range(c(new.dd$plo, new.dd$phi, dats[,y1]),
                        na.rm=TRUE),
             xlab="",
             ylab="",
             xaxt="n",
             yaxt="n",
             las=1)
    }else{
        plot(NA, xlim=range(dats[, xs], na.rm=TRUE),
             ylim=range(c(new.dd$plo, new.dd$phi, dats[,y1]),
                        na.rm=TRUE),
             xlab="",
             ylab="",
             xaxt="n",
             yaxt="n",
             las=1)
    }
    if(plot.y){
        axis(2, pretty(c(dats[,y1], new.dd$plo, new.dd$phi), 4), las=1)
        mtext(ylabel, 2, line=4, cex=1.5)
    }
    if(plot.x){
            axis(1, pretty(dats[,xs], 4))
    }
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
                             dats,
                             y1,
                             y2=y1,
                             xs="",
                             legend.loc="bottomleft",
                             legend.loc.year="topleft",
                             by.year=FALSE,
                             x.adj=-0.5,
                             width=4, height=4,
                             type="all",
                             xlabel="Pyrodiversity",
                             f.path = 'figures'){
    plot.ci <- function(){
        col.lines <- rev(brewer.pal(4, "RdYlGn"))[c(2,3,4)]
        col.fill <- add.alpha(col.lines, alpha=0.3)
        treatments <- c("LOW", "MOD", "HIGH")
        if(by.year){
            layout(matrix(1:2, ncol=2))
            par(oma=c(6, 7, 2, 1),
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
                           ylabel)
            }
        } else{
            layout(matrix(1, ncol=1))
            par(oma=c(6, 6, 2, 1),
                mar=c(0.5, 0, 0.5, 1))
            plot.panel(dats, new.dd, y1, y2, xs,
                       treatments,
                       year=NA,
                       col.lines,
                       col.fill,
                       legend.loc.year=NA,
                       ylabel)
        }
        mtext(xlabel, 1, line=3, cex=1.5)
        legend(legend.loc,
               legend=c("Low", "Moderate", "High"),
               col=col.lines,
               pch=c(15,16,17), bty="n", cex=0.8)
    }
    pdf.f(plot.ci, file=file.path(f.path,
                                  sprintf("%s.pdf", paste(
                                                        type,
                                                        gsub(" ", "", ylabel),
                                                        xs,
                                                        sep="_"))),
          width=width, height=height)

}
