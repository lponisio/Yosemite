plot.panel <- function(dats,
                       new.dd,
                       y1,
                       y2,
                       xs,
                       treatments,
                       year,
                       col.lines,
                       col.fill,
                       legend.loc.year=NULL,
                       ylabel,
                       ag.col="Year",
                       plot.x=TRUE,
                       pchs=c(16),
                       leg.labs=NULL){
    plotting.loop <- function(){
        for(i in treatments){
            print(i)
            ## subset data to specific treatment
            sub.dd <- new.dd[new.dd[,ag.col]==i,]
            sub.dats <- dats[dats[,ag.col]==i,]
            this.fill <- col.fill[[i]][y1]
            this.line <- col.lines[[i]][y1]
            ## subset to a specific year if year is TRUE
            if(!is.na(year)){
                sub.dd <- sub.dd[sub.dd$Year==year,]
                sub.dats <- sub.dats[sub.dats$Year==year,]
            }
            ## take means of ys for each plot
            ys <- aggregate(list(y=sub.dats[,y1]),
                            list(x=sub.dats[,xs]),
                            mean, na.rm=TRUE)
            ## add fill from ci.up to ci.lb
            polygon(c(sub.dd[,xs],
                      rev(sub.dd[,xs])),
                    c(sub.dd$phi,
                      rev(sub.dd$plo)),
                    col=this.fill, border=NA)
            ## plots means
            points(x=jitter(ys$x, factor=0.25),
                   y=ys$y,
                   pch=pchs[i],
                   col=this.line,
                   cex=1.2)
            ## plots CI
            lines(x=sub.dd[,xs],
                  y=sub.dd[,y1],
                  col=this.line,
                  lwd=2)
            lines(x=sub.dd[,xs],
                  y=sub.dd$plo,
                  col=this.line,
                  lty="dashed")
            lines(x=sub.dd[,xs],
                  y=sub.dd$phi,
                  col=this.line,
                  lty="dashed")

        }
    }
    if(!is.na(year)){
        plot(NA, xlim=range(dats[dats$Year == year, xs]),
             ylim=range(c(0, new.dd$plo, new.dd$phi, dats[,y1]),
                        na.rm=TRUE),
             xlab="",
             ylab="",
             xaxt="n",
             yaxt="n",
             las=1)
    }else{
        plot(NA, xlim=range(dats[, xs], na.rm=TRUE),
             ylim= range(c(0, new.dd$plo, new.dd$phi, dats[,y1]),
                         na.rm=TRUE),
             xlab="",
             ylab="",
             xaxt="n",
             yaxt="n",
             las=1)
    }
    if(is.na(year)){
        axis(2, pretty(range(c(0, new.dd$plo, new.dd$phi, dats[,y1]),
                             na.rm=TRUE),
                       n = 5),
             las=1)
        mtext(ylabel, 2, line=4, cex=1)
    }
    if(plot.x){
        axis(1, pretty(dats[,xs], 4))
    }
    plotting.loop()
    if(y1 == "pol.FunRedundancy" | y1 == "Robustness"){
        legend("bottomleft", legend=c("Drought", "Extreme drought"),
               col=c("darkgoldenrod1", "midnightblue"),
               pch=c(16, 1), bty="n")
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
                             by.year=TRUE,
                             x.adj=-0.5,
                             width=7, height=4.5,
                             type="all",
                             xlabel="Pyrodiversity",
                             f.path = 'figures',
                             leg.labs){
    plot.ci <- function(){
        col.lines <- "black"
        col.fill <- add.alpha("white", alpha=0.3)
        treatments <- c("all")
        names(col.lines) <- names(col.fill) <- treatments
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
                           ylabel,
                           leg.labs=leg.labs)
                mtext(xlabel, 1, line=3, cex=1)
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
                       ylabel,
                       leg.labs=leg.labs)
            mtext(xlabel, 1, line=3, cex=1)
        }
    }

    pdf.f(plot.ci, file=file.path(f.path,
                                  sprintf("%s.pdf", paste(
                                                        type,
                                                        gsub(" ", "", ylabel),
                                                        xs,
                                                        sep="_"))),
          width=width, height=height)

}
