


makePanels <- function(mods, xvars, xlabel, ys,
                       not.sig.2013=NULL, not.sig.2014=NULL){


    plotNetworkMets <- function(){
        layout(matrix(1:6, ncol=2, byrow=TRUE))
        par(oma=c(6, 0, 2, 1),
            mar=c(0.5, 7, 1, 1), cex.axis=1.5)

        for(y in ys){
            print(y)
            ## create a matrix of possible variable values
            dd.met <- expand.grid(xvar =seq(
                                      from=  min(cor.dats[, x[y]]),
                                      to= max(cor.dats[, x[y]]),
                                      length=10),
                                  Year= years,
                                  yvar=0)
            colnames(dd.met)[1] <- x[y]
            colnames(dd.met)[3] <- y

            ## "predict" data using model coefficient to draw predicted
            ## relationship and CI
            met.pi <- predict.int(mod= mods[[y]],
                                  dd=dd.met,
                                  y=y,
                                  family="guassian")
            cor.dats$SiteStatus <- "all"

            plot.panel(dats=cor.dats,
                       new.dd=met.pi,
                       xs=x[y],
                       y1=y,
                       treatments=years,
                       year=NA,
                       col.lines=col.lines,
                       col.fill=col.fill,
                       ylabel= ylabs[y],
                       plot.x=FALSE,
                       pchs=pchs)
            if(y == "mean.number.of.links.LL" |
               y == "mean.number.of.links.HL" ){
                axis(1, pretty(cor.dats[,x[y]], 4))
                mtext(xlabel, 1, line=3, cex=1)
            }

            plotDiag <- function(){
                plotDiagnostics(mods=mods.div[[y]], dats=cor.dats)
            }

            pdf.f(plotDiag,
                  file=file.path('figures/diagnostics',
                                 sprintf('%s_%s.pdf', y, x[y])),
                  height=6, width=4)
        }
    }


    if(length(xvars) == 1){
        x <- rep(xvars, length(ys))
    } else{
        x <- rep(xvars, times=length(ys)/length(xvars))
    }
    names(x) <- ys
    years <- c("2013", "2014")
    treatments <- years
    col.lines.2013 <- rep("darkgoldenrod1", length(ys))
    col.lines.2014 <-  rep("midnightblue", length(ys))
    col.fill.2013 <- add.alpha(col.lines.2013, alpha=0.3)
    col.fill.2014 <- add.alpha(col.lines.2014, alpha=0.3)
    ## not stat sig
    if(!is.null(not.sig.2013)){
        col.fill.2013[c(not.sig.2013)]  <- add.alpha("white", alpha=0.001)
        col.fill.2014[c(not.sig.2014)]  <- add.alpha("white",
                                                     alpha=0.001)
    }
    names(col.lines.2013) <- names(col.fill.2013) <- ys
    names(col.lines.2014) <- names(col.fill.2014) <- ys
    col.lines <- list(col.lines.2013, col.lines.2014)
    col.fill <- list(col.fill.2013, col.fill.2014)
    names(col.lines) <- years
    names(col.fill) <- years
    pchs <- c(16,1)
    names(pchs) <- years

    pdf.f(plotNetworkMets, file=file.path("figures",
                                          sprintf("%s.pdf", xlabel)),
          width=6, height=7)
}

