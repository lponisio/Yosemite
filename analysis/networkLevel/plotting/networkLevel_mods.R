## setwd('~/Dropbox/Yosemite')
rm(list=ls())
setwd('analysis/networkLevel')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/diagnostics.R")

xvars <- c("simpson.div")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## network metrics
## ************************************************************

load('saved/mods/metrics.Rdata')

ys <- names(mods.div)
ylabs <- c("Plant functional \n complementarity",
           "Pollinator functional \n complementarity",
           "Reciprocal \n specialization (H2)",
           "Niche breadth")
names(ylabs) <- ys
x <- xvars


f <- function(){
    col.lines.2013 <- rep("black", length(ys))
    col.lines.2014 <-  rep("black", length(ys))
    col.fill.2013 <- add.alpha(col.lines.2013, alpha=0.3)
    col.fill.2013[3]  <- "white"
    col.fill.2014 <- rep("white", length(ys))

    years <- c("2013", "2014")
    treatments <- c("all")
    names(col.lines.2013) <- names(col.fill.2013) <- ys
    names(col.lines.2014) <- names(col.fill.2014) <- ys
    col.lines <- list(col.lines.2013, col.lines.2014)
    col.fill <- list(col.fill.2013, col.fill.2014)
    names(col.lines) <- names(col.fill) <- c("2013", "2014")

    leg.labs <- c("Drought",
                  "Extreme drought")
    names(leg.labs) <- years

    layout(matrix(1:8, ncol=2, byrow=TRUE))
    par(oma=c(6, 7, 2, 1),
        mar=c(0.5, 0, 1, 1), cex.axis=1.5)

    for(y in ys){
        print(y)
        ## create a matrix of possible variable values
        dd.met <- expand.grid(xvar =seq(
                                  from=  min(cor.dats[, x]),
                                  to= max(cor.dats[, x]),
                                  length=10),
                              SiteStatus= c("all"),
                              Year= years,
                              yvar=0)
        colnames(dd.met)[1] <- x
        colnames(dd.met)[4] <- y

        ## "predict" data using model coefficient to draw predicted
        ## relationship and CI
        met.pi <- predict.int(mod= mods.div[[y]],
                              dd=dd.met,
                              y=y,
                              family="guassian")
        cor.dats$SiteStatus <- "all"

        for(yr in years){
            cols <- col.lines[[yr]][y]
            cols.fill=col.fill[[yr]][y]
            names(cols) <- names(cols.fill) <- "all"
            plot.panel(dats=cor.dats,
                       new.dd=met.pi,
                       xs=x,
                       y1=y,
                       treatments=treatments,
                       year=yr,
                       col.lines=cols,
                       col.fill=cols.fill,
                       legend.loc.year="topleft",
                       ylabel= ylabs[y],
                       plot.x=FALSE,
                       leg.labs=leg.labs)
            if(y == "links.per.species"){
                axis(1, pretty(cor.dats[,x], 4))
                mtext(xlabel, 1, line=3, cex=1)
            }

        }
        plotDiag <- function(){
            plotDiagnostics(mods=mods.div[[y]], dats=cor.dats)
        }

        pdf.f(plotDiag,
              file=file.path('figures/diagnostics/',
                             sprintf('%s.pdf', y)),
              height=7, width=3)
    }
}


pdf.f(f, file=file.path("figures",
                        sprintf("%s.pdf", type)),
      width=5, height=8)
