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
ylabs <- c("Plant \n partner diversity",
           "Pollinator \n  partner diversity",
           "Plant functional \n complementarity",
           "Pollinator functional \n complementarity",
           "Reciprocal \n specialization (H2)",
           "Niche breadth")
names(ylabs) <- ys
x <- xvars


f <- function(){
    col.lines <- "black"
    col.fill <- add.alpha(col.lines, alpha=0.3)
    treatments <- c("all")
    names(col.lines) <- names(col.fill) <- treatments
    leg.labs <- c("Drought",
                  "Extreme drought")
    names(leg.labs) <- c("2013", "2014")

    layout(matrix(1:8, ncol=2, byrow=TRUE))
    par(oma=c(6, 7, 2, 1),
        mar=c(0.5, 0, 1, 1), cex.axis=1.5)

    for(y in ys){
        print(y)
        dd.met <- expand.grid(xvar =seq(
                                  from=  min(cor.dats[, x]),
                                  to= max(cor.dats[, x]),
                                  length=10),
                              SiteStatus= c("all"),
                              Year= c("2013", "2014"),
                              yvar=0)
        colnames(dd.met)[1] <- x
        colnames(dd.met)[4] <- y

        met.pi <- predict.int(mod= mods.div[[y]],
                              dd=dd.met,
                              y=y,
                              family="guassian")
        cor.dats$SiteStatus <- "all"

        years <- c("2013", "2014")
        for(yr in years){
            plot.panel(dats=cor.dats,
                       new.dd=met.pi,
                       xs=x,
                       y1=y,
                       treatments=treatments,
                       year=yr,
                       col.lines=col.lines,
                       col.fill=col.fill,
                       legend.loc.year="topleft",
                       ylabel= ylabs[y],
                       plot.x=FALSE,
                       leg.labs=leg.labs)
            if(y == "functional.complementarity.HL" | y == "links.per.species"){
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
