
rm(list=ls())
setwd('analysis/network')
source('src/initialize.R')
source("src/misc.R")
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/diagnostics.R")

xvars <- c("simpson.div")
type <- "all"
xlabel <- "Pyrodiversity"

## ************************************************************
## ## robustness
## ## ************************************************************

ys <- "Robustness"
ylabs <- c("Co-extinction cascade resistance")

extinction.methods <- c("visit")
participants<- c("lower")
net.type <- c("obs", "potential")


col.lines.2013 <- rep("darkgoldenrod1", length(ys))
col.lines.2014 <-  rep("midnightblue", length(ys))
col.fill.2013 <- add.alpha("white", alpha=0.001)
col.fill.2014 <- add.alpha("white", alpha=0.001)
years <- c("2013", "2014")
treatments <- years
names(col.lines.2013) <- names(col.fill.2013) <- ys
names(col.lines.2014) <- names(col.fill.2014) <- ys
col.lines <- list(col.lines.2013, col.lines.2014)
col.fill <- list(col.fill.2013, col.fill.2014)
names(col.lines) <- years
names(col.fill) <- years
pchs <- c(16,1)
names(pchs) <- years

for(net in net.type){
    for(sp.level in participants){
        for(ex.method in extinction.methods){
            load(file.path('saved/mods/',
                           sprintf('robustness_%s_%s_%s.Rdata',
                                   ex.method, sp.level, net)))
            mods <- list(mod.div)
            for(j in 1:length(xvars)){
                x <- xvars[j]
                print(x)
                dd.met <- expand.grid(xvar =seq(
                                          from=  min(dat.mods[, x]),
                                          to= max(dat.mods[, x]),
                                          length=10),
                                      Year= c("2013", "2014"),
                                      yvar=0)
                colnames(dd.met)[1] <- x
                colnames(dd.met)[3] <- ys

                met.pi <- predict.int(mod= mods[[j]],
                                      dd=dd.met,
                                      y=ys,
                                      family="guassian")

                plot.ci <- function(){
                    layout(matrix(1, ncol=1))
                    par(oma=c(6, 5, 2, 1),
                        mar=c(0.5, 0, 0.5, 1))
                    plot.panel(dats=res,
                               new.dd=met.pi,
                               xs=x,
                               y1=ys,
                               treatments=years,
                               year=NA,
                               col.lines=col.lines,
                               col.fill=col.fill,
                               ylabel= ylabs,
                               plot.x=TRUE,
                               pchs=pchs)
                    mtext(xlabel, 1, line=3, cex=1.2)
                }

                pdf.f(plot.ci, file=file.path("figures",
                                              sprintf("%s_%s_%s.pdf",
                                              net, sp.level,
                                              ex.method)),
                           width=4.5, height=4)

                plotDiag <- function(){
                    plotDiagnostics(mods=mods[[j]], dats=res)
                }

                pdf.f(plotDiag,
                      file=file.path('figures/diagnostics',
                                     sprintf('robustness_%s_%s_%s.pdf',
                                             ex.method, sp.level, net)),
                      height=7, width=3)


            }
        }
    }
}

