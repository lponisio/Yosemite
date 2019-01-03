source('src/initialize-delta.R')
source("plotting/src/predictIntervals.R")
source("plotting/src/CIplotting.R")
source("plotting/src/plotPanels.R")
source("plotting/src/diagnostics.R")
source("plotting/src/plotInteractions.R")


plotDeltaDiag <- function(){
    plotDiagnostics(mods=mods,
                    dats=delta[delta$deltaAbund != 0,])
}

plotPersistDiag <- function(){
    plotDiagnostics(mods=mods.ext, dats=delta)
}
