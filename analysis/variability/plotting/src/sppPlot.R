makeMetricBySppPlot <- function(){
    spp.dats <- aggregate(list(mean.pca1 = spec.abund$mean.pca1),
                          list(GenusSpecies=spec.abund$GenusSpecies,
                               var.pca1=spec.abund$var.pca1,
                               beta.dist=spec.abund$beta.dist),
                          mean, na.rm=TRUE)

    spp.dats$beta.dist <- standardize(spp.dats$beta.dist)
    spp.dats$var.pca1 <- standardize(spp.dats$var.pca1)
    spp.dats$mean.pca1 <- standardize(spp.dats$mean.pca1)
      par(oma=c(14, 4, 0.5, 0.5),
      mar=c(2, 2, 2, 2))

    plot(NA,
         ylim=range(c(spp.dats$beta.dist,
                      spp.dats$var.pca1,
                      spp.dats$mean.pca1)),
         xlim=c(1, nrow(spp.dats)),
         xaxt="n", xlab="", ylab="")
    abline(v=1:(nrow(spp.dats) +1) - 0.5, lty="dashed", col="grey")


    points(x=1:nrow(spp.dats), y=spp.dats$beta.dist,
           pch=16)
    points(x=1:nrow(spp.dats), y=spp.dats$var.pca1,
           col= "darkred", pch=16)
    points(x=1:nrow(spp.dats), y=spp.dats$mean.pca1,
           col= "dodgerblue", pch=16)
    axis(1, at=1:nrow(spp.dats), labels=spp.dats$GenusSpecies, las=2)
    mtext("Metrics", 2, line=3)
        legend("topleft", legend=c("Network niche",
                                "Network niche variability",
                               "Partner variability"),
           pch=16,
           col=c("dodgerblue",
                 "darkred",
                 "black"),  box.lwd = 0,
           box.col = "white",
           bg = "white",
           cex=0.9)
}
