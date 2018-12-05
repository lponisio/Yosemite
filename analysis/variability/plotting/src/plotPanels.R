plot.panels <- function(){
    f <- function(){
        col.lines <- rev(brewer.pal(4, "RdYlGn"))[c(2,3,4)]
        col.fill <- add.alpha(col.lines, alpha=0.3)
        treatments <- c("LOW", "MOD", "HIGH")
        layout(matrix(1:4, ncol=2))
        par(oma=c(6, 8, 2, 1),
            mar=c(0.5, 0, 1, 1), cex.axis=1.5)


        plot.panel(dats=
                       delta$delta[delta$delta$deltaAbund != 0,],
                   new.dd=abund.beta.dist.pi,
                   xs="beta.dist",
                   y1="deltaAbund",
                   treatments=treatments,
                   col.lines=col.lines,
                   col.fill=col.fill,
                   ## ylabel= expression(delta ~ "Abundance"),
                   ylabel= "Log Ratio \n Abundance",
                   plot.x=FALSE,
                   plot.y=TRUE)


        legend("topleft",
               legend="a)",
               bty="n", cex=1)

        plot.panel(dats=
                       delta$delta,
                   new.dd=persist.beta.dist.pi,
                   xs="beta.dist",
                   y1="Persist",
                   treatments=treatments,
                   col.lines=col.lines,
                   col.fill=col.fill,
                   ylabel= "Persistence",
                   plot.x=TRUE,
                   plot.y=TRUE)
        legend("topleft",
               legend="c)",
               bty="n", cex=1)

        mtext("Partner variability", 1, line=3.5, cex=1.5)

        plot.panel(dats=
                       delta$delta[delta$delta$deltaAbund != 0,],
                   new.dd=abund.var.pca.pi,
                   xs="var.pca1",
                   y1="deltaAbund",
                   treatments=treatments,
                   col.lines=col.lines,
                   col.fill=col.fill,
                   ylabel= "Log Ratio \n Abundance",
                   plot.x=FALSE,
                   plot.y=FALSE)
        legend("topleft",
               legend="b)",
               bty="n", cex=1)
        legend("topright",
               legend=c("Low", "Moderate","High"),
               col=col.lines,
               pch=c(15, 16, 17), cex=0.7)

        plot.panel(dats=
                       delta$delta,
                   new.dd=persist.var.pca.pi,
                   xs="var.pca1",
                   y1="Persist",
                   treatments=treatments,
                   col.lines=col.lines,
                   col.fill=col.fill,
                   ylabel= "Proportion \n persistence",
                   plot.x=TRUE,
                   plot.y=FALSE)
        legend("topleft",
               legend="d)",
               bty="n", cex=1)


        mtext("Role variability", 1, line=3.5, cex=1.5)

    }
    path <- 'figures'
    pdf.f(f, file=file.path(path, "flexibility.pdf"),
          width=6, height=6)

}

