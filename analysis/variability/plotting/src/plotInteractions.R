library(viridis)

plotInteractionsPyroDiv <- function(){
    f <- function(){
        layout(matrix(c(1, 2, 3, 4), nrow=2, byrow=TRUE),
               heights=c(1,1,1, 1))
        par(oma=c(2, 5, 2, 1),
            mar=c(5, 2, 1, 1.5), cex.axis=1.5)
        nbreaks <- 15
        h1 <- hist(delta$beta.dist, breaks=nbreaks, plot=FALSE)
        h2 <- hist(delta$var.pca1, breaks=nbreaks, plot=FALSE)
        cols1 <- rev(viridis(length(h1$density)))
        cols2 <- rev(viridis(length(h2$density)))
        plot(h1, col=cols1,
             xlab="", main="", ylab="", las=1)
        mtext("Frequency", 2, line=4, cex=1.3)
        mtext("Partner variability", 1, line=3, cex=1.3)
        legend("topright", legend="(a)", bty="n", cex=1.2)

        abline(v=mean(delta$beta.dist), lty=2, col="red", lwd=3)
        plot(h2, col=cols2,
             xlab="", main="", ylab="", las=1)
        abline(v=mean(delta$var.pca1), lty=2, col="red", lwd=3)
        mtext("Role variability", 1, line=3, cex=1.3)
        legend("topright", legend="(b)", bty="n", cex=1.2)

        quantiles.partner <- (h1$mids - mean(delta$beta.dist))/sd(delta$beta.dist)
        quantiles.role <- (h2$mids - mean(delta$var.pca1))/sd(delta$var.pca1)

        means <- summary(mods)$coefficients[,"Estimate"]

        x <- max(delta$simpson.div)
        ylims1 <- range(means['(Intercept)'] +
                        means["scale(simpson.div)"] * x +
                        means["scale(beta.dist)"] * quantiles.partner +
                        means["scale(beta.dist):scale(simpson.div)"] * x *
                        quantiles.partner)
        ylims2 <- range(means['(Intercept)'] +
                        means["scale(simpson.div)"] * x +
                        means["scale(var.pca1)"] * quantiles.role +
                        means["scale(simpson.div):scale(var.pca1)"] * x *
                        quantiles.role)

        plot(NA, ylim=range(c(ylims1, ylims2)),
             xlim=range(delta$simpson.div), las=1,
             ylab="", xlab="")
        legend("topleft", legend="(c)", bty="n", cex=1.2)
        for(i in 1:length(quantiles.partner)){
            curve(means['(Intercept)'] +
                  means["scale(simpson.div)"] * x +
                  means["scale(beta.dist)"] * quantiles.partner[i] +
                  means["scale(beta.dist):scale(simpson.div)"] * x * quantiles.partner[i],
                  from=range(delta$simpson.div)[1],
                  to=range(delta$simpson.div)[2],
                  col=cols1[i],
                  lwd=2,
                  add=TRUE)
        }

        curve(means['(Intercept)'] +
              means["scale(simpson.div)"] * x,
              from=range(delta$simpson.div)[1],
              to=range(delta$simpson.div)[2],
              col="red",
              lty=2,
              lwd=2,
              add=TRUE)

        mtext("Pyrodiversity", 1, line=3, cex=1.3)
        mtext("Log Ratio \n  Abundance", 2, line=4, cex=1.3)

        ylims <- range(means['(Intercept)'] +
                       means["scale(simpson.div)"] * x +
                       means["scale(var.pca1)"] * quantiles.role +
                       means["scale(simpson.div):scale(var.pca1)"] * x *
                       quantiles.role)

        plot(NA, ylim=range(c(ylims1, ylims2)),
             xlim=range(delta$simpson.div), las=1,
             ylab="", xlab="")
        legend("topleft", legend="(d)", bty="n", cex=1.2)
        for(i in 1:length(quantiles.role)){
            curve(means['(Intercept)'] +
                  means["scale(simpson.div)"] * x +
                  means["scale(var.pca1)"] * quantiles.role[i] +
                  means["scale(simpson.div):scale(var.pca1)"] * x * quantiles.role[i],
                  from=range(delta$simpson.div)[1],
                  to=range(delta$simpson.div)[2],
                  col=cols2[i],
                  lwd=2,
                  add=TRUE)
        }

        curve(means['(Intercept)'] +
              means["scale(simpson.div)"] * x,
              from=range(delta$simpson.div)[1],
              to=range(delta$simpson.div)[2],
              col="red",
              lty=2,
              lwd=2,
              add=TRUE)


        mtext("Pyrodiversity", 1, line=3, cex=1.3)
    }
    path <- 'figures'
    pdf.f(f, file=file.path(path, "PyroDivFlexibility.pdf"),
          width=6, height=6)

}

