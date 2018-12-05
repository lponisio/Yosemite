plotDiagnostics  <- function(mods, dats){
    layout(matrix(1:4))
    plot(fitted(mods), residuals(mods),
         xlab = "Fitted Values", ylab = "Residuals",
    main="Residuals ~ fitted")
    abline(h=0, lty=2)
    lines(smooth.spline(fitted(mods),
                        residuals(mods)))

    hist(residuals(mods))

    boxplot(residuals(mods) ~ Site,
            data = dats, main = "Site",
            ylab = "Residuals")


    boxplot(residuals(mods) ~ GenusSpecies,
            data = dats, main = "GenusSpecies",
            ylab = "Residuals")

}
