
makePies <- function(){
    layout(matrix(1:length(delta.HR), nrow=3, byrow=TRUE))
    for(hr in delta.HR){
        this.delta <- hr$deltaAbund[is.finite(hr$deltaAbund)]
        this.delta <- this.delta[this.delta != 0]
        pie(c(sum(this.delta > 1),
              sum(this.delta < 1),
              sum(this.delta == 1)),
            labels=c(paste("Increased", sum(this.delta > 1)),
                     paste("Declined",  sum(this.delta < 1)),
                     paste("Stable", sum(this.delta == 1))),
            col=c("darkolivegreen", "red4","floralwhite"),
            main=unique(hr$Years))
    }
}

makeBoxes <- function(){
    delta.only <- lapply(delta.HR, function(x){
        x[, c("deltaAbund", "Years")]
    })

    delta.only <- do.call(rbind, delta.only)
    delta.only <- delta.only[is.finite(delta.only$deltaAbund),]
    delta.only <- delta.only[delta.only$deltaAbund != 0,]
    boxplot(delta.only$deltaAbund ~ delta.only$Years, las=2)
}

getMeanSD <- function(x){
    x <- x[is.finite(x[,"deltaAbund"]),]
    x <- x[x[, "deltaAbund"] != 0,]
    out <- c(mean=mean(x[,"deltaAbund"]),
             median=median(x[,"deltaAbund"]),
             sd=sd(x[,"deltaAbund"]))
    return(out)
}

makePointLine <- function(){
    yose.mean.sd <- getMeanSD(delta$delta)
    HR.mean.sd <- sapply(delta.HR, getMeanSD)
    par(oma=c(6.5, 8, 0.5, 1),
        mar=c(0.5, 0, 2.5, 1), cex.axis=1.5)
    years <- sapply(delta.HR, function(x) unique(x$Years))
    plot(y=HR.mean.sd["median",], x=1:ncol(HR.mean.sd),
         pch=16, type="o", xaxt="n", xlab="", ylab="")
    axis(1, at=1:ncol(HR.mean.sd), labels=rep("", ncol(HR.mean.sd)))
    abline(h=1, lty="dashed")
    points(y=yose.mean.sd["median"], x=ncol(HR.mean.sd)-1,
           pch=16, col="darkred")
    text(x=1:ncol(HR.mean.sd),
         y=par('usr')[3],
         srt = 45, adj=1.2,
         labels=years,
         xpd = NA,
         cex=1)
    mtext("Log ratio abundance", side=2,
          line=4, cex=1.5)

}
