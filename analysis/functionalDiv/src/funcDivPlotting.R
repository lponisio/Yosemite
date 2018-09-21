## plotting
add.alpha <- function(col, alpha=0.2){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))  
}

## plots densities
plot.densities <- function(all.null.func,
                           all.obs.func,
                           traits,
                           col.lines,
                           cols, plot.labs=FALSE){
  labs <- paste(letters[1:length(all.obs.func)], ")", sep="")
  for(i in 1:(length(all.null.func))){
    null.func <- all.null.func[[i]]
    obs.func <- all.obs.func[[i]]
    den.cont <- density(null.func['LOW',],
                        kernel="gaussian")
    den.HIGH <- density(null.func['MOD',],
                        kernel="gaussian")
    den.MOD <- density(null.func['HIGH',],
                       kernel="gaussian")
    plot(den.cont,
         xlim=range(c(den.cont$x, den.HIGH$x, den.MOD$x,
           null.func, obs.func)),
         ylim=c(0,80),
         col=col.lines[1],
         main='',
         xlab='',
         ylab='',
         yaxt='n',
         xaxt='n',
         cex.lab=1.5)
    if(i==2){
      ## plot(1,1, col="white", yaxt="n", xaxt="n", ylab="", xlab="")
      legend("topleft",
             legend=c('Low', 'Moderate', 'High'),
             col=col.lines, pch=16, cex=2, bty='n')
    }
    print(max(c(den.cont$y, den.HIGH$y, den.MOD$y)))
    if(plot.labs){
      mtext(labs[i], 3, line=-1.5, adj=0.98, cex=1.2)
    }
    axis(1,
         at=pretty(range(c(den.cont$x, den.HIGH$x,
           den.MOD$x)), n=4, min.n=3))
    polygon(den.cont, col=cols[1])
    abline(v=obs.func['LOW'], col=col.lines[1], lwd=2.5)
    
    mtext(traits[i], 1, line=3, cex=1.2)
    if(i == 1 | i == 4 ){
      mtext('Frequency', 2, line=4, cex=1.5)
      axis(2, at=pretty(c(0,80), las=1))
    }
    
    points(den.MOD, type='l',  col=col.lines[3])
    polygon(den.MOD, col=cols[3])
    abline(v=obs.func['HIGH']+0.002,  col=col.lines[3], lwd=2.5)

    points(den.HIGH, type='l',  col=col.lines[2])
    polygon(den.HIGH, col=cols[2])
    abline(v=obs.func['MOD'],  col=col.lines[2], lwd=2.5)
  }
}


## plots boxes
plot.boxes <- function(all.dats.func, statuses, traits, cols,
                       horizontal=FALSE, plot.labs=TRUE,...){
  statuses <- factor(statuses,
                     levels=c("LOW", "MOD", "HIGH"))
  labs <- paste(letters[1:length(all.dats.func)], ")", sep="")
  for(i in 1:length(all.dats.func)){
    dats.func <- all.dats.func[[i]]
    boxplot(dats.func ~ statuses, col=cols,
            xlab='', ylab='',
            names=c('Low', 'Moderate', 'High'),
            cex.lab=1.5,
            las=1,
            horizontal=horizontal,...)
    if(!horizontal){
      mtext(traits[i], 2, line=4.5, cex=1.2)
    }
    ## add a-f to panels
    if(plot.labs){
      mtext(labs[i], 3, line=-1.5, adj=0.99, cex=1)
    }
  }
}


## implments density plot for all traits
plot.func <- function(all.null.func,
                      all.obs.func,
                      sub='all',
                      traits,
                      occ){
  f <- function(){
    layout(matrix(1:6, nrow=2, byrow=TRUE))
    par(oma=c(2,6,1,1), mar=c(4,1,2,1), mgp=c(2,1,0),
        cex.axis=1.5)
    ## fg="white", col.axis="white")
    col.lines <- c("darkolivegreen3", "darkgoldenrod1", "brown3")
    cols <- add.alpha(col.lines)
    plot.densities(all.null.func,
                   all.obs.func,
                   traits,
                   col.lines,
                   cols)
  }
  path <- 'figures'
  pdf.f(f, file= file.path(path, sprintf('%s.pdf',
             paste(sub, occ, sep='_'))),
        width=9.5, height=7)
}

## impliments box plots for all traits
plot.box <- function(all.dats.func, statuses,
                     sub='all', traits, occ){
  f <- function(){
    layout(matrix(1:6, nrow=2, byrow=TRUE))
    par(oma=c(2,6,1,1), mar=c(3,6,2,2), mgp=c(2,1,0),
        cex.axis=1.5)
    cols <- c("darkolivegreen3", "darkgoldenrod1", "brown3")
    plot.boxes(all.dats.func, statuses, traits, cols) 
  }
  path <- 'figures'
  pdf.f(f, file= file.path(path, sprintf('%s.pdf',
             paste('box', sub, occ, sep='_'))),
        width=12, height=9.5)
}

## plots quantiles for all traits
plot.quantile <- function(site.pvals,
                          statuses,
                          sub='all',
                          cases, occ,
                          file.name="quantiles",
                          r.margin.width=1,
                          fig.height=5) {
  loaded <- do.call(rbind, site.pvals)
  f <- function() {
    cols <- c("darkolivegreen3", "darkgoldenrod1", "brown3")
    layout(matrix(1, 1, 1, byrow=TRUE))
    par(oma=c(2,0.25,0.25,r.margin.width), mar=c(1, 0, 0, 0),
        mgp=c(0,-0.1,0), tcl=0.25, cex.axis=1)
    ## fg="white", col.axis="white")
    plot(NA, xlim=c(0,1), ylim=c(0, length(unlist(site.pvals)) +
                            length(cases)-0.5),
         yaxt='n', xlab='', ylab='')
    ## make coordinates for points and lines
    to <- cumsum(sapply(site.pvals, length)) + 0:(length(site.pvals)-1)
    from <- mapply(function(a, b) a-(length(b)-1), to, site.pvals)
    y.pos <- as.vector(mapply(function(a,b) a:b, from, to))
    ## add points and error bars
    points(x=unlist(site.pvals), y=y.pos, pch=16, cex=1.1, col=cols)
    ## add vertical and horizontal lines
    abline(v=c(0.025, 0.975), col='gray', lty=2, lwd=2)
    sapply((from-1)[-1], function(x)
           abline(h=x, col='black', lty=1))
    ## add pabel labels
    text(cases, x=c(0.15, 0.25, 0.25, 0.2, 0.15, 0.2), y=to+0.5, cex=0.6)
    ## add x-axis label
    mtext('Quantile of the Null Distribution',
          side=1, line=1, cex=1)
    
  }
  path <- 'figures'
  pdf.f(f, file=file.path(path, sprintf('%s.pdf', file.name)),
        width=3.5, height=fig.height)
}
