
vaznull.fast <- function(web) {
    rs.p <- rowSums(web)/sum(web)
    cs.p <- colSums(web)/sum(web)
    P <- P1 <- tcrossprod(rs.p, cs.p)
    finalmat <- matrix(0, nrow(web), ncol(web))
    n.int.finalmat <- 0
    while (n.int.finalmat < sum(dim(web))) {
        sel <- sample(1:length(web), 1, prob = P, replace = TRUE)
        selc <- floor((sel - 1)/(dim(web)[1])) + 1
        selr <- ((sel - 1)%%dim(web)[1]) + 1
        if (sum(finalmat[, selc]) == 0 | sum(finalmat[selr,
                                                      ]) == 0) {
            finalmat[sel] <- 1
            P[sel] <- 0
        }
        n.int.finalmat <- sum(rowSums(finalmat) > 0) + sum(colSums(finalmat) >
                                                           0)
    }
    conn.remain <- sum(web > 0) - sum(finalmat > 0)
    if (conn.remain > 0) {
        if (length(which(finalmat == 0)) == 1) {
            add <- which(finalmat == 0)
        }
        else {
            add <- sample(which(finalmat == 0), conn.remain,
                          prob = P1[finalmat == 0])
        }
        finalmat[add] <- 1
    }
    int.remain <- sum(web) - sum(finalmat)
    if (int.remain > 0) {
        add <- sample(which(finalmat > 0),
                      int.remain, prob = P1[which(finalmat >
                                                  0)], replace = TRUE)
        finalmat[as.numeric(names(table(add)))] <-
            finalmat[as.numeric(names(table(add)))] +
                table(add)
        }
        finalmat
    }
