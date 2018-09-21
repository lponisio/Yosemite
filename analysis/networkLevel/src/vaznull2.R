 vaznull.fast <- function(web) {
        rs.p <- rowSums(web)/sum(web)
        cs.p <- colSums(web)/sum(web)
        P <- P1 <- tcrossprod(rs.p, cs.p)
        finalmat <- matrix(0, nrow(web), ncol(web))
        n.int.finalmat <- 0
        finalmat <- try(simulate(vegan::nullmodel(web,
                                                  method="quasiswap"),
                                 1)[,,1], silent=TRUE)
        if(inherits(finalmat, "try-error")) browser()

        conn.remain <- sum(web > 0) - sum(finalmat > 0)
        if (conn.remain > 0) {
            add <- sample(which(finalmat == 0), conn.remain,
                          prob = P1[finalmat == 0])
            finalmat[add] <- 1
        }
        int.remain <- sum(web) - sum(finalmat)
        if (int.remain > 0) {
            add <- sample(which(finalmat > 0), int.remain,
                          prob = P1[finalmat > 0], replace = TRUE)
            finalmat[as.numeric(names(table(add)))] <-
                finalmat[as.numeric(names(table(add)))] + table(add)
        }
        finalmat
    }
