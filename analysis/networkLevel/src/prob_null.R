prob.null <- function(comm) {
	fill <- sum(comm > 0)
	
	nr <- rowSums(comm)
	nc <- colSums(comm)
	
	probs <- expand.grid(nr,nc)
	probs <- probs[,1]*probs[,2]
	
	sim.cell <- sample(length(probs),fill,prob=probs)
	
	out <- numeric(length(probs))
	out[sim.cell] <- 1
	
	return(matrix(out,ncol=ncol(comm)))
}
