target <- function(x) dnorm(x, 4, 2)
cand_pdf <- function(x) dunif(x,-30,30)
cand_rng <- function(n) runif(n, -30, 30)

rej_norm <- function(n) {	
	M <- target(4)/cand_pdf(4)
	result <- numeric(n)
	for(i in 1:n) {	
		accept <- FALSE
		while(!accept) {
			X <- cand_rng(1)
			p <- target(X) / (M * cand_pdf(X))
			P <- runif(1)
			accept <- (P < p)
		}
		result[i] <- X
	}
	return(result)
}

y <- rej_norm(1000)
xx <- seq(-2,10,0.01)
yy <- dnorm(xx, 4, 2)
plt <- hist(y)
plot(plt, freq=F, main="", ylim=c(0, max(plt$density, yy)))
lines(xx,yy , lwd=3.5, col="#c96840")

system.time(rej_norm(1000))
system.time(rnorm(1000, 4, 2))