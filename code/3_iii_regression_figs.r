alpha <- 1.565
beta <- 0.453
sigma <- 2

x <- runif(200, -10, 10)
y <- rnorm(length(x), alpha + beta * x, sigma)

par(mar=c(4.5,4.5,0.5,0.5), bty='n', cex.axis=1.2, cex.lab=1.3)
plot(x, y, pch=16, xlab="X", ylab="Y", bty='n')
lines(c(-10, 10), alpha + beta * c(-10,10), col='red', lwd=4)

log_lik <- function(alpha, beta, sigma, x, y) {
	y_hat <- alpha + beta * x
	ll <- sum(dnorm(y, y_hat, sigma, log=TRUE))
	return(ll)
}

library(rstan)
model <- stan("code/3_iii_regression_simple.stan", 
	data = list(x = x, y = y, n = length(x)), chains=1, iter=2000)
colMeans(as.matrix(model, pars=c('alpha', 'bet', 'sigma')))