library(data.table)
trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

lik_func <- function(theta, n, k) {
	sum(dbinom(n, k, theta, log=TRUE))
}

optim(0.5, lik_func, n=dat$died, k=dat$n, method="Brent", lower=0, upper=1, control=list(fnscale=-1))

par(mar=c(4.5,4.5,0.2,0.2))
plot(seq(0,1,0.01), sapply(seq(0,1,0.01), lik_func, n=dat$died, k=dat$n), type='l', col='#0066cc', bty='n', xlab=expression(theta), ylab="Log Likelihood", lwd=3.5)
abline(v=0.147293, lty=2)
abline(h=lik_func(0.147293, n=dat$died, k=dat$n), lty=2)
text(0.147293, 4000, expression(paste(theta, "=", "0.147")), pos=4)

ll_count1 <- function(lambda, n) {
	sum(dpois(n, lambda, log=TRUE))
}

ll_count2 <- function(musize, n) {
	sum(dnbinom(n, mu=musize[1], size=musize[2], log=TRUE))
}

res1 <- optim(5, ll_count1, n=dat$n, lower=0, upper=max(dat$n), method='Brent',control=list(fnscale=-1))
res2 <- optim(c(5, 5), ll_count2, n=dat$n, control=list(fnscale=-1))



sim1 <- rpois(nrow(dat), res1$par)
sim2 <- rnbinom(nrow(dat), mu=res2$par[1], size=res2$par[2])

par(mfrow=c(1,2))
hist(sim1, xlim=range(dat$n), freq=FALSE, col="#c6b6e5", main="Poisson")
lines(density(dat$n), col="#c96840", lwd=4)
text(15, 0.1, paste("mean =", round(res1$par, 3), "\nvariance =", round(res1$par, 3)), pos=4, cex=1.3, col="#7e54bf")
text(15, 0.08, paste("mean =", round(mean(dat$n), 3), "\nvariance =", round(var(dat$n), 3)), pos=4, cex=1.3, col="#c96840")

hist(sim2, xlim=range(dat$n), freq=FALSE, col="#c6b6e5", main= "Negative Binomial")
lines(density(dat$n), col="#c96840", lwd=4)
text(15, 0.06, paste("mean =", round(res2$par[1], 3), "\nvariance =", round(res2$par[1] + res2$par[1]^2/res2$par[2], 3)), pos=4, cex=1.3, col="#7e54bf")
text(15, 0.05, paste("mean =", round(mean(dat$n), 3), "\nvariance =", round(var(dat$n), 3)), pos=4, cex=1.3, col="#c96840")


