library(data.table)
library(mvtnorm)
library(bayesplot)

### note this script also requires results (samples_nb) from 3_iv_poisson.r

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("ACE-SAC", species) & n+born > 0]
dat <- dat[complete.cases(dat$annual_mean_temp)]

log_posterior <- function(params, data) {
	intercept <- params[1]
	slope <- params[2]
	phi <- exp(params[3])

	# using a log link function, hence exp()
	mu <- exp(intercept + slope * data$annual_mean_temp)

	loglik <- sum(dnbinom(data$n, mu = mu, size = phi, log=TRUE))
	logprior <- dnorm(intercept, 0, 50, log = TRUE) + 
		dnorm(slope, 0, 30, log=TRUE) + 
		dexp(phi, 0.1, log=TRUE)
	return(loglik + logprior)
}

initial_values <- c(0, 0, 1)
fit <- optim(initial_values, log_posterior, control = list(fnscale = -1), hessian = TRUE, data = dat)

fit$par ## remember to exp() the last parameter
colMeans(samples_nb)

vcv_mat <- solve(-fit$hessian)

samples_nb_la <- rmvnorm(5000, fit$par, vcv_mat)
samples_nb_la[,3] <- exp(samples_nb_la[,3])
colnames(samples_nb_la) <- c("intercept", "slope", "phi")
rbind(apply(samples_nb_la, 2, sd), apply(samples_nb[,1,], 2, sd))

mcmc_pairs(samples_nb_la)

predictions <- apply(samples_nb_la, 1, function(x) rnbinom(nrow(dat), mu = exp(x[1] + x[2] * dat$annual_mean_temp), size = x[3]))
predictions_mu <- apply(samples_nb_la, 1, function(x) exp(x[1] + x[2] * dat$annual_mean_temp))


par(mfrow=c(1,2), mar=c(4.5,4.5,0.5,0.5), bty='n', cex.lab=1.4, cex.axis=1.2)
plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3, xlab="", ylab="Number of Trees")
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(pri_nb[1, ord], rev(pri_nb[2, ord])), 
	col="#0000ff33", border=NA)
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(ci_nb[1, ord], rev(ci_nb[2, ord])), 
	col="#0000ff99", border=NA)
lines(dat$annual_mean_temp[ord], colMeans(lambda_nb)[ord], 
	col='blue', lwd=1)

quant_pi_la <- apply(predictions, 1, quantile, c(0.05, 0.9))
quant_la <- apply(predictions_mu, 1, quantile, c(0.05, 0.9))
plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3, xlab="", ylab="")
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(quant_pi_la[1, ord], rev(quant_pi_la[2, ord])), 
	col="#0066ff33", border=NA)
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(quant_la[1, ord], rev(quant_la[2, ord])), 
	col="#0066ff99", border=NA)
lines(dat$annual_mean_temp[ord], rowMeans(predictions_mu)[ord], 
	col='#0066ff', lwd=1)
mtext("Annual Mean Temperature", side=1, outer=TRUE, cex=1.4, line=-1.5)
