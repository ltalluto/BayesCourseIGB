library(mcmc)
library(data.table)
library(ggplot2)
library(bayesplot)

library(data.table)
trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

log_posterior <- function(pars, X) {
	mulog <- exp(pars[1]) ## we need to ensure these parameters are positive
	siglog <- exp(pars[2]) ## we need to ensure these parameters are positive
	sum(dlnorm(X, mulog, siglog, log=TRUE)) + ## likelihood
		dnorm(mulog, log(15.8), log(3), log=TRUE) + ## prior on the average of log(average temperature), based on global average temperature
		dexp(siglog, 0.1, log=TRUE) # uninformative prior of standard deviation
}

model <- metrop(log_posterior, initial = c(2, -0.3), nbatch=250, scale=c(1,1), X=dat$annual_mean_temp)
model$accept

## initial acceptance rate very low, so we decrease the scale and iterate until we find something that works
model <- metrop(model, nbatch=2000, scale=c(0.03,0.05), X=dat$annual_mean_temp)
model$accept

model <- metrop(model, nbatch=20000, scale=c(0.03,0.05), X=dat$annual_mean_temp)
samples <- exp(model$batch) ## because we actually tracked the log of the parameters
colnames(samples) <- c("logmu", "logsig")
mcmc_trace(samples)

## check the log mean and log sd against the original data
ln_mean <- function(lmu, lsig) exp(lmu + lsig^2/2)
ln_sd <- function(lmu, lsig)
	sqrt((exp(lsig^2) - 1) * exp(2 * lmu + lsig^2))

cbind(samples = c(median(ln_mean(samples[,1], samples[,2])),
	median(ln_sd(samples[,1], samples[,2]))), 
	data = c(mean(dat$annual_mean_temp), 
		sd(dat$annual_mean_temp)))

	


xx <- seq(2, 16, 0.01)
yy <- t(apply(samples, 1, function(x) dlnorm(xx, x[1], x[2])))
yy.qu <- t(apply(yy, 2, quantile, c(0.05, 0.95)))

hist(dat$annual_mean_temp, freq=F, xlab="Annual Mean Temperature", main="")
polygon(c(xx, rev(xx)), c(yy.qu[,1], rev(yy.qu[,2])), col="#66666666", border=NA)
apply(yy[sample(nrow(yy), 500),], 1, function(x) lines(xx, x, col="#c9684044", lwd=0.2))
lines(xx, colMeans(yy), col="#9f4f2d", lwd=3)

