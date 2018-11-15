library(data.table)
library(rstan)
library(bayesplot)

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("ACE-SAC", species) & n+born > 0]
dat <- dat[complete.cases(dat$annual_mean_temp)]

plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3)

standata <- list(n = nrow(dat), ntrees = dat$n, 
	temperature = dat$annual_mean_temp)
model <- stan("code/3_iv_poisson.stan", data = standata, 
	iter=2000, chains=1)

samples <- as.matrix(model, pars=c("intercept", "slope"))
yhats <- as.matrix(model, pars = "lambda")
mcmc_trace(samples)

plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3)
ord <- order(dat$annual_mean_temp)
ci <- apply(yhats, 2, quantile, c(0.05, 0.95))
polygon(c(dat$annual_mean_temp[ord], rev(dat$annual_mean_temp[ord])),
	c(ci[1, ord], rev(ci[2, ord])), col="#ff000066", border=NA)
lines(dat$annual_mean_temp[ord], colMeans(yhats)[ord], 
	col='red', lwd=4)
