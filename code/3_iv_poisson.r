library(data.table)
library(rstan)
library(bayesplot)
options(mc.cores = parallel::detectCores())

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("ACE-SAC", species) & n+born > 0]
dat <- dat[complete.cases(dat$annual_mean_temp)]

plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3)

standata <- list(n = nrow(dat), ntrees = dat$n, 
	temperature = dat$annual_mean_temp)
model <- stan("code/3_iv_poisson.stan", data = standata, 
	iter=4000, chains=4)

samples <- as.array(model, pars=c("intercept", "slope"))
lambda <- as.matrix(model, pars = "lambda")
yhats <- as.matrix(model, pars = "yhat")
mcmc_trace(samples)

plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3, ylim=c(0,30))
ord <- order(dat$annual_mean_temp)
ci <- apply(lambda, 2, quantile, c(0.05, 0.95))

ord2 <- order(dat$annual_mean_temp)
pri <- apply(yhats, 2, quantile, c(0.05, 0.95))

polygon(c(dat$annual_mean_temp[ord2], 
	rev(dat$annual_mean_temp[ord2])),
	c(pri[1, ord2], rev(pri[2, ord2])), 
	col="#0000ff66", border=NA)
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(ci[1, ord], rev(ci[2, ord])), 
	col="#ff000066", border=NA)
lines(dat$annual_mean_temp[ord], colMeans(lambda)[ord], 
	col='red', lwd=1)

mcmc_hist(samples)
mcmc_hist_by_chain(samples)
mcmc_dens_overlay(samples)
mcmc_violin(samples)

mcmc_intervals(samples, pars="intercept")
mcmc_areas(samples)

mcmc_scatter(samples)
mcmc_pairs(samples)
mcmc_combo(samples, combo = c("dens", "trace"))