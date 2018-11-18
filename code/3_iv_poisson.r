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
model_nb <- stan("code/3_iv_nbinom.stan", data = standata, 
	iter=4000, chains=1)

samples <- as.array(model, pars=c("intercept", "slope"))
samples_nb <- as.array(model_nb, pars=c("intercept", "slope", "phi"))
lambda <- as.matrix(model, pars = "lambda")
yhats <- as.matrix(model, pars = "yhat")
lambda_nb <- as.matrix(model_nb, pars = "lambda")
yhats_nb <- as.matrix(model_nb, pars = "yhat")
mcmc_trace(samples)

ord <- order(dat$annual_mean_temp)
ci <- apply(lambda, 2, quantile, c(0.05, 0.95))
pri <- apply(yhats, 2, quantile, c(0.05, 0.95))

ci_nb <- apply(lambda_nb, 2, quantile, c(0.05, 0.95))
pri_nb <- apply(yhats_nb, 2, quantile, c(0.05, 0.95))


par(mfrow=c(1,2), mar=c(4.5,4.5,0.5,0.5), bty='n', cex.lab=1.4, cex.axis=1.2)
plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3, xlab="", ylab="Number of Trees")
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(pri[1, ord], rev(pri[2, ord])), 
	col="#ff000033", border=NA)
polygon(c(dat$annual_mean_temp[ord], 
	rev(dat$annual_mean_temp[ord])),
	c(ci[1, ord], rev(ci[2, ord])), 
	col="#ff000099", border=NA)
lines(dat$annual_mean_temp[ord], colMeans(lambda)[ord], 
	col='red', lwd=1)

plot(n ~ annual_mean_temp, data=dat, pch=16, cex=0.3, xlab="", ylab="")
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
mtext("Annual Mean Temperature", side=1, outer=TRUE, cex=1.4, line=-1.5)


mcmc_hist(samples_nb)
mcmc_hist_by_chain(samples)
mcmc_dens_overlay(samples)
mcmc_violin(samples)

mcmc_intervals(samples, pars="intercept")
mcmc_areas(samples)

mcmc_scatter(samples)
mcmc_pairs(samples_nb)
mcmc_combo(samples, combo = c("dens", "trace"))