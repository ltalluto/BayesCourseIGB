library(data.table)
library(mvtnorm)
library(bayesplot)

### note this script also requires results (samples_nb) from 3_iv_poisson.r

trees <- readRDS("data/trees.rds")
table(trees$species)

dat <- trees[grepl("ABI-BAL|THU-OCC|BET-PAP|PIC-MAR|PIN-BAN|POP-TRE|PIC-GLA|PIC-RUB", species) & n+born > 0]
dat <- dat[complete.cases(dat$annual_mean_temp)]
quartz()
dat$sp_integer <- as.integer(factor(dat$species))
sp <- unique(dat$sp_integer)
hist(dat[, annual_mean_temp], col=1, xlim=range(dat$annual_mean_temp), freq=F, ylim=c(0,0.5))
for(spp in sp) {
	par(new=T)
	lines(density(dat[sp_integer==spp, annual_mean_temp]), col=as.integer(spp))
}

standata <- list(n = nrow(dat), nsp = length(sp), ntrees = dat$n, species = dat$sp_integer,
	temperature = dat$annual_mean_temp)
model <- stan("code/5_ii_hierarchical.stan", data = standata, 
	iter=4000, chains=1)

samples <- as.array(model, pars=c("intercept", "slope"))
mcmc_trace(samples)
lambda <- as.matrix(model, pars = "lambda")
yhats <- as.matrix(model, pars = "yhat")

ci <- apply(lambda, 2, quantile, c(0.05, 0.95))
pri <- apply(yhats, 2, quantile, c(0.05, 0.95))

par(mfrow=c(2,4), mar=c(4.5,4.5,0.5,0.5), bty='n', cex.lab=1.4, cex.axis=1.2)
for(spp in sp) {
	ord <- order(dat[sp_integer == spp, annual_mean_temp])
	plot(n ~ annual_mean_temp, data=dat[sp_integer == spp], pch=16, cex=0.3, xlab="", ylab="Number of Trees")
	polygon(c(dat[sp_integer == spp, annual_mean_temp][ord], 
		rev(dat[sp_integer == spp, annual_mean_temp][ord])),
		c(pri[1, dat$sp_integer == spp][ord], rev(pri[2, dat$sp_integer == spp][ord])), 
		col="#ff000033", border=NA)
	polygon(c(dat[sp_integer == spp, annual_mean_temp][ord], 
		rev(dat[sp_integer == spp, annual_mean_temp][ord])),
		c(ci[1, dat$sp_integer == spp][ord], rev(ci[2, dat$sp_integer == spp][ord])), 
		col="#ff000099", border=NA)
	lines(dat[sp_integer == spp, annual_mean_temp][ord], colMeans(lambda[,dat$sp_integer == spp])[ord], 
		col='red', lwd=1)


}
