library(data.table)
library(rstan)
library(bayesplot)

library(data.table)
trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

plot(died/n ~ annual_mean_temp, data=dat, pch=16, cex=1.5, 
	col="#34A5DA", xlab = "Annual Mean Temperature", ylab = "Proportion Dead")

standata <- list(n = nrow(dat), ntrees = dat$n, died = dat$died,
	temperature = dat$annual_mean_temp)
model <- stan("code/4_ii_stan_glm.stan", data = standata, 
	iter=5000, chains=1)

samples <- as.matrix(model, pars=c("intercept", "slope"))

# some diagnostics
mcmc_combo(samples, combo = c("hist", "trace"))
mcmc_pairs(samples)
cor(samples)

# 90% credible intervals for parameters
apply(samples, 2, quantile, c(0.05, 0.95))



# View the regression line
temp_pr <- seq(min(standata$temperature), max(standata$temperature), length.out=200)
predictedProbs <- sapply(temp_pr, function(temp) {
	plogis(samples[, 'intercept'] + temp * samples[,'slope'])})

# view the regression line with posterior samples
plot(died/n ~ annual_mean_temp, data=dat, pch=16, cex=1.2, 
	col="#34A5DA", xlab = "Annual Mean Temperature", ylab = "Proportion Dead")
for(i in seq(1, nrow(predictedProbs), length.out=500)) {
	lines(temp_pr, predictedProbs[i,], col="#c9684018", lwd=0.15)
}
lines(temp_pr, apply(predictedProbs, 2, median), col="#c96840", lwd=2)
posterior_quantiles <- apply(predictedProbs, 2, quantile, c(0.05, 0.95))

# look at the posterior predictive distribution via simulation
# for the number of trials, we use the median number of trees present
post_sims <- matrix(rbinom(length(predictedProbs), max(standata$ntrees), 
	predictedProbs), nrow = nrow(predictedProbs)) / max(standata$ntrees)

ppd_quantiles <- apply(post_sims, 2, quantile, c(0.05, 0.95))
plot(died/n ~ annual_mean_temp, data=dat, pch=16, cex=1.2, 
	col="#34A5DA", xlab = "Annual Mean Temperature", ylab = "Proportion Dead")
lines(temp_pr, apply(predictedProbs, 2, median), col="#c96840", lwd=2)
lines(temp_pr, posterior_quantiles[1, ], col="#c96840", lwd=2, lty=2)
lines(temp_pr, posterior_quantiles[2, ], col="#c96840", lwd=2, lty=2)
lines(temp_pr, ppd_quantiles[1, ], col="#c96840", lwd=2, lty=3)
lines(temp_pr, ppd_quantiles[2, ], col="#c96840", lwd=2, lty=3)






## other plots to try when you have multiple chains
mcmc_hist_by_chain(samples)
mcmc_dens_overlay(samples)
mcmc_violin(samples)

mcmc_intervals(samples)
mcmc_areas(samples)

mcmc_scatter(samples)
