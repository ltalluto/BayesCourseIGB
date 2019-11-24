library(data.table)
library(mvtnorm)
library(bayesplot)
library(rstan)
library(ggplot2)

### note this script also requires results (samples_nb) from 3_iv_poisson.r

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]


# this posterior is identical to the model block from the stan code
log_posterior <- function(params, data) {
	ntrees <- data$n  # number of trees in each plot
	died <- data$died # number of trees that died
	temperature <- data$annual_mean_temp

	intercept <- params[1]
	slope <- params[2]

	# using a logit link function, whose inverse is plogis
	mu <- plogis(intercept + slope * temperature)

	ll <- sum(dbinom(died, ntrees, mu, log=TRUE))
	lp <- dnorm(intercept, 0, 10, log = TRUE) + dnorm(slope, 0, 5, log=TRUE)

	return(ll + lp)
}


# initial parameter values for intercept and slope; just drawing from the prior
# to come up with something random but plausible
inits <- c(rnorm(1, 0, 10), rnorm(1, 0, 5))

# hessian = TRUE tells optim to give you the hessian matrix
# system.time allows us to find out how long the whole procedure takes for comparison
laplace_time <- system.time({
	fit <- optim(inits, log_posterior, control = list(fnscale = -1), hessian = TRUE, data = dat)
	vcv_mat <- solve(-fit$hessian)
	samples_laplace <- rmvnorm(5000, fit$par, vcv_mat)
})


## here is the stan fit, so we can compare how long to get the same 5000 samples
stan_time <- system.time({
	standata <- list(n = nrow(dat), ntrees = dat$n, died = dat$died,
		temperature = dat$annual_mean_temp)
	model <- stan("code/4_ii_stan_glm.stan", data = standata, 
		iter=10000, chains=1)
	samples_stan <- as.matrix(model, par=c('intercept', 'slope'))
})

## compare parameters & standard errors
cbind(stan=colMeans(samples_stan), laplace=fit$par)
cbind(stan=apply(samples_stan, 2, sd), laplace=sqrt(diag(vcv_mat)))

# compare correlations
cbind(stan = cor(samples_stan[,1], samples_stan[,2]), laplace = cov2cor(vcv_mat)[2])


# these are not really mcmc samples, but can still use some bayesplot diagnostics
# just note that mcmc_trace() is not useful here, because these are independent samples
colnames(samples_laplace) <- c('intercept', 'slope')
mcmc_pairs(samples_laplace)
mcmc_pairs(samples_stan)


# view the regression line with posterior samples
temp_pr <- seq(min(standata$temperature), max(standata$temperature), length.out=200)
predictedProbs <- sapply(temp_pr, function(temp) {
	plogis(samples_stan[, 'intercept'] + temp * samples_stan[,'slope'])})
predictedProbs_la <- sapply(temp_pr, function(temp) {
	plogis(samples_laplace[, 'intercept'] + temp * samples_laplace[,'slope'])})


par(mfrow=c(1,2))
plot(died/n ~ annual_mean_temp, data=dat, pch=16, cex=1.2, 
	col="#34A5DA", xlab = "Annual Mean Temperature", ylab = "Proportion Dead", main="Stan")
for(i in seq(1, nrow(predictedProbs), length.out=500)) {
	lines(temp_pr, predictedProbs[i,], col="#c9684018", lwd=0.15)
}
lines(temp_pr, apply(predictedProbs, 2, median), col="#c96840", lwd=2)

plot(died/n ~ annual_mean_temp, data=dat, pch=16, cex=1.2, 
	col="#34A5DA", xlab = "Annual Mean Temperature", ylab = "Proportion Dead", main="Laplace")
for(i in seq(1, nrow(predictedProbs_la), length.out=500)) {
	lines(temp_pr, predictedProbs_la[i,], col="#984ea318", lwd=0.15)
}
lines(temp_pr, apply(predictedProbs_la, 2, median), col="#984ea3", lwd=2)
