library(data.table)
library(mvtnorm)
library(bayesplot)

### note this script also requires results (samples_nb) from 3_iv_poisson.r

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & n > 0]


# hierarchical models usually need some variable to indicate group identity (here, the year)
# it is convenient to number these from 1 to the number of groups
# that way, if group number 1 is 1989 (for example), then intercepts[1] is the intercept
# for 1989
years <- sort(unique(dat$year))
dat$group_id <- match(dat$year, years)


log_posterior <- function(params, data) {
	ntrees <- data$n  # number of trees in each plot
	died <- data$died # number of trees that died
	temperature <- data$annual_mean_temp
	group_id <- data$group_id

	# note that I have named the vector to make life easier
	intercepts <- params[grepl('intercept', names(params))]
	slopes <- params[grepl('slope', names(params))]
	int_mu <- params[grepl('int_mu', names(params))]
	slope_mu <- params[grepl('slope_mu', names(params))]

	# sd parameters are on log scale to make sure they are normally distributed
	log_int_sd <- params[grepl('log_int_sd', names(params))]
	log_slope_sd <- params[grepl('log_slope_sd', names(params))]


	# regression equation
	# the only trick here is we use the slope and intercept corresponding to the correct year
	# for each data point
	prob <- plogis(intercepts[group_id] + temperature * slopes[group_id])

	# note that the distinction of what is a 'prior' gets a bit fuzzy with hierarchical
	# models, so we avoid this term in favor of just using log probability
	logprob <- sum(dbinom(died, ntrees, prob, log=TRUE))

	# random effects for intercept and slope
	logprob <- logprob + sum(dnorm(intercepts, int_mu, exp(log_int_sd), log=TRUE))
	logprob <- logprob + sum(dnorm(slopes, slope_mu, exp(log_slope_sd), log=TRUE))

	# hyperpriors
	# these are quite vague, as it is hard to be sure what makes sense
	logprob <- logprob + dnorm(int_mu, 0, 20, log=TRUE))
	logprob <- logprob + dnorm(slope_mu, 0, 20, log=TRUE))
	logprob <- logprob + dnorm(log_int_sd, 0, 20, log=TRUE))
	logprob <- logprob + dnorm(log_slope_sd, 0, 20, log=TRUE))

	return(logprob)
}

## with big models, it is convenient to name the parameters
# intercept and slope for each group, plus population means and sds for both intercept and slope
nparams <- 2 * max(dat$group_id) + 4
inits <- rnorm(nparams, 0, 3)
names(inits) <- c(paste0("intercept", years), paste0("slope", years), "int_mu", "slope_mu",
	"log_int_sd", "log_slope_sd")

# try laplace approximation, but it will be difficult with this many parameters
fit <- optim(inits, log_posterior, control = list(fnscale = -1), hessian = TRUE, data = dat)
vcv_mat <- solve(-fit$hessian)
## the error here indicates that no, this won't work, so we try stan


standata <- list(n = nrow(dat), k = length(years), ntrees = dat$n, died = dat$died,
	group_id = dat$group_id, temperature = dat$annual_mean_temp)
model <- stan("code/5_ii_hierarchical.stan", data = standata, iter=10000, chains=4)

summary(model, 
	pars = c("intercepts", "slopes", "int_mu", "slope_mu", "int_sd", "slope_sd"))
samples <- as.array(model, 
	pars = c("intercepts", "slopes", "int_mu", "slope_mu", "int_sd", "slope_sd"))
mcmc_intervals(samples)



# View the regression lines
samples <- as.matrix(model, 
	pars = c("intercepts", "slopes", "int_mu", "slope_mu", "int_sd", "slope_sd"))
temp_pr <- seq(min(standata$temperature), max(standata$temperature), length.out=200)
cols <- colorRampPalette(c("#c7e9b4", "#0c2c84"))(length(years))
licols <- colorRampPalette(c("#fc8d59", "#990000"))(length(years))
plot(0, 0, type='n', xlab = "Annual Mean Temperature", ylab = "Proportion Dead", 
	xlim = range(dat$annual_mean_temp), ylim=c(0,1))
for(yr in years) {
	i <- match(yr, years)
	probs <- sapply(temp_pr, function(temp) {
		plogis(samples[, paste0('intercepts[', i, ']')] + 
		temp * samples[,paste0('slopes[', i, ']')])})
	dt <- dat[year == yr]
	col <- cols[i]
	points(died/n ~ annual_mean_temp, data=dt, pch=16, cex=1.2, col=col)
	lines(temp_pr, colMeans(probs), col=licols[i], lwd=1)
}

probs <- sapply(temp_pr, function(temp) {
	plogis(samples[, "int_mu"] + temp * samples[,"slope_mu"])})
licol <- "#bf812d"
interval <- apply(probs, 2, quantile, c(0.05, 0.95))
lines(temp_pr, colMeans(probs), col=licol, lwd=4)
polygon(c(temp_pr, rev(temp_pr)), c(interval[1,], rev(interval[2,])), 
	border=NA, col=paste0(linecolor[2], "55"))


## show two extremes
plot(0, 0, type='n', xlab = "Annual Mean Temperature", ylab = "Proportion Dead", 
	xlim = range(dat$annual_mean_temp), ylim=c(0,1))
yr <- 2001
i <- match(yr, years)
probs <- sapply(temp_pr, function(temp) {
	plogis(samples[, paste0('intercepts[', i, ']')] + 
	temp * samples[,paste0('slopes[', i, ']')])})
dt <- dat[year == yr]
col <- cols[i]
linecolor <- "#1f78b4"
interval <- apply(probs, 2, quantile, c(0.05, 0.95))
points(died/n ~ annual_mean_temp, data=dt, pch=16, cex=1.2, col=col)
lines(temp_pr, colMeans(probs), col=linecolor, lwd=1)
polygon(c(temp_pr, rev(temp_pr)), c(interval[1,], rev(interval[2,])), 
	border=NA, col=paste0(linecolor, "55"))

leg1 <- paste0(yr, ": n = ", nrow(dt))
legend("topright", legend = leg1, lwd=1, col=linecolor, bg='white')


yr <- 2004
i <- match(yr, years)
probs <- sapply(temp_pr, function(temp) {
	plogis(samples[, paste0('intercepts[', i, ']')] + 
	temp * samples[,paste0('slopes[', i, ']')])})
dt <- dat[year == yr]
col <- cols[i]
linecolor <- c(linecolor, "#ff7f00")
interval <- apply(probs, 2, quantile, c(0.05, 0.95))
points(died/n ~ annual_mean_temp, data=dt, pch=16, cex=1.2, col=col)
lines(temp_pr, colMeans(probs), col=linecolor[2], lwd=1)
polygon(c(temp_pr, rev(temp_pr)), c(interval[1,], rev(interval[2,])), 
	border=NA, col=paste0(linecolor[2], "55"))

leg2 <- paste0(yr, ": n = ", nrow(dt))
legend("topright", legend = c(leg1, leg2), lwd=1, col=linecolor, bg='white')





yr <- 2011
i <- match(yr, years)
probs <- sapply(temp_pr, function(temp) {
	plogis(samples[, paste0('intercepts[', i, ']')] + 
	temp * samples[,paste0('slopes[', i, ']')])})
dt <- dat[year == yr]
col <- cols[i]
linecolor <- c(linecolor, "#6a3d9a")
interval <- apply(probs, 2, quantile, c(0.05, 0.95))
points(died/n ~ annual_mean_temp, data=dt, pch=16, cex=1.2, col=col)
lines(temp_pr, colMeans(probs), col=linecolor[3], lwd=1)
polygon(c(temp_pr, rev(temp_pr)), c(interval[1,], rev(interval[2,])), 
	border=NA, col=paste0(linecolor[3], "55"))
leg3 <- paste0(yr, ": n = ", nrow(dt))


legend("topright", legend = c(leg1, leg2, leg3), lwd=1, col=linecolor, bg='white')