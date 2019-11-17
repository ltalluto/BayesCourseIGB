#' Compute the metropolis acceptance rate
#' @param X candidate value
#' @param Y current value
#' @param target Target distribution density function
#' @param data Data for the target function
mh_acceptance <- function(X, Y, target, data) {

	## why the exponential? why not use the ratio t(X)/t(Y)
	r <- exp(target(X, data) - target(Y, data))
	if(is.na(target(X, data)))
		r <- 0
	return(r)
}

plot.mh <- function(X) {
	# function for plotting results of metropolis-hastings
	# produces a trace plot and a histogram
	# X: an object of class mh (returned from function metropolis)
	# display.type: if 'burnin' burn in values are included on the trace plot

	par(mfrow=c(1,2))
	ch.len <- length(X$chain)
	ch.vals <- X$chain
	plot(0:(ch.len-1), ch.vals, xlab="time", ylab="chain state", type='l')
	text(0.2*ch.len, max(ch.vals), paste("scale = ", round(X$scale,2), sep=""))
		
	hist(X$chain, main="Histogram of Chain States")
}


#' Propose and select candidate values for the metropolis-hastings algorithm
#' @param current Current state of the chain
#' @param scale Scale of the proposal distribution
#' @param target Target distribution density function
#' @param data Data for the target distribution
#' @return New value in the chain
mh_propose <- function(current, scale, target, data) {
	# rnorm is the proposal/candidate distribution
	candidate <- rnorm(1, current, scale)
	r_cand <- mh_acceptance(candidate, current, target, data)
	U <- runif(1)
	# cat(paste("proposal: ", round(candidate.value,2), "   previous: ", round(previous,2), "   r: ", round(r.cand, 2), "   U: ", round(U,2)))
	if(U < r_cand) {
		result <- candidate
		# cat("   accept\n")
	} else {
		result <- current
		# cat("   reject\n")
	}
	return(result)
}


#' Single parameter metropolis-hastings implementation
#' @param target Target function (returning log unnormalised posterior density);
#' 	this function should take the parameter as it's first argument and a data list as its second
#' @param initial Initial value of the parameter
#' @param data Data to pass to the target
#' @param iter Number of iterations
#' @param scale Scale for the proposal distribution, if NA, the algorithm will adaptively
#' 		choose a scale
#' @param adapt_iter Number of iterations for adaptation, if needed
#' 
#' @return An object of class 'mh', with three members: 'chain' is the markov chain, 'scale' 
#' 		is the scale parameter used, and 'accept' is the acceptance rate
metropolis <- function(target, initial, data, iter = 5000, scale = 1, adapt_iter = 1000) {


	# automatic tuning
	if(is.na(scale)) {
		scale <- 1
		accept <- 0
		chain <- numeric(adapt_iter)
		chain[1] <- initial
		
		for(t in 2:adapt_iter) {
			chain[t] <- mh_propose(chain[t-1], scale, target, data)
			if(chain[t] == chain[t-1]) {
				scale <- scale / 1.1
			} else {
				scale <- scale * 1.1
				accept <- accept + 1
			}	
		}
	cat("Finished adaptation; acceptance rate = ", accept/adapt_iter, "\n")
	}

	# set up the markov chain
	chain <- numeric(iter + 1)
	chain[1] <- initial
	accept <- 0
	for(t in 2:(iter+1)) {
		# propose and evaluate candidates
		chain[t] <- mh_propose(chain[t-1], scale, target, data)
		if(chain[t] != chain[t-1]) {
			accept <- accept + 1
		}	
	}
	
	# process the results
	result <- list(chain=chain, scale=scale, accept = accept / iter)
	class(result) <- 'mh'
	return(result)
}



#' log posterior distribution
#' this function always has more or less the same signature
#' 
#' @param params The parameter vector for the model
#' @param data A list of data needed to evaluate the model
#' @return The log unnormalized posterior probability of the parameters conditional on the data
log_posterior <- function(params, data) {
	# here we 'unpack' the objects to make the names clearer
	N <- params[1]
	s <- data$s
	nmin <- data$nmin
	nmax <- data$nmax

	log_liklihood <- sum(dunif(s, 1, N, log=TRUE))
	log_prior <- dunif(N, nmin, nmax, log = TRUE)
	return(log_liklihood + log_prior)
}



# original code for generating the data
## s <- sample(1:245, 10)

# the data we use in class
s <- c(147, 126, 183, 88, 9, 203, 16, 10, 112, 205)
N_est <- 1400
data <- list(s = s, nmin = max(s), nmax = 2 * N_est)

data_max <- data
data_max$s <- max(s)
result_max <- metropolis(log_posterior, initial = max(s), data = data_max)
plot(result_max)

## stats
## see bottom for the mode
mean(result_max$chain)
median(result_max$chain)
sd(result_max$chain)
quantile(result_max$chain, c(0.05, 0.95))


result <- metropolis(log_posterior, initial = max(s), data = data)
plot(result)

mean(result$chain)
median(result$chain)
sd(result$chain)
quantile(result$chain, c(0.05, 0.95))

result_adapt <- metropolis(log_posterior, initial = max(s), data = data, scale = NA)
plot(result_adapt)
mean(result_adapt$chain)
median(result_adapt$chain)
sd(result_adapt$chain)
quantile(result_adapt$chain, c(0.05, 0.95))

library(bayesplot)
compare <- cbind(result$chain, result_max$chain, result_adapt$chain)
colnames(compare) <- c("single", "all", "adapt")
mcmc_trace(compare)
mcmc_hist(compare)

# this problem is very simple
# we can actually compute the posterior prob for every possible N and just plot them
N_proposals <- 1:(data$nmax+1)
liklihood <- sapply(N_proposals, function(x) exp(sum(dunif(s, 1, x, log=TRUE))))
prior <- sapply(N_proposals, function(x) exp(dunif(x, data$nmin, data$nmax)))
posterior <- exp(sapply(N_proposals, log_posterior, data = data))

## remember that the posterior is unnormalized
## in this case, we can normalize it because we have captured every possible value
posterior <- posterior/sum(posterior)

par(mfrow=c(1,3))
plot(N_proposals, liklihood, type='l', col='blue', lwd=2, xlab="N", ylab="Likelihood")
plot(N_proposals, prior, type='l', col='red', lwd=2, xlab="N", ylab="Prior Probability")
plot(N_proposals, posterior, type='l', col='purple', lwd=2, xlab="N", ylab="Posterior Probability")




# what if we just want the estimate of the mode, not confidence limits?
# this is the maximum a posteriori estimate, and you already know how to do it
map_estimate <- optim(max(s), log_posterior, method = "Brent", data = data, lower = data$nmin,
	upper = data$nmax, control=list(fnscale = -1))

map_estimate_max <- optim(max(s), log_posterior, method = "Brent", data = data_max, 
	lower = data$nmin, upper = data$nmax, control=list(fnscale = -1))

