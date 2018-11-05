mh.acceptance <- function(X, Y, target) {
	# X: proposed candidate value
	# Y: previous value
	# target: function returning target density at parameter x
	r <- exp(target(X) - target(Y))
	if(is.na(target(X)))
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
	text(0.2*ch.len, max(ch.vals), paste("sig = ", round(X$sig,2), sep=""))
		
	hist(X$chain, main="Histogram of Chain States")
}

mh.propose <- function(samp, previous, sig, target) {
	# propose and select candidate values for the metropolis-hastings algorithm
	# samp: the function used to draw samples (as a function of previous and sig)
	# previous: the previous state in the chain
	# sig: the tuning parameter
	# target: function(x) returning the target density at X
	
	candidate.value <- samp(previous, sig)
	r.cand <- mh.acceptance(candidate.value, previous, target)
	U <- runif(1)
	# cat(paste("proposal: ", round(candidate.value,2), "   previous: ", round(previous,2), "   r: ", round(r.cand, 2), "   U: ", round(U,2)))
	if(U < r.cand) {
		result <- candidate.value
		# cat("   accept\n")
	} else {
		result <- previous
		# cat("   reject\n")
	}
	return(result)
}

metropolis <- function(target, N = 5000, starting.val = 0, sig = NA, tune.len = 1000) {
	# target: function with a single parameter X returning the density of the target distribution at X
	# N: number of MCMC iterations
	# starting.val: starting value of the chain
	# sig: tuning parameter; if NA, the algorithm will iterate to find a tuning value
	# tune.len: how many iterations during tuning


	# define candidate distribution for sampling -- we use the normal distribution
	candidate.sample <- function(y, sig) rnorm(1, y, sig)
	candidate.density <- function(x, y, sig) dnorm(x, y, sig)

	# automatic tuning
	if(is.na(sig)) {
		sig <- 1
		accept <- 0
		chain <- numeric(tune.len)
		chain[1] <- starting.val
		
		for(t in 2:tune.len) {
			chain[t] <- mh.propose(candidate.sample, chain[t-1], sig, target)
			if(chain[t] == chain[t-1]) {
				sig <- sig / 1.1
			} else {
				sig <- sig * 1.1
				accept <- accept + 1
			}	
		}
	cat("Tuning acceptance probability = ", accept/tune.len, "\n")
	}

	# set up the markov chain
	chain <- numeric(N + 1)
	chain[1] <- starting.val
	accept <- 0
	for(t in 2:(N+1)) {
		# propose and evaluate candidates
		chain[t] <- mh.propose(candidate.sample, chain[t-1], sig, target)
		if(chain[t] != chain[t-1]) {
			accept <- accept + 1
		}	
	}
	
	# process the results
	result <- list(chain=chain, sig=sig, accept = accept / N)
	class(result) <- 'mh'
	return(result)
}


library(data.table)
trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

log_posterior <- function(theta) {
	prob <- sum(dbinom(dat$died, dat$n, theta, log=TRUE)) + dbeta(theta, 2,2, log=TRUE)
	return(prob)
}

X <- metropolis(log_posterior, starting.val=0.5, N= 10000)
plot(X)


