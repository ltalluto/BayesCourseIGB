library(data.table)
trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

lik_func <- function(theta, n, k) sum(dbinom(n, k, theta, log=TRUE))
cand <- function(old_theta, sigma) rnorm(1, old_theta, sigma)
log_prior <- function(theta) dbeta(theta, 1.2, 1.2, log=TRUE)
log_posterior <- function(theta, n, k) lik_func(theta, n, k) + log_prior(theta)
accept <- function(proposal, original) 
		exp(log_posterior(proposal, n = dat$died, k = dat$n) - 
			log_posterior(original, n = dat$died, k = dat$n))

sigma <- 0.05
N <- 10000
chain <- numeric(N)
chain[1] <- 0.5

num_acceptances <- 0
for(t in 2:N)
{
	proposal <- cand(chain[t-1], sigma)
	r <- accept(proposal, chain[t-1])
	U <- runif(1,0,1)
	if(is.na(r)) r <- 0
	if(U < r) {
		chain[t] <- proposal
		num_acceptances <- num_acceptances + 1
	} else
		chain[t] <- chain[t-1]
}