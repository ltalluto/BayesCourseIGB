library(rstan)
library(data.table)
library(bayesplot)

trees <- readRDS("data/trees.rds")
dat <- trees[grepl("TSU-CAN", species) & year == 2005 & n > 0]

stan_data <- list(n = nrow(dat), died = dat$died, ntrees = dat$n)
model <- stan("code/3_i_trees.stan", data = stan_data, chains=3,
	iter = 2000)

# some functions to play with
mcmc_trace(as.array(model))

mcmc_hist(as.array(model))
mcmc_hist_by_chain(as.array(model))
mcmc_dens_overlay(as.array(model))
mcmc_violin(as.array(model))

mcmc_intervals(as.array(model), pars=c('theta'))
mcmc_areas(as.array(model), pars=c('theta'))

mcmc_scatter(as.array(model))
mcmc_pairs(as.array(model))
mcmc_combo(as.array(model), combo = c("dens", "trace"))
