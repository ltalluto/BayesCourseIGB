data {
	int<lower=0> n; // number of data points
	int<lower=0> nsp; // number of species
	int<lower=0> ntrees[n];
	int<lower=1, upper=nsp> species [n]; // array of species IDs
	vector [n] temperature;
}
parameters {
	vector [nsp] intercept; // one intercept per species
	vector [nsp] slope; // one slope per species
	real int_mean;
	real slope_mean;
	real<lower=0> int_sd;
	real<lower=0> slope_sd;
}
transformed parameters {
	vector [n] lambda;
	for(i in 1:n) {
		lambda[i] = exp(intercept[species[i]] + slope[species[i]] * temperature[i]);
	}
	
}
model {
	ntrees ~ poisson(lambda);
	intercept ~ normal(int_mean, int_sd);
	slope ~ normal(slope_mean, slope_sd);

	int_mean ~ normal(0, 50);
	slope_mean ~ normal(0, 50);
	int_sd ~ exponential(0.1);
	slope_sd ~ exponential(0.1);
}
generated quantities {
	int yhat [n];
	yhat = poisson_rng(lambda);
}
