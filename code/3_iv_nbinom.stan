data {
	int n; // number of data points
	int<lower=0> ntrees[n];
	vector [n] temperature;
}
parameters {
	real intercept;
	real slope;
	real<lower=0> phi;
}
transformed parameters {
	vector [n] lambda;
	lambda = exp(intercept + slope * temperature);
}
model {
	ntrees ~ neg_binomial_2(lambda, phi);
	intercept ~ normal(0, 50);
	slope ~ normal(0,30);
	phi ~ exponential(0.1);
}
generated quantities {
	int yhat [n];
	yhat = neg_binomial_2_rng(lambda, phi);
}
