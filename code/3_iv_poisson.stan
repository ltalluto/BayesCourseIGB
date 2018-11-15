data {
	int n; // number of data points
	int<lower=0> ntrees[n];
	vector [n] temperature;
}
parameters {
	real intercept;
	real slope;
}
transformed parameters {
	vector [n] lambda;
	lambda = exp(intercept + slope * temperature);
}
model {
	ntrees ~ poisson(lambda);
	intercept ~ normal(0, 50);
	slope ~ normal(0,30);
}
