data {
	int n; // number of data points
	int <lower=0> ntrees [n]; // arrays of integers with n observations 
	int <lower=0> died [n];
	vector [n] temperature;
}
parameters {
	real intercept;
	real slope;
}
transformed parameters {
	vector [n] prob;
	prob = inv_logit(intercept + slope * temperature);
}
model {
	died ~ binomial(ntrees, prob);
	intercept ~ normal(0, 10);
	slope ~ normal(0, 5);
}
