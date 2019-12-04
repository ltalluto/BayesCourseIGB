data {
	int<lower=1> n; // number of data points
	int<lower=1> k; // number of groups
	int<lower=0> ntrees[n];
	int<lower=0> died[n];
	int<lower=1, upper=k> group_id [n]; // array of group IDs
	vector [n] temperature;
}
parameters {
	vector [k] intercepts; // one intercept per year
	vector [k] slopes; // one slope per year
	real int_mu;
	real slope_mu;
	real<lower=0> int_sd;
	real<lower=0> slope_sd;
}
transformed parameters {
	vector [n] prob;
	for(i in 1:n) {
		prob[i] = inv_logit(intercepts[group_id[i]] + slopes[group_id[i]] * temperature[i]);
	}
	
}
model {
	died ~ binomial(ntrees, prob);
	intercepts ~ normal(int_mu, int_sd);
	slopes ~ normal(slope_mu, slope_sd);

	// hyperpriors
	// these are quite vague, as it is hard to be sure what makes sense
	int_mu ~ normal(0, 20);
	slope_mu ~ normal(0, 20);
	int_sd ~ normal(0, 20);
	slope_sd ~ normal(0, 20);
}
