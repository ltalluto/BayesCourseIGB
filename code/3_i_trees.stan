data {
	int<lower=0> n; // number of data points
	int<lower=0> died[n];
	int<lower=0> ntrees[n];
}
parameters {
	real<lower=0, upper=1> theta;
}
model {
	died ~ binomial(ntrees, theta);
	theta ~ beta(1.2, 1.2);
}
