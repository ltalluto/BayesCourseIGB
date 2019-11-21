data {
	int<lower=0> n; // number of data points
	vector[n] x;
	vector[n] y;
}
parameters {
	real<lower=0> sigma;
	real alpha;
	real bet;
}
transformed parameters {
	vector[n] y_hat;
	y_hat = alpha + bet * x;
}
model {
	y ~ normal(y_hat, sigma);
}
