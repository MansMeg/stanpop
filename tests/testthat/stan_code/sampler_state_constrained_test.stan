parameters {
  real<lower=0> sigma;
  vector[2] y;
}

model {
  sigma ~ lognormal(0, 0.25);
  y ~ normal(0, sigma);
}
