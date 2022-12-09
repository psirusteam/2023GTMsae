data {
  int<lower=0> n;   // Número de observaciones
  int<lower=0> K;   // Número de predictores
  matrix[n, K] x;   // Matrix de predictores
  vector[n] y;      // Vector respuesta
}
parameters {
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma2; 
}
transformed parameters{
  real<lower=0> sigma;
  sigma = sqrt(sigma2);
}

model {
  to_vector(beta) ~ normal(0, 10000);
  sigma2 ~ inv_gamma(0.0001, 0.0001);
  y ~ normal(x * beta, sigma);  // likelihood
}
generated quantities {
    real ypred[n];                    // vector de longitud n
    ypred = normal_rng(x * beta, sigma);
}

