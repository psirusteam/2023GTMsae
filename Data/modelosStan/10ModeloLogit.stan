data {
  int<lower=0> n;   // Número de observaciones
  int<lower=0> K;   // Número de predictores
  matrix[n, K] x;   // Matrix de predictores
  int<lower=0,upper=1> y[n];       // Vector respuesta
}
parameters {
  vector[K] beta;       // coefficients for predictors
}
transformed parameters {
   vector[n] eta;
   eta = x * beta;
   }

model {
  to_vector(beta) ~ normal(0, 10000);
  y ~ bernoulli_logit(eta);  // likelihood
  }
generated quantities {
    real ypred[n];                    // vector de longitud n
    ypred = bernoulli_logit_rng(eta);
}

