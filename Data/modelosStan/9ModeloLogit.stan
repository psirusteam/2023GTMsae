data {
  int<lower=0> n;   // Número de observaciones
  int<lower=0> K;   // Número de predictores
  matrix[n, K] x;   // Matrix de predictores
  int<lower=0,upper=1> y[n];      // Vector respuesta
}
parameters {
  vector[K] beta;       // coefficients for predictors
}
transformed parameters {
    vector[n] theta;
   theta = inv_logit(x * beta);
}

model {
  to_vector(beta) ~ normal(0, 10000);
  y ~ bernoulli(theta);  // likelihood
}
generated quantities {
    real ypred[n];                    // vector de longitud n
    ypred = bernoulli_rng(theta);
}

