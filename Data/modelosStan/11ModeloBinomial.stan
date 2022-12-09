data {
  int<lower=0> D;   // Número de observaciones
  int<lower=0> K;   // Número de predictores
  int<lower=0> nd[D];  // Número de ensayos
  int<lower=0> yd[D];   // Número de exitos
  matrix[D, K] x;   // Matrix de predictores
}
parameters {
  vector[K] beta;       // coefficients for predictors
}
transformed parameters {
   vector[D] eta;
   eta =  x * beta;
   }

model {
  to_vector(beta) ~ normal(0, 10000);
  yd ~ binomial_logit(nd, eta);  // likelihood

  }
generated quantities {
    real ypred[D];                    // vector de longitud n
  ypred = binomial_rng(nd, inv_logit(eta));  

}

