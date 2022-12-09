data {
  int<lower=0> n;   // Número de observaciones
  vector[n] x;      // Variable predictora
  vector[n] y;      // Variable respuesta
}
parameters {
  real b0;            // Intercepto
  real b1;            // Pendiente
  real<lower=0> sigma2;  
}
transformed parameters{
  real<lower=0> sigma;
  sigma = sqrt(sigma2);
}

model {
  b0 ~ normal(0, 1000);
  b1 ~ normal(0, 1000);
  sigma2 ~ inv_gamma(0.0001, 0.0001);
  y ~ normal(b0 + b1*x, sigma);  // likelihood
}
generated quantities {
    real ypred[n];                    // Vector de longitud n
    ypred = normal_rng(b0 + b1*x, sigma);

}
