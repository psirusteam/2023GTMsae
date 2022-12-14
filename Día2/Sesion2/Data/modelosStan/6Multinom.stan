data {
  int<lower=0> k;  // Número de cátegoria 
  int y[k];        // Número de exitos 
  vector[k] alpha; // Parámetro de las distribción previa 
}
parameters {
  simplex[k] theta;
}
transformed parameters {
  real delta;                              // Tasa de desocupación
  delta = theta[2]/ (theta[2] + theta[1]); // (Desocupado)/(Desocupado + Ocupado)
}
model {
  y ~ multinomial(theta);
  theta ~ dirichlet(alpha);
}
generated quantities {
  int ypred[k];
  ypred = multinomial_rng(theta, sum(y));
}


