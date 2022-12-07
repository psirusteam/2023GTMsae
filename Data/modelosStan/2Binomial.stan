data {
  int<lower=0> n;              // Número de ensayos 
  int<lower=0> s;              // Número de éxitos
  real a;
  real b;
}
parameters {
  real<lower=0, upper=1> theta;    
}
model {

  s ~ binomial(n, theta);

  theta ~ beta(a, b);

}

generated quantities {
    real spred;                    // vector de longitud D
    spred = binomial_rng(n, theta);

}
