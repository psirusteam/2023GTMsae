data {
  int<lower=0> K;                 // Número de provincia  
  int<lower=0> n[K];              // Número de ensayos 
  int<lower=0> s[K];              // Número de éxitos
  real a;
  real b;
}
parameters {
  real<lower=0, upper=1> theta[K]; // theta_d|S_d
}
model {
  for(kk in 1:K) {
  s[kk] ~ binomial(n[kk], theta[kk]);
  }
  to_vector(theta) ~ beta(a, b);
}

generated quantities {
    real spred[K];                    // Vector de longitud n
    for(kk in 1:K){
    spred[kk] = binomial_rng(n[kk],theta[kk]);
}
}
