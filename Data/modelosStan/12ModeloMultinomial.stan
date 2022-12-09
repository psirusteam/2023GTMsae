data {
  int<lower=1> D;    // Número de dominios 
  int<lower=1> P;    // Categorías
  int<lower=1> K;    // Número de regresores
  int y[D, P];       // Matriz de datos
  matrix[D, K] X;    // Matriz de covariables
}
  

parameters {
  matrix[P-1, K] beta;// Matriz de parámetros 
                 
}

transformed parameters {
  simplex[P] theta[D];// Vector de parámetros;
  real num[D, P];
  real den[D];

for(d in 1:D){
    num[d, 1] = 1;
    num[d, 2] = exp(X[d, ] * beta[1, ]') ;
    num[d, 3] = exp(X[d, ] * beta[2, ]') ;
    
    den[d] = sum(num[d, ]);

  }
  for(d in 1:D){
    for(p in 2:P){
    theta[d, p] = num[d, p]/den[d];
    }
    theta[d, 1] = 1/den[d];
  }
}

model {
  to_vector(beta) ~ normal(0, 100);
  for(d in 1:D){
    target += multinomial_lpmf(y[d, ] | theta[d, ]); 
  }
}

generated quantities {
    int ypred[D,P];               
    for(d in 1:D){
    ypred[d,] = multinomial_rng(theta[d, ], sum(y[d, ])); 
  }
}


