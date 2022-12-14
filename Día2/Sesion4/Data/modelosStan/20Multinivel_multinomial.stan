functions {
  matrix pred_theta(matrix Xp, matrix Zp, int p, matrix beta, matrix u){
  int D1 = rows(Xp);
  real num1[D1, p];
  real den1[D1];
  matrix[D1,p] theta_p;
  
  for(d in 1:D1){
    num1[d, 1] = 1;
    num1[d, 2] = exp(Xp[d, ] * beta[1, ]' + Zp[d, ] * u[1, ]') ;
    num1[d, 3] = exp(Xp[d, ] * beta[2, ]' + Zp[d, ] * u[2, ]') ;
    
    den1[d] = sum(num1[d, ]);
  }
  
  for(d in 1:D1){
    for(i in 2:p){
    theta_p[d, i] = num1[d, i]/den1[d];
    }
    theta_p[d, 1] = 1/den1[d];
   }

  return theta_p  ;
  }
  
}

data {
  int<lower=1> D;    // número de postestrto 
  int<lower=1> D1;   // número de dominios por predesir 
  int<lower=1> P;    // categorías
  int<lower=1> K;  // cantidad de regresores
  int<lower=1> Kz; // cantidad de regresores en Z
  int y[D, P];       // matriz de datos
  matrix[D, K] X; // matriz de covariables
  matrix[D, Kz] Z; // matriz de covariables
  matrix[D1, K] Xp; // matriz de covariables
  matrix[D1, Kz] Zp; // matriz de covariables
}
  

parameters {
  matrix[P-1, K] beta;// matriz de parámetros 
  vector<lower=0>[P-1] sigma_u;       // random effects standard deviations
  // declare L_u to be the Choleski factor of a 2x2 correlation matrix
  cholesky_factor_corr[P-1] L_u;
  matrix[P-1, Kz] z_u;                  
}

transformed parameters {
  simplex[P] theta[D];// vector de parámetros;
  real num[D, P];
  real den[D];
  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[P-1, Kz] u; // random effect matrix
  u = diag_pre_multiply(sigma_u, L_u) * z_u;
  
  for(d in 1:D){
    num[d, 1] = 1;
    num[d, 2] = exp(X[d, ] * beta[1, ]' + Z[d, ] * u[1, ]') ;
    num[d, 3] = exp(X[d, ] * beta[2, ]' + Z[d, ] * u[2, ]') ;
    
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
  L_u ~ lkj_corr_cholesky(1); // LKJ prior for the correlation matrix
  to_vector(z_u) ~ normal(0, 1);
  sigma_u ~ cauchy(0, 50);
  to_vector(beta) ~ normal(0, 100);
 
  for(d in 1:D){
    target += multinomial_lpmf(y[d, ] | theta[d, ]); 
  }
}

  
generated quantities {
  // predict 
  matrix[D1,P] theta_p;// vector de parámetros;
  matrix[2, 2] Omega;
  vector<lower=0>[2] sdcomprobar;
  sdcomprobar[1] = sd(u[1, ]);
  sdcomprobar[2] = sd(u[2, ]);

  Omega = L_u * L_u'; // so that it return the correlation matrix
// predicción 

theta_p = pred_theta(Xp,Zp,P, beta, u) ; 

}
