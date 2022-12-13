data {
  int<lower=1> N1;                      // sample size
  int<lower=1> N2;                      // sample size
  int<lower=1> p;                       // p predictors
  vector<lower=0,upper=1>[N1] y;        // response 
  matrix[N1,p] X;
  matrix[N2,p] Xs;
  vector<lower=0>[N1] phi;              // dispersion parameter
}

parameters {
  vector[p] beta;
  real<lower=0> sigma2_v;               // K predictors
  vector[N1] v;
// reg coefficients
}

transformed parameters{
  vector[N1] LP;
  real<lower=0> sigma_v;
  vector[N1] theta;                     // linear predictor
  LP = X * beta + v;
  sigma_v = sqrt(sigma2_v); 
  for (i in 1:N1) { 
    theta[i] = inv_logit(LP[i]); 
  }
}

model {
  // model calculations
  vector[N1] a;                         // parameter for beta distn
  vector[N1] b;                         // parameter for beta distn

  for (i in 1:N1) { 
    a[i] = theta[i] * phi[i];
    b[i] = (1 - theta[i]) * phi[i];
  }

  // priors
  beta ~ normal(0, 100);
  v ~ normal(0, sigma_v);
  sigma2_v ~ inv_gamma(0.0001, 0.0001);

  // likelihood
  y ~ beta(a, b);
}

generated quantities {
  vector[N2] y_pred;
  vector[N2] thetapred;
  for (i in 1:N2) {
    y_pred[i] = normal_rng(Xs[i] * beta, sigma_v);
    thetapred[i] = inv_logit(y_pred[i]);
  }
}
  
