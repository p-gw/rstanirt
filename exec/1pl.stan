data {
  int<lower=1> J;              // number of persons 
  int<lower=1> K;              // number of items
  int<lower=1> N;              // number of observations (J*K)
  int<lower=1, upper=J> j[N];  // index for persons  
  int<lower=1, upper=K> k[N];  // index for items
  int<lower=0, upper=1> y[N];  // item response   
}
parameters {
  // person parameters
  vector[J] theta;    

  // item parameters
  vector[K] beta;   
  real mu_beta;
  real<lower=0> sigma_beta;
  
}
model {
  // person priors (model identification)
  theta ~ normal(0, 1); 

  // item priors
  beta ~ normal(mu_beta, sigma_beta);
  mu_beta ~ normal(0, 1);
  sigma_beta ~ cauchy(0, 1);

  y ~ bernoulli_logit(theta[j] - beta[k]); 
}
