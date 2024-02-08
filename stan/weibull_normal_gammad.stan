//stan code to compute the parameters of a weibull distribution given normal prior 
//on the shape parameter and gamma priors on the scale parameters
//  beta: weibull shape parameter
//  eta:  weibull scale parameters (nVariants values)

data {
  int<lower=0>  nData;
  int<lower=0>  nVariants;
  real<lower=0> reliability[nData];
  int<lower=0>  is_cen[nData];
  int<lower=0>  variant[nData];
  real beta_mean;
  real beta_std;
  real eta_shape[nVariants];
  real eta_rate[nVariants];
}
parameters {
  real<lower=0> beta;
  real<lower=0> eta[nVariants];
}
model {
  // priors
  beta ~ normal(beta_mean,beta_std);
  for(j in 1:nVariants) {
    eta[j] ~ gamma(eta_shape[j],eta_rate[j]);
    //target += gamma_lupdf(eta[j] | eta_shape, eta_rate); //**syntax error**//
  }

  // likelihood
  for(i in 1:nData) {
    // weibull scale parameter (stan uses R's parametrization)
    real scale  = eta[variant[i]];
    // if uncensored, compute (unnormalized) pdf at reliability[i] directly
    if (is_cen[i] == 0)
      reliability[i] ~ weibull( beta, scale );
    //  target += weibull_lupdf( reliability[i] | beta, scale ); //**syntax error**//
    // if censored, compute probability that value is >= reliability[i]
    if (is_cen[i] == 1)
      target += weibull_lccdf( reliability[i] | beta, scale );
  }
}
