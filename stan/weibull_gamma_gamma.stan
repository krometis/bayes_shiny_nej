//stan code to compute the parameters of a weibull distribution given gamma priors
//  beta: weibull shape parameter
//  eta:  weibull scale parameter (single prior, multiple posterior estimates)
data {
  int<lower=0>  nData;
  int<lower=0>  nVariants;
  real<lower=0> reliability[nData];
  int<lower=0>  is_cen[nData];
  int<lower=0>  variant[nData];
  real<lower=0> beta_shape;
  real<lower=0> beta_rate;
  real<lower=0> eta_shape;
  real<lower=0> eta_rate;
}
parameters {
  real<lower=0> beta;
  real<lower=0> eta[nVariants];
}
model {

  // simpler priors
  beta ~ gamma(beta_shape,beta_rate);
  for(j in 1:nVariants) {
    eta[j] ~ gamma(eta_shape, eta_rate);
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
