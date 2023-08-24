// generated with brms 2.17.0
functions {
  /* hurdle lognormal log-PDF of a single response
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) +
             lognormal_lpdf(y | mu, sigma);
    }
  }
  /* hurdle lognormal log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: linear predictor for the hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_logit_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             lognormal_lpdf(y | mu, sigma);
    }
  }
  // hurdle lognormal log-CCDF and log-CDF functions
  real hurdle_lognormal_lccdf(real y, real mu, real sigma, real hu) {
    return bernoulli_lpmf(0 | hu) + lognormal_lccdf(y | mu, sigma);
  }
  real hurdle_lognormal_lcdf(real y, real mu, real sigma, real hu) {
    return log1m_exp(hurdle_lognormal_lccdf(y | mu, sigma, hu));
  }
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and 1
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> Ksp;  // number of special effects terms
  int<lower=1> Imo;  // number of monotonic variables
  int<lower=1> Jmo[Imo];  // length of simplexes
  int Xmo_1[N];  // monotonic variable
  vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  simplex[Jmo[1]] simo_1;  // monotonic simplex
  vector[Ksp] bsp;  // special effects coefficients
  real<lower=0> sigma;  // dispersion parameter
  real<lower=0,upper=1> hu;  // hurdle probability
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += normal_lpdf(Intercept | 1.6, 0.6);
  lprior += dirichlet_lpdf(simo_1 | con_simo_1);
  lprior += normal_lpdf(bsp | 0, 1);
  lprior += exponential_lpdf(sigma | 0.44);
  lprior += beta_lpdf(hu | 1, 1);
  lprior += exponential_lpdf(sd_1 | 0.44);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[n]) + r_1_1[J_1[n]] * Z_1_1[n];
    }
    for (n in 1:N) {
      target += hurdle_lognormal_lpdf(Y[n] | mu[n], sigma, hu);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // additionally sample draws from priors
  real prior_Intercept = normal_rng(1.6,0.6);
  simplex[Jmo[1]] prior_simo_1 = dirichlet_rng(con_simo_1);
  real prior_bsp = normal_rng(0,1);
  real prior_sigma = exponential_rng(0.44);
  real prior_hu = beta_rng(1,1);
  real prior_sd_1 = exponential_rng(0.44);
  // use rejection sampling for truncated priors
  while (prior_sigma < 0) {
    prior_sigma = exponential_rng(0.44);
  }
  while (prior_hu < 0 || prior_hu > 1) {
    prior_hu = beta_rng(1,1);
  }
  while (prior_sd_1 < 0) {
    prior_sd_1 = exponential_rng(0.44);
  }
}
