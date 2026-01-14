data {
  int<lower=1> T;               // number of hours per day 
  int<lower=1> D;               // number of days in the site data (82–423)
  real deltat;                  // time step (days)
  matrix[T, D] lightMA;         // light at each timestep
  vector[D] sumlightIdeal;      // idealized total daily light (streamMetabolizer)
  vector[D] sumlightReal;       // observed total daily light (satellite)
  matrix[T, D] zMA;             // depth / channel depth (m)
  matrix[T, D] concMA;          // observed nitrate concentration (mmol m^-3)
  real Ne_meanprior;            // prior mean for N_e
  real Ne_sdprior;              // prior sd for N_e
}

parameters {
  // --- observation & process error
  real<lower=0> sigma;

  // --- hierarchical parameters for K (turnover)
  real logK_mean;
  real<lower=0> sigma_logK;
  vector[D] z_logK;

  // --- hierarchical parameters for U (autotrophic uptake)
  real b0;
  real b1;
  real<lower=0> sigma_U;
  vector[D] z_logU;

  // --- equilibrium nitrate concentrations
  vector<lower=0>[D] N_e;
}


transformed parameters {
  // --- derived parameters
  vector[D] logK;
  vector[D] logU;
  vector<lower=0>[D] K;
  vector [D] U;

  // --- non-centered reparameterization
  logK = logK_mean + sigma_logK * z_logK;
  logU = b0 + b1 * log(sumlightReal) + sigma_U * z_logU;

  // --- back-transform to natural scale
  K = exp(logK);
  U = exp(logU);
}


model {
  matrix[T, D] conc_hat;

  // --- hierarchical priors (non-centered implies z_~normal(0,1))
  z_logK ~ normal(0, 1);
  z_logU ~ normal(0, 1);

  // --- priors for hyperparameters
  b0 ~ normal(0, 10);
  b1 ~ normal(0, 1);
  sigma_U ~ normal(0, 0.2);
  logK_mean ~ normal(2, 1.6);     // LINX2-informed
  sigma_logK ~ normal(0, 0.5);
  N_e ~ normal(Ne_meanprior, Ne_sdprior);
  sigma ~ normal(0, 1);

  // --- likelihood
  for (d in 1:D) {
    conc_hat[1, d] = concMA[1, d];

    for (i in 2:T) {
      conc_hat[i, d] = concMA[i - 1, d] - (U[d] * lightMA[i, d]) / (zMA[i, d] * sumlightIdeal[d])
        + K[d] * (N_e[d] - concMA[i - 1, d]) * deltat;

      concMA[i, d] ~ normal(conc_hat[i, d], sigma);
    }
  }
}

generated quantities {
  matrix[T, D] conc_pred;
  matrix[T, D] conc_tilde;
  vector[D] U_tot;
  vector[D] Vf_tot;

  for (d in 1:D) {
    conc_pred[1, d] = concMA[1, d];
    conc_tilde[1, d] = normal_rng(concMA[1, d], sigma);

    U_tot[d] = K[d] * N_e[d] * zMA[1, d];   // derived total uptake
    Vf_tot[d] = K[d] * zMA[1, d];           // derived areal uptake velocity

    for (i in 2:T) {
      conc_pred[i, d] = conc_pred[i - 1, d] - (U[d] * lightMA[i, d]) / (zMA[i, d] * sumlightIdeal[d]) 
           + K[d] * (N_e[d] - conc_pred[i - 1, d]) * deltat;

      conc_tilde[i, d] = normal_rng(conc_pred[i, d], sigma);
    }
  }
}
