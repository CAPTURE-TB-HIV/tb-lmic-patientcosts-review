data {
  int<lower=1> N;        // number of data points        
  real<lower=0>cost[N];  // response variable - cost reported by the study
  real dme_prediag[N];   // indicator of whether cost includes direct medical
                         // pre-diagnosis costs
  real dnme_prediag[N];  // indicator of whether cost includes direct non-
                         // medical pre-diagnosis costs
  real ie_prediag[N];    // indicator of whether cost includes indirect
                         // pre-diagnosis costs
  real dme_intsvtx[N];   // indicator of whether cost includes direct medical
                         // intensive TB treatment costs
  real dnme_intsvtx[N];  // indicator of whether cost includes direct non-
                         // medical intensive TB treatment costs
  real ie_intsvtx[N];    // indicator of whether cost includes indirect
                         // intensive treatment costs
  real dme_contntx[N];   // indicator of whether cost includes direct medical
                         // continuation TB treatment costs
  real dnme_contntx[N];  // indicator of whether cost includes direct non-
                         // medical continuation TB treatment costs
  real ie_contntx[N];    // indicator of whether cost includes indirect
                         // continuation treatment costs
  ///// COVARIATES ////////////////////////////////////////////////////////////
  int drtb[N];           // whether a study was exclusively drtb
  vector<lower=0>[N] se; // study reported dispersion estimate 
}

transformed data {
    vector<lower=0>[N] se2 = square(se); 
}

parameters {
  // coefficients for the different cost components 
  real<lower=0> coef_dme_prediag;
  real<lower=0> coef_dnme_prediag;
  real<lower=0> coef_ie_prediag;
  real<lower=0> coef_dme_intsvtx;
  real<lower=0> coef_dnme_intsvtx;
  real<lower=0> coef_ie_intsvtx;
  real<lower=0> coef_dme_contntx;
  real<lower=0> coef_dnme_contntx;
  real<lower=0> coef_ie_contntx;
  // additional variance parameter
  real<lower=0> add_var;
  // covariates
  // real<lower=0> coef_drtb;
}

// Calculate mu for each n
transformed parameters {
    vector<lower=0>[N] mu;
    vector[N] beta_cost; 
    vector<lower=0>[N] inverse_phi_cost;

    for (n in 1:N){
        // systematic component of the estimate 
        mu[n] = coef_dme_prediag * dme_prediag[n] +
                coef_dnme_prediag * dnme_prediag[n] +
                coef_ie_prediag * ie_prediag[n] + 
                coef_dme_intsvtx * dme_intsvtx[n] +
                coef_dnme_intsvtx * dnme_intsvtx[n] +
                coef_ie_intsvtx * ie_intsvtx[n] + 
                coef_dme_contntx * dme_contntx[n] +
                coef_dnme_contntx * dnme_contntx[n] +
                coef_ie_contntx * ie_contntx[n]; # + 
                ////// COVARIATES /////////
                // coef_drtb * drtb[n];
        // shape parameter
        inverse_phi_cost[n] = se2[n] + add_var; 
        // rate parameter
        beta_cost[n] = inverse_phi_cost[n] / mu[n];
    }
}


// The model to be estimated.
model {
    // Priors for dispersion parameter 
    // add_var ~ exponential(1);
    add_var ~ cauchy(0, 25);
    // add_var ~ gamma(1, 10000);
    //inverse_phi_cost ~ exponential(0.001); // prior on inverse dispersion parameter
    // cost estimate
    cost ~ gamma(inverse_phi_cost, beta_cost);
}

generated quantities {
    vector[N] y_rep;
    for(n in 1:N){
        //posterior draws to get posterior predictive checks
        y_rep[n] <- gamma_rng(inverse_phi_cost[n], beta_cost[n]); 
    }
}
