functions {
  real alt_gamma_lpdf(vector x, real tau, vector mu) {
    int N = num_elements(x);
    return (tau - 1) * sum(log(x)) + tau * sum(log(tau) - log(mu)) - N * lgamma(tau) - sum(x * tau ./ mu);
  }
}

data {
    int<lower=1> N;         // number of data points        
    vector<lower=0>[N]cost; // response variable - cost reported by the study
    real dme_prediag[N];    // indicator of whether cost includes direct medical
                            // pre-diagnosis costs
    real dnme_prediag[N];   // indicator of whether cost includes direct non-
                            // medical pre-diagnosis costs
    real ie_prediag[N];     // indicator of whether cost includes indirect
                            // pre-diagnosis costs
    real dme_intsvtx[N];    // indicator of whether cost includes direct medical
                            // intensive TB treatment costs
    real dnme_intsvtx[N];   // indicator of whether cost includes direct non-
                            // medical intensive TB treatment costs
    real ie_intsvtx[N];     // indicator of whether cost includes indirect
                            // intensive treatment costs
    real dme_contntx[N];    // indicator of whether cost includes direct medical
                            // continuation TB treatment costs
    real dnme_contntx[N];   // indicator of whether cost includes direct non-
                            // medical continuation TB treatment costs
    real ie_contntx[N];     // indicator of whether cost includes indirect
                            // continuation treatment costs
    // int study_id[N];     // Survey identifiers
    int N_country;        // Number of countries
    int country_id[N];      // Country identifier
    vector<lower=0>[N] se;  // study reported dispersion estimate 
    // real<lower=0> tau;
}

transformed data {
    vector<lower=0>[N] se2 = square(se); 
}

parameters {
    // overall effect for the different cost components 
    real beta_dme_prediag;
    real beta_dnme_prediag;
    real beta_ie_prediag;
    real beta_dme_intsvtx;
    real beta_dnme_intsvtx;
    real beta_ie_intsvtx;
    real beta_dme_contntx;
    real beta_dnme_contntx;
    real beta_ie_contntx;
    
    // country-level random effects 
    real randeff_dme_prediag[N_country];
    real randeff_dnme_prediag[N_country];
    real randeff_ie_prediag[N_country];
    real randeff_dme_intsvtx[N_country];
    real randeff_dnme_intsvtx[N_country];
    real randeff_ie_intsvtx[N_country];
    real randeff_dme_contntx[N_country];
    real randeff_dnme_contntx[N_country];
    real randeff_ie_contntx[N_country];
    
    // dispersion of country-level random effects 
    real<lower=0> tau_randeff_dme_prediag;
    real<lower=0> tau_randeff_dnme_prediag;
    real<lower=0> tau_randeff_ie_prediag;
    real<lower=0> tau_randeff_dme_intsvtx;
    real<lower=0> tau_randeff_dnme_intsvtx;
    real<lower=0> tau_randeff_ie_intsvtx;
    real<lower=0> tau_randeff_dme_contntx;
    real<lower=0> tau_randeff_dnme_contntx;
    real<lower=0> tau_randeff_ie_contntx;
    
    // additional variance parameter
    real<lower=0> tau;
}

// Calculate mu for each n
transformed parameters {
    //Coefficients inclusive of the country random effects
    real<lower=0> cost_dme_prediag[N_country];
    real<lower=0> cost_dnme_prediag[N_country];
    real<lower=0> cost_ie_prediag[N_country];
    real<lower=0> cost_dme_intsvtx[N_country];
    real<lower=0> cost_dnme_intsvtx[N_country];
    real<lower=0> cost_ie_intsvtx[N_country];
    real<lower=0> cost_dme_contntx[N_country];
    real<lower=0> cost_dnme_contntx[N_country];
    real<lower=0> cost_ie_contntx[N_country];
    vector<lower=0>[N] mu;
    // vector[N] rate_cost; //%% changed this from 'scale' to 'rate'. Doesnt affect code, but was otherwise was finding this very confusing
    // vector shape_cost;


    for(nc in 1:N_country){
        cost_dme_prediag[nc]  = exp(beta_dme_prediag + randeff_dme_prediag[nc]);
        cost_dnme_prediag[nc] = exp(beta_dnme_prediag + randeff_dnme_prediag[nc]);
        cost_ie_prediag[nc]   = exp(beta_ie_prediag + randeff_ie_prediag[nc]);
        cost_dme_intsvtx[nc]  = exp(beta_dme_intsvtx + randeff_dme_intsvtx[nc]);
        cost_dnme_intsvtx[nc] = exp(beta_dnme_intsvtx + randeff_dnme_intsvtx[nc]);
        cost_ie_intsvtx[nc]   = exp(beta_ie_intsvtx + randeff_ie_intsvtx[nc]);
        cost_dme_contntx[nc]  = exp(beta_dme_contntx + randeff_dme_contntx[nc]);
        cost_dnme_contntx[nc] = exp(beta_dnme_contntx + randeff_dnme_contntx[nc]);
        cost_ie_contntx[nc]   = exp(beta_ie_contntx + randeff_ie_contntx[nc]);
    }
    
    for (n in 1:N){
        // systematic component of the estimate 
        mu[n] = (cost_dme_prediag[country_id[n]]  * dme_prediag[n] +
                cost_dnme_prediag[country_id[n]] * dnme_prediag[n] +
                cost_ie_prediag[country_id[n]]   * ie_prediag[n] + 
                cost_dme_intsvtx[country_id[n]]  * dme_intsvtx[n] +
                cost_dnme_intsvtx[country_id[n]] * dnme_intsvtx[n] +
                cost_ie_intsvtx[country_id[n]]   * ie_intsvtx[n] + 
                cost_dme_contntx[country_id[n]]  * dme_contntx[n] +
                cost_dnme_contntx[country_id[n]] * dnme_contntx[n] +
                cost_dnme_contntx[country_id[n]] * ie_contntx[n]); 
                
    }
}


// The model to be estimated.
model {
    // Priors for fixed effects  
    beta_dme_prediag  ~ normal(0, 10);
    beta_dnme_prediag ~ normal(0, 10);
    beta_ie_prediag   ~ normal(0, 10);
    beta_dme_intsvtx  ~ normal(0, 10);
    beta_dnme_intsvtx ~ normal(0, 10);
    beta_ie_intsvtx   ~ normal(0, 10);
    beta_dme_contntx  ~ normal(0, 10);
    beta_dnme_contntx ~ normal(0, 10);
    beta_ie_contntx   ~ normal(0, 10);
    
    // Priors for random slopes  
    randeff_dme_prediag  ~ normal(0, tau_randeff_dme_prediag);
    randeff_dnme_prediag ~ normal(0, tau_randeff_dnme_prediag);
    randeff_ie_prediag   ~ normal(0, tau_randeff_ie_prediag);
    randeff_dme_intsvtx  ~ normal(0, tau_randeff_dme_intsvtx);
    randeff_dnme_intsvtx ~ normal(0, tau_randeff_dnme_intsvtx);
    randeff_ie_intsvtx   ~ normal(0, tau_randeff_ie_intsvtx);
    randeff_dme_contntx  ~ normal(0, tau_randeff_dme_contntx);
    randeff_dnme_contntx ~ normal(0, tau_randeff_dnme_contntx);
    randeff_ie_contntx   ~ normal(0, tau_randeff_ie_contntx);
    
    // Priors for dispersion of random effect
    tau_randeff_dme_prediag  ~ normal(0, 10);
    tau_randeff_dnme_prediag ~ normal(0, 10);
    tau_randeff_ie_prediag   ~ normal(0, 10);
    tau_randeff_dme_intsvtx  ~ normal(0, 10);
    tau_randeff_dnme_intsvtx ~ normal(0, 10);
    tau_randeff_ie_intsvtx   ~ normal(0, 10);
    tau_randeff_dme_contntx  ~ normal(0, 10);
    tau_randeff_dnme_contntx ~ normal(0, 10);
    tau_randeff_ie_contntx   ~ normal(0, 10);
    
    // Priors for dispersion parameter  %% half-normal probably better
    //shape_cost ~ normal(0, 800);
    // tau ~ exponential(1);
    tau ~ normal(0, 10); 
    
    // cost estimate
    cost ~ alt_gamma(tau, mu);
}

generated quantities {
  //  vector[N] y_rep;
  //  for(n in 1:N){
  // // posterior draws to get posterior predictive checks
  //      y_rep[n] <- gamma_rng(shape_cost[n], rate_cost[n]);
  //  }
}
