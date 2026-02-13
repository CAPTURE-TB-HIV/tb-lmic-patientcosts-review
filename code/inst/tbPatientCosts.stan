functions {
    real alt_gamma_lpdf(vector x, real tau, vector mu) {
        int N = num_elements(x);
        return (tau - 1) * sum(log(x)) + tau * sum(log(tau) - log(mu)) - N * 
                lgamma(tau) - sum(x * tau ./ mu);
    }
}

data {
    int<lower=1> N;          // number of data points        
    vector<lower=0>[N] cost; // response variable - cost reported by the study
    
    real dme_prediag[N];     // indicator of whether cost includes direct medical
                             // pre-diagnosis costs
    real dnme_prediag[N];    // indicator of whether cost includes direct non-
                             // medical pre-diagnosis costs
    real ie_prediag[N];      // indicator of whether cost includes indirect
                             // pre-diagnosis costs
    real dme_intsvtx[N];     // indicator of whether cost includes direct medical
                             // intensive TB treatment costs
    real dnme_intsvtx[N];    // indicator of whether cost includes direct non-
                             // medical intensive TB treatment costs
    real ie_intsvtx[N];      // indicator of whether cost includes indirect
                             // intensive treatment costs
    real dme_contntx[N];     // indicator of whether cost includes direct medical
                             // continuation TB treatment costs
    real dnme_contntx[N];    // indicator of whether cost includes direct non-
                             // medical continuation TB treatment costs
    real ie_contntx[N];      // indicator of whether cost includes indirect
                             // continuation treatment costs
    
    // Random effects
    // int study_id[N];      // Survey identifiers
    int N_country;           // Number of countries
    int country_id[N];       // Country identifier
    
    // Covariate data 
    // internal covariates 
    real drtb_ind[N];       // indicator of whether study is DRTB
    real survey_year[N];     // mean-centered survey end year 
    
    // external covariates
    real median_income[N];  // log-scale daily median income formatted in covariates.R
    // real median_income[N_country];  // log-scale daily median income formatted in covariates.R
    vector<lower=0>[N] se;         // study reported dispersion estimate 
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
    real randeff_country[N_country];
    
    // dispersion of country-level random effects 
    real<lower=0> tau_randeff_country;
    
    // DR-TB multiplier coefficient 
    // real beta_drtb; 
    
    // Covariates 
    // DR-TB multiplier coefficients 
    real beta_drtb_dme_prediag;
    real beta_drtb_dnme_prediag;
    real beta_drtb_ie_prediag;
    
    real beta_drtb_dme_intsvtx;
    real beta_drtb_dnme_intsvtx;
    real beta_drtb_ie_intsvtx;
    
    real beta_drtb_dme_contntx;
    real beta_drtb_dnme_contntx;
    real beta_drtb_ie_contntx;
    
    // Study end year
    real beta_year_dme_prediag;
    real beta_year_dnme_prediag;
    real beta_year_ie_prediag;
    
    real beta_year_dme_intsvtx;
    real beta_year_dnme_intsvtx;
    real beta_year_ie_intsvtx;
    
    real beta_year_dme_contntx;
    real beta_year_dnme_contntx;
    real beta_year_ie_contntx;
    
    // Median income
    real beta_med_income_dme_prediag;
    real beta_med_income_dnme_prediag;
    real beta_med_income_ie_prediag;
    
    real beta_med_income_dme_intsvtx;
    real beta_med_income_dnme_intsvtx;
    real beta_med_income_ie_intsvtx;
    
    real beta_med_income_dme_contntx;
    real beta_med_income_dnme_contntx;
    real beta_med_income_ie_contntx;
    
    // Additional variance parameter
    real<lower=0> tau;
}

// Calculate mu for each n
transformed parameters {
    ///////////////////////////////////////////////////////////////////////////
    // Define parameters
    // Split into study and country-level variation to make the calculation 
    // more readable / easier to follow. 
    ///////////////////////////////////////////////////////////////////////////
    // Coefficients for study variant parameters
    // real<lower=0> stdy_cost_dme_prediag[N];
    // real<lower=0> stdy_cost_dnme_prediag[N];
    // real<lower=0> stdy_cost_ie_prediag[N];
    // real<lower=0> stdy_cost_dme_intsvtx[N];
    // real<lower=0> stdy_cost_dnme_intsvtx[N];
    // real<lower=0> stdy_cost_ie_intsvtx[N];
    // real<lower=0> stdy_cost_dme_contntx[N];
    // real<lower=0> stdy_cost_dnme_contntx[N];
    // real<lower=0> stdy_cost_ie_contntx[N];
    // 
    // // Coefficients for country variant parameters
    // real<lower=0> cntry_cost_dme_prediag[N_country];
    // real<lower=0> cntry_cost_dnme_prediag[N_country];
    // real<lower=0> cntry_cost_ie_prediag[N_country];
    // real<lower=0> cntry_cost_dme_intsvtx[N_country];
    // real<lower=0> cntry_cost_dnme_intsvtx[N_country];
    // real<lower=0> cntry_cost_ie_intsvtx[N_country];
    // real<lower=0> cntry_cost_dme_contntx[N_country];
    // real<lower=0> cntry_cost_dnme_contntx[N_country];
    // real<lower=0> cntry_cost_ie_contntx[N_country];
    
    real<lower=0> stdy_cost_dme_prediag[N];
    real<lower=0> stdy_cost_dnme_prediag[N];
    real<lower=0> stdy_cost_ie_prediag[N];
    real<lower=0> stdy_cost_dme_intsvtx[N];
    real<lower=0> stdy_cost_dnme_intsvtx[N];
    real<lower=0> stdy_cost_ie_intsvtx[N];
    real<lower=0> stdy_cost_dme_contntx[N];
    real<lower=0> stdy_cost_dnme_contntx[N];
    real<lower=0> stdy_cost_ie_contntx[N];
    
    // Coefficients for country variant parameters
    // real<lower=0> cntry_cost_dme_prediag[N_country];
    // real<lower=0> cntry_cost_dnme_prediag[N_country];
    // real<lower=0> cntry_cost_ie_prediag[N_country];
    // real<lower=0> cntry_cost_dme_intsvtx[N_country];
    // real<lower=0> cntry_cost_dnme_intsvtx[N_country];
    // real<lower=0> cntry_cost_ie_intsvtx[N_country];
    // real<lower=0> cntry_cost_dme_contntx[N_country];
    // real<lower=0> cntry_cost_dnme_contntx[N_country];
    // real<lower=0> cntry_cost_ie_contntx[N_country];
    
    // Mean cost estimate
    vector<lower=0>[N] mu;
    
    ///////////////////////////////////////////////////////////////////////////
    // study level covariates 
    ///////////////////////////////////////////////////////////////////////////
    for(n in 1:N){
        stdy_cost_dme_prediag[n]  = exp(beta_year_dme_prediag  * survey_year[n] + 
                                        beta_drtb_dme_prediag  * drtb_ind[n] +
                                        beta_med_income_dme_prediag * median_income[n]);
        stdy_cost_dnme_prediag[n] = exp(beta_year_dnme_prediag * survey_year[n] + 
                                        beta_drtb_dnme_prediag * drtb_ind[n] + 
                                        beta_med_income_dnme_prediag * median_income[n]);
        stdy_cost_ie_prediag[n]   = exp(beta_year_ie_prediag   * survey_year[n] + 
                                        beta_drtb_ie_prediag   * drtb_ind[n] + 
                                        beta_med_income_ie_prediag * median_income[n]);
        stdy_cost_dme_intsvtx[n]  = exp(beta_year_dme_intsvtx  * survey_year[n] + 
                                        beta_drtb_dme_intsvtx  * drtb_ind[n] + 
                                        beta_med_income_dme_intsvtx * median_income[n]);
        stdy_cost_dnme_intsvtx[n] = exp(beta_year_dnme_intsvtx * survey_year[n] +
                                        beta_drtb_dnme_intsvtx * drtb_ind[n] + 
                                        beta_med_income_dnme_intsvtx * median_income[n]);
        stdy_cost_ie_intsvtx[n]   = exp(beta_year_ie_intsvtx   * survey_year[n] + 
                                        beta_drtb_ie_intsvtx   * drtb_ind[n] + 
                                        beta_med_income_ie_intsvtx * median_income[n]);
        stdy_cost_dme_contntx[n]  = exp(beta_year_dme_contntx  * survey_year[n] + 
                                        beta_drtb_dme_contntx  * drtb_ind[n] + 
                                        beta_med_income_dme_contntx * median_income[n]);
        stdy_cost_dnme_contntx[n] = exp(beta_year_dnme_contntx * survey_year[n] + 
                                        beta_drtb_dnme_contntx * drtb_ind[n] + 
                                        beta_med_income_dnme_contntx * median_income[n]);
        stdy_cost_ie_contntx[n]   = exp(beta_year_ie_contntx   * survey_year[n] + 
                                        beta_drtb_ie_contntx   * drtb_ind[n] + 
                                        beta_med_income_ie_contntx * median_income[n]);
    }
    ///////////////////////////////////////////////////////////////////////////
    // country level covariates 
    ///////////////////////////////////////////////////////////////////////////
    // for(nc in 1:N_country){
    //     cntry_cost_dme_prediag[nc]  = exp(beta_med_income_dme_prediag * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_dnme_prediag[nc] = exp(beta_med_income_dnme_prediag * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_ie_prediag[nc]   = exp(beta_med_income_ie_prediag * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_dme_intsvtx[nc]  = exp(beta_med_income_dme_intsvtx * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_dnme_intsvtx[nc] = exp(beta_med_income_dnme_intsvtx * median_income[nc] +
    //                                   randeff_country[nc]);
    //     cntry_cost_ie_intsvtx[nc]   = exp(beta_med_income_ie_intsvtx * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_dme_contntx[nc]  = exp(beta_med_income_dme_contntx * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_dnme_contntx[nc] = exp(beta_med_income_dnme_contntx * median_income[nc] + 
    //                                   randeff_country[nc]);
    //     cntry_cost_ie_contntx[nc]   = exp(beta_med_income_ie_contntx * median_income[nc] + 
    //                                   randeff_country[nc]);
    // }
    ///////////////////////////////////////////////////////////////////////////
    // Calculate mean cost estimate 
    ///////////////////////////////////////////////////////////////////////////
    for (n in 1:N){
        mu[n] = exp(beta_dme_prediag + randeff_country[country_id[n]]) * 
                     // cntry_cost_dme_prediag[country_id[n]] * 
                     stdy_cost_dme_prediag[n] * 
                     dme_prediag[n]   +
                 exp(beta_dnme_prediag + randeff_country[country_id[n]]) * 
                     // cntry_cost_dnme_prediag[country_id[n]] * 
                     stdy_cost_dnme_prediag[n] * 
                     dnme_prediag[n] +
                 exp(beta_ie_prediag + randeff_country[country_id[n]]) * 
                     // cntry_cost_ie_prediag[country_id[n]]  * 
                     stdy_cost_ie_prediag[n] * 
                     ie_prediag[n]   + 
                 exp(beta_dme_intsvtx + randeff_country[country_id[n]]) * 
                     // cntry_cost_dme_intsvtx[country_id[n]] * 
                     stdy_cost_dme_intsvtx[n] * 
                     dme_intsvtx[n]  +
                 exp(beta_dnme_intsvtx + randeff_country[country_id[n]]) * 
                     // cntry_cost_dnme_intsvtx[country_id[n]] * 
                     stdy_cost_dnme_intsvtx[n]* 
                     dnme_intsvtx[n] +
                 exp(beta_ie_intsvtx + randeff_country[country_id[n]]) * 
                     // cntry_cost_ie_intsvtx[country_id[n]]  * 
                     stdy_cost_ie_intsvtx[n]  * 
                     ie_intsvtx[n]   + 
                 exp(beta_dme_contntx + randeff_country[country_id[n]]) * 
                     // cntry_cost_dme_contntx[country_id[n]] * 
                     stdy_cost_dme_contntx[n] * 
                     dme_contntx[n]  +
                 exp(beta_dnme_contntx + randeff_country[country_id[n]]) * 
                     // cntry_cost_dnme_contntx[country_id[n]] * 
                     stdy_cost_dnme_contntx[n]* 
                     dnme_contntx[n] +
                 exp(beta_ie_contntx + randeff_country[country_id[n]]) * 
                     // cntry_cost_ie_contntx[country_id[n]]  * 
                     stdy_cost_ie_contntx[n] * 
                     ie_contntx[n];     //+
                // exp(beta_median_income) * median_income[n]) * 
                // exp(beta_drtb * drtb_ind[n]); # present it in the regression equations above in writing
        
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
    
    // Priors for internal covariate fixed effects 
    // DR TB indicator
    // beta_drtb ~ normal(0,10); 
    beta_drtb_dme_prediag  ~ normal(0, 10);
    beta_drtb_dnme_prediag ~ normal(0, 10);
    beta_drtb_ie_prediag   ~ normal(0, 10);
    beta_drtb_dme_intsvtx  ~ normal(0, 10);
    beta_drtb_dnme_intsvtx ~ normal(0, 10);
    beta_drtb_ie_intsvtx   ~ normal(0, 10);
    beta_drtb_dme_contntx  ~ normal(0, 10);
    beta_drtb_dnme_contntx ~ normal(0, 10);
    beta_drtb_ie_contntx   ~ normal(0, 10);
    
    // Mean centered study-end year
    beta_year_dme_prediag  ~ normal(0, 10);
    beta_year_dnme_prediag ~ normal(0, 10);
    beta_year_ie_prediag   ~ normal(0, 10);
    beta_year_dme_intsvtx  ~ normal(0, 10);
    beta_year_dnme_intsvtx ~ normal(0, 10);
    beta_year_ie_intsvtx   ~ normal(0, 10);
    beta_year_dme_contntx  ~ normal(0, 10);
    beta_year_dnme_contntx ~ normal(0, 10);
    beta_year_ie_contntx   ~ normal(0, 10);
    
    // Priors for external covariate fixed effects 
    // Daily median income 
    beta_med_income_dme_prediag  ~ normal(0, 10);
    beta_med_income_dnme_prediag ~ normal(0, 10);
    beta_med_income_ie_prediag   ~ normal(0, 10);
    beta_med_income_dme_intsvtx  ~ normal(0, 10);
    beta_med_income_dnme_intsvtx ~ normal(0, 10);
    beta_med_income_ie_intsvtx   ~ normal(0, 10);
    beta_med_income_dme_contntx  ~ normal(0, 10);
    beta_med_income_dnme_contntx ~ normal(0, 10);
    beta_med_income_ie_contntx   ~ normal(0, 10);
    
    // Priors for random slopes  
    randeff_country ~ normal(0, tau_randeff_country);
    
    // Priors for dispersion of random effect
    tau_randeff_country  ~ normal(0, 10); //default prior should be student t (CHECK!!!)
    
    // Priors for dispersion parameter  %% half-normal probably better
    tau ~ normal(0, 10); //default prior sh/could be student t (CHECK!!!)
    
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
