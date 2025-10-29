data {
    int<lower=1> N;         // number of data points        
    real<lower=0>cost[N];   // response variable - cost reported by the study
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
}

transformed data {
    vector<lower=0>[N] se2 = square(se); 
}

parameters {
    // overall effect for the different cost components 
    real<lower=0> eff_dme_prediag;
    real<lower=0> eff_dnme_prediag;
    real<lower=0> eff_ie_prediag;
    real<lower=0> eff_dme_intsvtx;
    real<lower=0> eff_dnme_intsvtx;
    real<lower=0> eff_ie_intsvtx;
    real<lower=0> eff_dme_contntx;
    real<lower=0> eff_dnme_contntx;
    real<lower=0> eff_ie_contntx;
    
    // country-level random effects 
    real<lower=0> randslp_dme_prediag[N_country];
    real<lower=0> randslp_dnme_prediag[N_country];
    real<lower=0> randslp_ie_prediag[N_country];
    real<lower=0> randslp_dme_intsvtx[N_country];
    real<lower=0> randslp_dnme_intsvtx[N_country];
    real<lower=0> randslp_ie_intsvtx[N_country];
    real<lower=0> randslp_dme_contntx[N_country];
    real<lower=0> randslp_dnme_contntx[N_country];
    real<lower=0> randslp_ie_contntx[N_country];
    
    // dispersion of country-level random effects 
    real<lower=0> scale_randslp_dme_prediag[N_country];
    real<lower=0> scale_randslp_dnme_prediag[N_country];
    real<lower=0> scale_randslp_ie_prediag[N_country];
    real<lower=0> scale_randslp_dme_intsvtx[N_country];
    real<lower=0> scale_randslp_dnme_intsvtx[N_country];
    real<lower=0> scale_randslp_ie_intsvtx[N_country];
    real<lower=0> scale_randslp_dme_contntx[N_country];
    real<lower=0> scale_randslp_dnme_contntx[N_country];
    real<lower=0> scale_randslp_ie_contntx[N_country];
    
    // additional variance parameter
    vector<lower=0>[N] add_var;
}

// Calculate mu for each n
transformed parameters {
    //Coefficients inclusive of the country random effects
    real<lower=0> coef_dme_prediag[N_country];
    real<lower=0> coef_dnme_prediag[N_country];
    real<lower=0> coef_ie_prediag[N_country];
    real<lower=0> coef_dme_intsvtx[N_country];
    real<lower=0> coef_dnme_intsvtx[N_country];
    real<lower=0> coef_ie_intsvtx[N_country];
    real<lower=0> coef_dme_contntx[N_country];
    real<lower=0> coef_dnme_contntx[N_country];
    real<lower=0> coef_ie_contntx[N_country];
    vector<lower=0>[N] mu;
    vector[N] scale_cost; 
    vector<lower=0>[N] shape_cost;
    
    for(nc in 1:N_country){
        coef_dme_prediag[nc] = eff_dme_prediag + randslp_dme_prediag[nc];
        coef_dnme_prediag[nc] = eff_dnme_prediag + randslp_dnme_prediag[nc];
        coef_ie_prediag[nc] = eff_ie_prediag + randslp_ie_prediag[nc];
        coef_dme_intsvtx[nc] = eff_dme_intsvtx + randslp_dme_intsvtx[nc];
        coef_dnme_intsvtx[nc] = eff_dnme_intsvtx + randslp_dnme_intsvtx[nc];
        coef_ie_intsvtx[nc] = eff_ie_intsvtx + randslp_ie_intsvtx[nc];
        coef_dme_contntx[nc] = eff_dme_contntx + randslp_dme_contntx[nc];
        coef_dnme_contntx[nc] = eff_dnme_contntx + randslp_dnme_contntx[nc];
        coef_ie_contntx[nc] = eff_ie_contntx + randslp_ie_contntx[nc];
    }
    
    for (n in 1:N){
        // systematic component of the estimate 
        mu[n] = coef_dme_prediag[country_id[n]] * dme_prediag[n] +
                coef_dnme_prediag[country_id[n]] * dnme_prediag[n] +
                coef_ie_prediag[country_id[n]] * ie_prediag[n] + 
                coef_dme_intsvtx[country_id[n]] * dme_intsvtx[n] +
                coef_dnme_intsvtx[country_id[n]] * dnme_intsvtx[n] +
                coef_ie_intsvtx[country_id[n]] * ie_intsvtx[n] + 
                coef_dme_contntx[country_id[n]] * dme_contntx[n] +
                coef_dnme_contntx[country_id[n]] * dnme_contntx[n] +
                coef_ie_contntx[country_id[n]] * ie_contntx[n]; 
        // shape parameter
        shape_cost[n] = se2[n] + add_var[n]; 
        // rate parameter
        scale_cost[n] = shape_cost[n] / mu[n];
    }
}


// The model to be estimated.
model {
    // Priors for fixed effects 
    eff_dme_prediag  ~ gamma(10, 100);
    eff_dnme_prediag ~ gamma(10, 100);
    eff_ie_prediag ~ gamma(10, 100);
    eff_dme_intsvtx ~ gamma(10, 100);
    eff_dnme_intsvtx ~ gamma(10, 100);
    eff_ie_intsvtx ~ gamma(10, 100);
    eff_dme_contntx ~ gamma(10, 100);
    eff_dnme_contntx ~ gamma(10, 100);
    eff_ie_contntx ~ gamma(10, 100);
    
    // Priors for random slopes  
    randslp_dme_prediag  ~ gamma(10, scale_randslp_dme_prediag);
    randslp_dnme_prediag ~ gamma(10, scale_randslp_dnme_prediag);
    randslp_ie_prediag ~ gamma(10, scale_randslp_ie_prediag);
    randslp_dme_intsvtx ~ gamma(10, scale_randslp_dme_intsvtx);
    randslp_dnme_intsvtx ~ gamma(10, scale_randslp_dnme_intsvtx);
    randslp_ie_intsvtx ~ gamma(10, scale_randslp_ie_intsvtx);
    randslp_dme_contntx ~ gamma(10, scale_randslp_dme_contntx);
    randslp_dnme_contntx ~ gamma(10, scale_randslp_dnme_contntx);
    randslp_ie_contntx ~ gamma(10, scale_randslp_ie_contntx);
    
    // Priors for dispersion of random slopes  
    scale_randslp_dme_prediag  ~ normal(10, 100);
    scale_randslp_dnme_prediag ~ normal(10, 100);
    scale_randslp_ie_prediag ~ normal(10, 100);
    scale_randslp_dme_intsvtx ~ normal(10, 100);
    scale_randslp_dnme_intsvtx ~ normal(10, 100);
    scale_randslp_ie_intsvtx ~ normal(10, 100);
    scale_randslp_dme_contntx ~ normal(10, 100);
    scale_randslp_dnme_contntx ~ normal(10, 100);
    scale_randslp_ie_contntx ~ normal(10, 100);
    
    // Priors for dispersion parameter 
    add_var ~ gamma(10, 100);
    
    // cost estimate
    cost ~ gamma(shape_cost, scale_cost);
}

generated quantities {
    vector[N] y_rep;
    for(n in 1:N){
        //posterior draws to get posterior predictive checks
        y_rep[n] <- gamma_rng(shape_cost[n], scale_cost[n]); 
    }
}
