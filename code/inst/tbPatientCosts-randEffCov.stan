/// An alternate parameterization of the gamma log prob dist 
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
    real median_income[N];  // log-scale, mean-centered daily median income formatted in covariates.R
    real oop_perc_che[N];   // mean-centered out of pocket expenses as a perc of current health expenditure
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
    real randeff_country[N_country];
    
    // dispersion of country-level random effects 
    real<lower=0> tau_randeff_country;
    
    // Covariates 
    // We are going to use a random effect setup 
    // Each covariate will have an overall effect plus the
    // nine effects for each of the cost categories: 
    // 10 covariates in total. 
    
    // DR-TB multiplier coefficients 
    real beta_drtb_overall; 
    
    real beta_drtb_dme_prediag;
    real beta_drtb_dnme_prediag;
    real beta_drtb_ie_prediag;

    real beta_drtb_dme_intsvtx;
    real beta_drtb_dnme_intsvtx;
    real beta_drtb_ie_intsvtx;

    real beta_drtb_dme_contntx;
    real beta_drtb_dnme_contntx;
    real beta_drtb_ie_contntx;
    
    real<lower=0> tau_drtb;    
    
    // Study end year
    real beta_year_overall; 

    real beta_year_dme_prediag;
    real beta_year_dnme_prediag;
    real beta_year_ie_prediag;

    real beta_year_dme_intsvtx;
    real beta_year_dnme_intsvtx;
    real beta_year_ie_intsvtx;

    real beta_year_dme_contntx;
    real beta_year_dnme_contntx;
    real beta_year_ie_contntx;
    
    real<lower=0> tau_year;
    
    // Median income
    real beta_med_income_overall; 

    real beta_med_income_dme_prediag;
    real beta_med_income_dnme_prediag;
    real beta_med_income_ie_prediag;

    real beta_med_income_dme_intsvtx;
    real beta_med_income_dnme_intsvtx;
    real beta_med_income_ie_intsvtx;

    real beta_med_income_dme_contntx;
    real beta_med_income_dnme_contntx;
    real beta_med_income_ie_contntx;
    
    real<lower=0> tau_med_income;
    
    // OOP as perc of current health expenditures
    real beta_oop_perc_che_overall; 

    real beta_oop_perc_che_dme_prediag;
    real beta_oop_perc_che_dnme_prediag;
    real beta_oop_perc_che_ie_prediag;

    real beta_oop_perc_che_dme_intsvtx;
    real beta_oop_perc_che_dnme_intsvtx;
    real beta_oop_perc_che_ie_intsvtx;

    real beta_oop_perc_che_dme_contntx;
    real beta_oop_perc_che_dnme_contntx;
    real beta_oop_perc_che_ie_contntx;
    
    real<lower=0> tau_oop_perc_che;

    
    // Additional variance parameter
    real<lower=0> tau;
}

// Calculate mu for each n
transformed parameters {

// Mean cost estimate
    vector<lower=0>[N] mu;
    

///////////////////////////////////////////////////////////////////////////
// Calculate mean cost estimate 
///////////////////////////////////////////////////////////////////////////
for (n in 1:N){
        mu[n] =  exp(beta_dme_prediag + randeff_country[country_id[n]] + 
                     (beta_med_income_dme_prediag + beta_med_income_overall) * median_income[n] + 
                     (beta_oop_perc_che_dme_prediag + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dme_prediag + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_dme_prediag + beta_year_overall)  * survey_year[n]) *
                 dme_prediag[n]  +
                 
                 exp(beta_dnme_prediag + randeff_country[country_id[n]] +
                     (beta_med_income_dnme_prediag + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_dnme_prediag + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dnme_prediag + beta_drtb_overall)  * drtb_ind[n]) *
                 exp((beta_year_dnme_prediag + beta_year_overall)  * survey_year[n]) *
                 dnme_prediag[n] +

                 exp(beta_ie_prediag + randeff_country[country_id[n]] +
                     (beta_med_income_ie_prediag + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_ie_prediag + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_ie_prediag + beta_drtb_overall)  * drtb_ind[n]) *
                 exp((beta_year_ie_prediag + beta_year_overall)  * survey_year[n]) *
                 ie_prediag[n]  +

                 exp(beta_dme_intsvtx + randeff_country[country_id[n]] +
                     (beta_med_income_dme_intsvtx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_dme_intsvtx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dme_intsvtx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_dme_intsvtx + beta_year_overall)  * survey_year[n]) *
                 dme_intsvtx[n]  +

                 exp(beta_dnme_intsvtx + randeff_country[country_id[n]] +
                     (beta_med_income_dnme_intsvtx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_dnme_intsvtx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dnme_intsvtx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_dnme_intsvtx + beta_year_overall)  * survey_year[n]) *
                 dnme_intsvtx[n] +

                 exp(beta_ie_intsvtx + randeff_country[country_id[n]] +
                     (beta_med_income_ie_intsvtx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_ie_intsvtx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_ie_intsvtx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_ie_intsvtx + beta_year_overall)  * survey_year[n]) *
                 ie_intsvtx[n]   +

                 exp(beta_dme_contntx + randeff_country[country_id[n]] +
                     (beta_med_income_dme_contntx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_dnme_contntx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dme_contntx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_dme_contntx + beta_year_overall)  * survey_year[n]) *
                 dme_contntx[n]  +

                 exp(beta_dnme_contntx + randeff_country[country_id[n]] +
                     (beta_med_income_dnme_contntx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_dnme_contntx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_dnme_contntx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_dnme_contntx + beta_year_overall)  * survey_year[n]) *
                 dnme_contntx[n] +

                 exp(beta_ie_contntx + randeff_country[country_id[n]] +
                     (beta_med_income_ie_contntx + beta_med_income_overall) * median_income[n] +
                     (beta_oop_perc_che_ie_contntx + beta_oop_perc_che_overall) * oop_perc_che[n]) *
                 exp((beta_drtb_ie_contntx + beta_drtb_overall) * drtb_ind[n]) *
                 exp((beta_year_ie_contntx + beta_year_overall)  * survey_year[n]) *
                 ie_contntx[n];
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
    
    // Priors for internal covariates
    // DR TB indicator
    beta_drtb_overall ~ normal(0,10);
    beta_drtb_dme_prediag  ~ normal(0, tau_drtb);
    beta_drtb_dnme_prediag ~ normal(0, tau_drtb);
    beta_drtb_ie_prediag   ~ normal(0, tau_drtb);
    beta_drtb_dme_intsvtx  ~ normal(0, tau_drtb);
    beta_drtb_dnme_intsvtx ~ normal(0, tau_drtb);
    beta_drtb_ie_intsvtx   ~ normal(0, tau_drtb);
    beta_drtb_dme_contntx  ~ normal(0, tau_drtb);
    beta_drtb_dnme_contntx ~ normal(0, tau_drtb);
    beta_drtb_ie_contntx   ~ normal(0, tau_drtb);
    // 
    // Mean centered study-end year
    beta_year_overall ~ normal(0,10);
    beta_year_dme_prediag  ~ normal(0, tau_year);
    beta_year_dnme_prediag ~ normal(0, tau_year);
    beta_year_ie_prediag   ~ normal(0, tau_year);
    beta_year_dme_intsvtx  ~ normal(0, tau_year);
    beta_year_dnme_intsvtx ~ normal(0, tau_year);
    beta_year_ie_intsvtx   ~ normal(0, tau_year);
    beta_year_dme_contntx  ~ normal(0, tau_year);
    beta_year_dnme_contntx ~ normal(0, tau_year);
    beta_year_ie_contntx   ~ normal(0, tau_year);

    // Priors for external covariate fixed effects 
    // Median income 
    beta_med_income_overall ~ normal(0,10);
    beta_med_income_dme_prediag  ~ normal(0, tau_med_income);
    beta_med_income_dnme_prediag ~ normal(0, tau_med_income);
    beta_med_income_ie_prediag   ~ normal(0, tau_med_income);
    beta_med_income_dme_intsvtx  ~ normal(0, tau_med_income);
    beta_med_income_dnme_intsvtx ~ normal(0, tau_med_income);
    beta_med_income_ie_intsvtx   ~ normal(0, tau_med_income);
    beta_med_income_dme_contntx  ~ normal(0, tau_med_income);
    beta_med_income_dnme_contntx ~ normal(0, tau_med_income);
    beta_med_income_ie_contntx   ~ normal(0, tau_med_income);
    
    // OOP as perc of CHE
    beta_oop_perc_che_overall  ~ normal(0, 10);
    beta_oop_perc_che_dme_prediag  ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_dnme_prediag ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_ie_prediag   ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_dme_intsvtx  ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_dnme_intsvtx ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_ie_intsvtx   ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_dme_contntx  ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_dnme_contntx ~ normal(0, tau_oop_perc_che);
    beta_oop_perc_che_ie_contntx   ~ normal(0, tau_oop_perc_che);
    
    // Priors for the covariate random effects 
    tau_drtb ~ normal(0,10);
    tau_year ~ normal(0,10);
    tau_med_income ~ normal(0,10);
    tau_oop_perc_che  ~ normal(0,10);
    
    // Priors for random slopes  
    randeff_country ~ normal(0, tau_randeff_country);
    
    // Prior for dispersion of random effect
    tau_randeff_country  ~ normal(0, 10); //default prior should be student t (CHECK!!!)
    
    // Priors for dispersion parameter  %% half-normal probably better
    tau ~ normal(0, 10); //default prior sh/could be student t (CHECK!!!)
    
    // cost estimate
    cost ~ alt_gamma(tau, mu);
}

generated quantities {
  // Add log_likelihood for model comparison
  vector[N] log_lik;
  for (n in 1:N) {
    vector[1] cost_n = [cost[n]]';
    vector[1] mu_n = [mu[n]]';
    log_lik[n] = alt_gamma_lpdf(cost_n | tau, mu_n);
  }
}
