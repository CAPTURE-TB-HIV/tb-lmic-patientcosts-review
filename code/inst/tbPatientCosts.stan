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
    int N_survey;            // Number of surveys
    int survey_id[N];        // Survey identifiers
    int N_country;           // Number of countries
    int country_id[N];       // Country identifier
    
    // Covariate data 
    // internal covariates 
    real drtb_ind[N];       // indicator of whether study is DRTB
    real survey_year[N];     // mean-centered survey end year
    
    // external covariates
    real median_income[N];  // log-scale, mean-centered daily median income formatted in covariates.R
    real oop_perc_che[N];   // mean-centered out of pocket expenses as a perc of current health expenditure
    real uhc_index[N]; // mean-centered index of health care quality
    
    // dispersion
    vector<lower=0>[N] se;  // study reported dispersion estimate 
    
    int prior_sigma; /// for normal priors below
    
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
    
    // Random effects 
    // survey-level 
    real randeff_survey[N_survey];

    // country-level 
    real randeff_country[N_country];
    
    // dispersion of survey-level random effects 
    real<lower=0> tau_randeff_survey;

    // dispersion of country-level random effects 
    real<lower=0> tau_randeff_country;
    
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
    
    real<lower=0> tau_drtb; 
    
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
    
    real<lower=0> tau_year; 
    
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
    
    real<lower=0> tau_med_income;
    
    // OOP as perc of current health expenditures
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
    
    // UHC Index 
    real beta_uhc_dme_prediag;
    real beta_uhc_dnme_prediag;
    real beta_uhc_ie_prediag;

    real beta_uhc_dme_intsvtx;
    real beta_uhc_dnme_intsvtx;
    real beta_uhc_ie_intsvtx;

    real beta_uhc_dme_contntx;
    real beta_uhc_dnme_contntx;
    real beta_uhc_ie_contntx;
    
    real<lower=0> tau_uhc;
    
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
        mu[n] =  exp(beta_dme_prediag + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) + 
                     beta_med_income_dme_prediag * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dme_prediag * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dme_prediag  * uhc_index[n]) *
                 exp(beta_drtb_dme_prediag  * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dme_prediag  * survey_year[n]) *
                 dme_prediag[n]   +
                 
                 exp(beta_dnme_prediag + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) + 
                     beta_med_income_dnme_prediag * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dnme_prediag * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dnme_prediag  * uhc_index[n]) * 
                 exp(beta_drtb_dnme_prediag  * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dnme_prediag  * survey_year[n]) *
                 dnme_prediag[n] +
                 
                 exp(beta_ie_prediag + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) + 
                     beta_med_income_ie_prediag * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_ie_prediag * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_ie_prediag  * uhc_index[n]) * 
                 exp(beta_drtb_ie_prediag  * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_ie_prediag  * survey_year[n]) *
                 ie_prediag[n]   + 
                 
                 exp(beta_dme_intsvtx + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) + 
                     beta_med_income_dme_intsvtx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dme_intsvtx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dme_intsvtx  * uhc_index[n]) * 
                 exp(beta_drtb_dme_intsvtx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dme_intsvtx  * survey_year[n]) *
                 dme_intsvtx[n]  +
                 
                 exp(beta_dnme_intsvtx + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) +
                     beta_med_income_dnme_intsvtx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dnme_intsvtx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dnme_intsvtx  * uhc_index[n]) * 
                 exp(beta_drtb_dnme_intsvtx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dnme_intsvtx  * survey_year[n]) *
                 dnme_intsvtx[n] +
                 
                 exp(beta_ie_intsvtx + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) +
                     beta_med_income_ie_intsvtx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_ie_intsvtx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_ie_intsvtx  * uhc_index[n]) * 
                 exp(beta_drtb_ie_intsvtx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_ie_intsvtx  * survey_year[n]) *
                 ie_intsvtx[n]   + 
                 
                 exp(beta_dme_contntx +
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) +
                     beta_med_income_dme_contntx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dnme_contntx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dme_contntx  * uhc_index[n]) * 
                 exp(beta_drtb_dme_contntx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dme_contntx  * survey_year[n]) *
                 dme_contntx[n]  +
                 
                 exp(beta_dnme_contntx + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) +
                     beta_med_income_dnme_contntx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_dnme_contntx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dme_contntx  * uhc_index[n]) * 
                 exp(beta_drtb_dnme_contntx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_dnme_contntx  * survey_year[n]) *
                 dnme_contntx[n] +
                 
                 exp(beta_ie_contntx + 
                     (randeff_survey[survey_id[n]] * tau_randeff_survey) + 
                     (randeff_country[country_id[n]] * tau_randeff_country) +
                     beta_med_income_ie_contntx * tau_med_income * median_income[n] + 
                     beta_oop_perc_che_ie_contntx * tau_oop_perc_che * oop_perc_che[n] + 
                     beta_uhc_dme_contntx  * uhc_index[n]) * 
                 exp(beta_drtb_ie_contntx * tau_drtb * drtb_ind[n]) *
                 exp(beta_year_ie_contntx  * survey_year[n]) *
                 ie_contntx[n];     
    }
}

// The model to be estimated.
model {
    // Priors for fixed effects  
    beta_dme_prediag  ~ normal(0, prior_sigma);
    beta_dnme_prediag ~ normal(0, prior_sigma);
    beta_ie_prediag   ~ normal(0, prior_sigma);
    beta_dme_intsvtx  ~ normal(0, prior_sigma);
    beta_dnme_intsvtx ~ normal(0, prior_sigma);
    beta_ie_intsvtx   ~ normal(0, prior_sigma);
    beta_dme_contntx  ~ normal(0, prior_sigma);
    beta_dnme_contntx ~ normal(0, prior_sigma);
    beta_ie_contntx   ~ normal(0, prior_sigma);
    
    // Priors for internal covariate fixed effects 
    // DR TB indicator
    // beta_drtb ~ normal(0,10);
    beta_drtb_dme_prediag  ~ normal(0, 1);
    beta_drtb_dnme_prediag ~ normal(0, 1);
    beta_drtb_ie_prediag   ~ normal(0, 1);
    beta_drtb_dme_intsvtx  ~ normal(0, 1);
    beta_drtb_dnme_intsvtx ~ normal(0, 1);
    beta_drtb_ie_intsvtx   ~ normal(0, 1);
    beta_drtb_dme_contntx  ~ normal(0, 1);
    beta_drtb_dnme_contntx ~ normal(0, 1);
    beta_drtb_ie_contntx   ~ normal(0, 1);
    // 
    // Mean centered study-end year
    beta_year_dme_prediag  ~ normal(0, 1);
    beta_year_dnme_prediag ~ normal(0, 1);
    beta_year_ie_prediag   ~ normal(0, 1);
    beta_year_dme_intsvtx  ~ normal(0, 1);
    beta_year_dnme_intsvtx ~ normal(0, 1);
    beta_year_ie_intsvtx   ~ normal(0, 1);
    beta_year_dme_contntx  ~ normal(0, 1);
    beta_year_dnme_contntx ~ normal(0, 1);
    beta_year_ie_contntx   ~ normal(0, 1);

    // Priors for external covariate fixed effects 
    // Median income 
    beta_med_income_dme_prediag  ~ normal(0, 1);
    beta_med_income_dnme_prediag ~ normal(0, 1);
    beta_med_income_ie_prediag   ~ normal(0, 1);
    beta_med_income_dme_intsvtx  ~ normal(0, 1);
    beta_med_income_dnme_intsvtx ~ normal(0, 1);
    beta_med_income_ie_intsvtx   ~ normal(0, 1);
    beta_med_income_dme_contntx  ~ normal(0, 1);
    beta_med_income_dnme_contntx ~ normal(0, 1);
    beta_med_income_ie_contntx   ~ normal(0, 1);
    
    // OOP as perc of CHE
    beta_oop_perc_che_dme_prediag  ~ normal(0, 1);
    beta_oop_perc_che_dnme_prediag ~ normal(0, 1);
    beta_oop_perc_che_ie_prediag   ~ normal(0, 1);
    beta_oop_perc_che_dme_intsvtx  ~ normal(0, 1);
    beta_oop_perc_che_dnme_intsvtx ~ normal(0, 1);
    beta_oop_perc_che_ie_intsvtx   ~ normal(0, 1);
    beta_oop_perc_che_dme_contntx  ~ normal(0, 1);
    beta_oop_perc_che_dnme_contntx ~ normal(0, 1);
    beta_oop_perc_che_ie_contntx   ~ normal(0, 1);
    
    // Mean centered UHC index
    beta_uhc_dme_prediag  ~ normal(0, tau_uhc);
    beta_uhc_dnme_prediag ~ normal(0, tau_uhc);
    beta_uhc_ie_prediag   ~ normal(0, tau_uhc);
    beta_uhc_dme_intsvtx  ~ normal(0, tau_uhc);
    beta_uhc_dnme_intsvtx ~ normal(0, tau_uhc);
    beta_uhc_ie_intsvtx   ~ normal(0, tau_uhc);
    beta_uhc_dme_contntx  ~ normal(0, tau_uhc);
    beta_uhc_dnme_contntx ~ normal(0, tau_uhc);
    beta_uhc_ie_contntx   ~ normal(0, tau_uhc);
    
    // Priors for random slopes  
    randeff_survey ~ normal(0, 1);
    randeff_country ~ normal(0, 1);
    
    // Priors for covariate sigmas 
    tau_drtb ~ normal(0, prior_sigma);
    tau_year ~ normal(0, prior_sigma);
    tau_med_income ~ normal(0, prior_sigma);
    tau_oop_perc_che  ~ normal(0, prior_sigma);
    tau_uhc ~ normal(0, prior_sigma); 
    
    // Priors for dispersion of random effects
    tau_randeff_survey   ~ normal(0, prior_sigma); 
    tau_randeff_country  ~ normal(0, prior_sigma); 
    
    // Priors for dispersion parameter
    tau ~ normal(0, prior_sigma); 
    
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
    
  // Add predictive capabilities 
  // vector[N] linpred = alpha[species] + beta_bodymass * bodymass;
  // vector[N] epred = linpred; // No inverse link function to apply here
  // array[N] real prediction = normal_rng(epred, sigma);
  }
}
