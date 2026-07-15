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
    
    // Random effects
    int N_survey;            // Number of surveys
    int survey_id[N];        // Survey identifiers
    int N_country;           // Number of countries
    int country_id[N];       // Country identifier
    // int N_natrep;            // Number of nationally representative surveys
    int natrep_id[N];         // Nationally representative survey identifier
    
    // Covariate data 
    // internal covariates 
    real drtb_ind[N];        // indicator of whether study is DRTB
    real survey_year[N];     // mean-centered survey end year

    // external covariates
    real median_income[N];  // log-scale, mean-centered daily median income formatted in covariates.R
    real oop_perc_che[N];   // mean-centered out of pocket expenses as a perc of current health expenditure
    real uhc_index[N]; // mean-centered index of health care quality
    
    // dispersion
    vector<lower=0>[N] se;  // study reported dispersion estimate 
    
    int prior_sigma; /// for normal priors below
    
    // Indicator to hold data out for country-k fold 
    // int<lower=0,upper=1> holdout[N]; 
    
    // Empirical prob that cost is zero
    real<lower=0, upper=1> prob_cost_zero; 
    
    // Predictions 
    int<lower=0> N_country_new;
    int in_review_id[N_country_new];       
    int review_ind[N_country_new];       
    real median_income_new[N_country_new];  
    real oop_perc_che_new[N_country_new];   
    real uhc_index_new[N_country_new];
    real pred_year;

    
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
    
    // nationally-rep-level
    real randeff_natrep;
    
    // dispersion of survey-level random effects 
    real<lower=0> sigma_randeff_survey;
    
    // dispersion of country-level random effects 
    real<lower=0> sigma_randeff_country;
    
     // dispersion of nat-rep random effects 
    real<lower=0> sigma_randeff_natrep;
    
    // Covariates 
    // We are going to use a random effect setup 
    // Each covariate will have an overall effect plus the
    // nine effects for each of the cost categories: 
        // prior_sigma covariates in total. 
    
    // DR-TB real multiplier coefficients 
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
    
    real<lower=0> sigma_drtb;    
    
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
    
    real<lower=0> sigma_year;
    
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
    
    real<lower=0> sigma_med_income;
    
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
    
    real<lower=0> sigma_oop_perc_che;
    
    // UHC real multiplier coefficients 
    real beta_uhc_overall; 
    
    real beta_uhc_dme_prediag;
    real beta_uhc_dnme_prediag;
    real beta_uhc_ie_prediag;
    
    real beta_uhc_dme_intsvtx;
    real beta_uhc_dnme_intsvtx;
    real beta_uhc_ie_intsvtx;
    
    real beta_uhc_dme_contntx;
    real beta_uhc_dnme_contntx;
    real beta_uhc_ie_contntx;
    
    real<lower=0> sigma_uhc;   
    
    // Additional variance parameter
    real<lower=0> tau;
    
    
    ///////////////////////////////////////////////////////////////////////////
    /// Parameters for the hurdle model as we have costs equal to zero ////////
    ///////////////////////////////////////////////////////////////////////////
    /// We will have a coefficient for each of the cost categories
    
    real<upper=0> beta_theta_dme_prediag;
    real<upper=0> beta_theta_dnme_prediag;
    real<upper=0> beta_theta_ie_prediag;

    real<upper=0> beta_theta_dme_intsvtx;
    real<upper=0> beta_theta_dnme_intsvtx;
    real<upper=0> beta_theta_ie_intsvtx;

    real<upper=0> beta_theta_dme_contntx;
    real<upper=0> beta_theta_dnme_contntx;
    real<upper=0> beta_theta_ie_contntx;
    
}

// Calculate mu for each n
transformed parameters {
    // Mean cost estimate
    vector<lower=0>[N] mu;
    // vector[N] eta;
    vector<upper=0>[N] theta;
    
    ///////////////////////////////////////////////////////////////////////////
    // Calculate mean cost estimate 
    ///////////////////////////////////////////////////////////////////////////
    for (n in 1:N){
            mu[n] =  (exp(beta_dme_prediag + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                         (beta_med_income_dme_prediag * sigma_med_income + beta_med_income_overall) * median_income[n] + 
                         (beta_oop_perc_che_dme_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                         (beta_uhc_dme_prediag * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dme_prediag * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_dme_prediag * sigma_year + beta_year_overall)  * survey_year[n]) *
                dme_prediag[n]  +
                
                exp(beta_dnme_prediag + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_dnme_prediag * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_dnme_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_dnme_prediag * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dnme_prediag * sigma_drtb + beta_drtb_overall)  * drtb_ind[n]) *
                exp((beta_year_dnme_prediag * sigma_year + beta_year_overall)  * survey_year[n]) *
                dnme_prediag[n] +
                
                exp(beta_ie_prediag + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_ie_prediag * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_ie_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_ie_prediag * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_ie_prediag * sigma_drtb + beta_drtb_overall)  * drtb_ind[n]) *
                exp((beta_year_ie_prediag * sigma_year + beta_year_overall)  * survey_year[n]) *
                ie_prediag[n]  +
                
                exp(beta_dme_intsvtx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_dme_intsvtx * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_dme_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_dme_intsvtx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dme_intsvtx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_dme_intsvtx * sigma_year + beta_year_overall)  * survey_year[n]) *
                dme_intsvtx[n]  +
                
                exp(beta_dnme_intsvtx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_dnme_intsvtx * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_dnme_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_dnme_intsvtx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dnme_intsvtx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_dnme_intsvtx * sigma_year + beta_year_overall)  * survey_year[n]) *
                dnme_intsvtx[n] +
                
                exp(beta_ie_intsvtx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_ie_intsvtx * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_ie_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_ie_intsvtx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_ie_intsvtx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_ie_intsvtx * sigma_year + beta_year_overall)  * survey_year[n]) *
                ie_intsvtx[n]   +
                
                exp(beta_dme_contntx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_dme_contntx * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_dme_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_dme_contntx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dme_contntx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_dme_contntx * sigma_year + beta_year_overall)  * survey_year[n]) *
                dme_contntx[n]  +
                
                exp(beta_dnme_contntx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_dnme_contntx * sigma_med_income + beta_med_income_overall) * median_income[n] +
                        (beta_oop_perc_che_dnme_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_dnme_contntx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_dnme_contntx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_dnme_contntx * sigma_year + beta_year_overall)  * survey_year[n]) *
                dnme_contntx[n] +
                
                exp(beta_ie_contntx + 
                        (randeff_country[country_id[n]] * sigma_randeff_country) +
                        (randeff_survey[survey_id[n]] * sigma_randeff_survey) +
                        (beta_med_income_ie_contntx   * sigma_med_income   + beta_med_income_overall)  * median_income[n] +
                        (beta_oop_perc_che_ie_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall) * oop_perc_che[n] + 
                        (beta_uhc_ie_contntx * sigma_uhc + beta_uhc_overall) * uhc_index[n]) *
                exp((beta_drtb_ie_contntx * sigma_drtb + beta_drtb_overall) * drtb_ind[n]) *
                exp((beta_year_ie_contntx * sigma_year + beta_year_overall)  * survey_year[n]) *
                ie_contntx[n]) * 
                
            /// fixed effects with only a single effect:
                exp(randeff_natrep * sigma_randeff_natrep * natrep_id[n]); 
            /// The probability that cost[n] is zero is equal to the
            /// joint probability that each component of that cost
            /// is zero, so we sum them together here (in log space). 
            theta[n]  = beta_theta_dme_prediag  * dme_prediag[n] +
                        beta_theta_dnme_prediag * dnme_prediag[n] +
                        beta_theta_ie_prediag   * ie_prediag[n] +
                        beta_theta_dme_intsvtx  * dme_intsvtx[n] +
                        beta_theta_dnme_intsvtx * dnme_intsvtx[n] +
                        beta_theta_ie_intsvtx   * ie_intsvtx[n] +
                        beta_theta_dme_contntx  * dme_contntx[n] +
                        beta_theta_dnme_contntx * dnme_contntx[n] +
                        beta_theta_ie_contntx   * ie_contntx[n];
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
    beta_drtb_overall ~ normal(0, prior_sigma);
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
    beta_year_overall ~ normal(0, prior_sigma);
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
    beta_med_income_overall ~ normal(0, prior_sigma);
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
    beta_oop_perc_che_overall  ~ normal(0, prior_sigma);
    beta_oop_perc_che_dme_prediag  ~ normal(0, 1);
    beta_oop_perc_che_dnme_prediag ~ normal(0, 1);
    beta_oop_perc_che_ie_prediag   ~ normal(0, 1);
    beta_oop_perc_che_dme_intsvtx  ~ normal(0, 1);
    beta_oop_perc_che_dnme_intsvtx ~ normal(0, 1);
    beta_oop_perc_che_ie_intsvtx   ~ normal(0, 1);
    beta_oop_perc_che_dme_contntx  ~ normal(0, 1);
    beta_oop_perc_che_dnme_contntx ~ normal(0, 1);
    beta_oop_perc_che_ie_contntx   ~ normal(0, 1);
    
    // UHC
    beta_uhc_overall ~ normal(0, prior_sigma);
    beta_uhc_dme_prediag  ~ normal(0, 1);
    beta_uhc_dnme_prediag ~ normal(0, 1);
    beta_uhc_ie_prediag   ~ normal(0, 1);
    beta_uhc_dme_intsvtx  ~ normal(0, 1);
    beta_uhc_dnme_intsvtx ~ normal(0, 1);
    beta_uhc_ie_intsvtx   ~ normal(0, 1);
    beta_uhc_dme_contntx  ~ normal(0, 1);
    beta_uhc_dnme_contntx ~ normal(0, 1);
    beta_uhc_ie_contntx   ~ normal(0, 1);
    
    // Nationally representative fixed effect
    // beta_natRep  ~ normal(0, prior_sigma);
    
    // Priors for random slopes  
    randeff_survey ~ normal(0, 1);
    randeff_country ~ normal(0, 1);
    randeff_natrep ~ normal(0, 1);
    
    // Priors for covariate sigmas 
    sigma_drtb ~ normal(0, prior_sigma);
    sigma_year ~ normal(0, prior_sigma);
    sigma_med_income ~ normal(0, prior_sigma);
    sigma_oop_perc_che  ~ normal(0, prior_sigma);
    sigma_uhc ~ normal(0, prior_sigma); 
    
    // Priors for dispersion of random effects
    sigma_randeff_survey   ~ student_t(3, 0, 2.5); 
    sigma_randeff_country  ~ student_t(3, 0, 2.5); 
    sigma_randeff_natrep   ~ student_t(3, 0, 2.5);
    
    // Priors for dispersion parameter
    tau ~ exponential(1); 
    
    // Priors for hurdle predictors  
    beta_theta_dme_prediag ~  normal(log(prob_cost_zero), 1);
    beta_theta_dnme_prediag ~ normal(log(prob_cost_zero), 1);
    beta_theta_ie_prediag ~   normal(log(prob_cost_zero), 1);

    beta_theta_dme_intsvtx  ~ normal(log(prob_cost_zero), 1);
    beta_theta_dnme_intsvtx ~ normal(log(prob_cost_zero), 1);
    beta_theta_ie_intsvtx   ~ normal(log(prob_cost_zero), 1);

    beta_theta_dme_contntx  ~ normal(log(prob_cost_zero), 1);
    beta_theta_dnme_contntx ~ normal(log(prob_cost_zero), 1);
    beta_theta_ie_contntx   ~ normal(log(prob_cost_zero), 1);
    
    /// Calculate the likelihood
    for(n in 1:N){
        // if(holdout[n] == 0){
            if (cost[n] == 0){
                // P(cost[n] = 0) 
                target += theta[n];
            } else {
                vector[1] cost_n = [cost[n]]';
                vector[1] mu_n = [mu[n]]';
                // First the hurdle (non-zero) indicator:
                target += log1m_exp(theta[n]) + 
                // Then the gamma density (no truncation needed):
                          alt_gamma_lpdf(cost_n | tau, mu_n);
        }
    } //}
}

generated quantities {
    // Add log_likelihood for model comparison
    vector[N] log_lik;
    // unconditional mean 
    vector[N] uncndtl_mu;
    
    for(n in 1:N){
            if (cost[n] == 0){
                // P(y_n = 0)
                log_lik[n] = theta[n];
            } else {
                vector[1] cost_n = [cost[n]]';
                vector[1] mu_n = [mu[n]]';
                // First the hurdle (non-zero) indicator:
                log_lik[n] = log1m_exp(theta[n]) +
                // Then the gamma density truncated at 0:
                             alt_gamma_lpdf(cost_n | tau, mu_n);
        }
    /// Calculate the unconditional mean 
    uncndtl_mu[n] = mu[n] * (1-exp(theta[n])); 
        
    }
    /// Predicted costs for each county
    vector[N_country_new] drtb_dme_prediag_cost_country;
    vector[N_country_new] dstb_dme_prediag_cost_country;
    vector[N_country_new] drtb_dnme_prediag_cost_country;
    vector[N_country_new] dstb_dnme_prediag_cost_country;
    vector[N_country_new] drtb_ie_prediag_cost_country;
    vector[N_country_new] dstb_ie_prediag_cost_country;
    
    vector[N_country_new] drtb_dme_intsvtx_cost_country;
    vector[N_country_new] dstb_dme_intsvtx_cost_country;
    vector[N_country_new] drtb_dnme_intsvtx_cost_country;
    vector[N_country_new] dstb_dnme_intsvtx_cost_country;
    vector[N_country_new] drtb_ie_intsvtx_cost_country;
    vector[N_country_new] dstb_ie_intsvtx_cost_country;
    
    vector[N_country_new] drtb_dme_contntx_cost_country;
    vector[N_country_new] dstb_dme_contntx_cost_country;
    vector[N_country_new] drtb_dnme_contntx_cost_country;
    vector[N_country_new] dstb_dnme_contntx_cost_country;
    vector[N_country_new] drtb_ie_contntx_cost_country;
    vector[N_country_new] dstb_ie_contntx_cost_country;
    
    vector[N_country_new] drtb_dme_cost_country;
    vector[N_country_new] drtb_dnme_cost_country;
    vector[N_country_new] drtb_ie_cost_country;

    vector[N_country_new] dstb_dme_cost_country;
    vector[N_country_new] dstb_dnme_cost_country;
    vector[N_country_new] dstb_ie_cost_country;
    
    vector[N_country_new] drtb_total_cost_country;
    vector[N_country_new] dstb_total_cost_country;
    
    vector[N_country_new] dstb_total_cost_country_noNatRep;
    vector[N_country_new] drtb_total_cost_country_noNatRep;

    
    vector[N_country_new] randeff_country_pred; 

    /// covariate mulipliers
    real mult_drtb_dme_prediag;
    real mult_year_dme_prediag;
    real mult_uhc_dme_prediag;
    real mult_oop_perc_che_dme_prediag;
    real mult_med_income_dme_prediag;

    real mult_drtb_dnme_prediag;
    real mult_year_dnme_prediag;
    real mult_uhc_dnme_prediag;
    real mult_oop_perc_che_dnme_prediag;
    real mult_med_income_dnme_prediag;

    real mult_drtb_ie_prediag;
    real mult_year_ie_prediag;
    real mult_uhc_ie_prediag;
    real mult_oop_perc_che_ie_prediag;
    real mult_med_income_ie_prediag;

    real mult_drtb_dme_intsvtx;
    real mult_year_dme_intsvtx;
    real mult_uhc_dme_intsvtx;
    real mult_oop_perc_che_dme_intsvtx;
    real mult_med_income_dme_intsvtx;

    real mult_drtb_dnme_intsvtx;
    real mult_year_dnme_intsvtx;
    real mult_uhc_dnme_intsvtx;
    real mult_oop_perc_che_dnme_intsvtx;
    real mult_med_income_dnme_intsvtx;

    real mult_drtb_ie_intsvtx;
    real mult_year_ie_intsvtx;
    real mult_uhc_ie_intsvtx;
    real mult_oop_perc_che_ie_intsvtx;
    real mult_med_income_ie_intsvtx;

    real mult_drtb_dme_contntx;
    real mult_year_dme_contntx;
    real mult_uhc_dme_contntx;
    real mult_oop_perc_che_dme_contntx;
    real mult_med_income_dme_contntx;

    real mult_drtb_dnme_contntx;
    real mult_year_dnme_contntx;
    real mult_uhc_dnme_contntx;
    real mult_oop_perc_che_dnme_contntx;
    real mult_med_income_dnme_contntx;

    real mult_drtb_ie_contntx;
    real mult_year_ie_contntx;
    real mult_uhc_ie_contntx;
    real mult_oop_perc_che_ie_contntx;
    real mult_med_income_ie_contntx;
    
    real mult_natrep; 
        
        // pre-treatment period
        // Covariate real multipliers
        mult_drtb_dme_prediag = beta_drtb_dme_prediag * sigma_drtb + beta_drtb_overall;
        mult_year_dme_prediag = beta_year_dme_prediag * sigma_year + beta_year_overall;
        mult_uhc_dme_prediag  = beta_uhc_dme_prediag * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dme_prediag = beta_oop_perc_che_dme_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dme_prediag = beta_med_income_dme_prediag * sigma_med_income + beta_med_income_overall;

        mult_drtb_dnme_prediag = beta_drtb_dnme_prediag * sigma_drtb + beta_drtb_overall;
        mult_year_dnme_prediag = beta_year_dnme_prediag * sigma_year + beta_year_overall;
        mult_uhc_dnme_prediag  = beta_uhc_dnme_prediag * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dnme_prediag = beta_oop_perc_che_dnme_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dnme_prediag = beta_med_income_dnme_prediag * sigma_med_income + beta_med_income_overall;

        mult_drtb_ie_prediag = beta_drtb_ie_prediag * sigma_drtb + beta_drtb_overall;
        mult_year_ie_prediag = beta_year_ie_prediag * sigma_year + beta_year_overall;
        mult_uhc_ie_prediag  = beta_uhc_ie_prediag * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_ie_prediag = beta_oop_perc_che_ie_prediag * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_ie_prediag = beta_med_income_ie_prediag * sigma_med_income + beta_med_income_overall;
        
        // intensive treatment period
        // Covariate multipliers
        mult_drtb_dme_intsvtx = beta_drtb_dme_intsvtx * sigma_drtb + beta_drtb_overall;
        mult_year_dme_intsvtx = beta_year_dme_intsvtx * sigma_year + beta_year_overall;
        mult_uhc_dme_intsvtx  = beta_uhc_dme_intsvtx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dme_intsvtx = beta_oop_perc_che_dme_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dme_intsvtx = beta_med_income_dme_intsvtx * sigma_med_income + beta_med_income_overall;

        mult_drtb_dnme_intsvtx = beta_drtb_dnme_intsvtx * sigma_drtb + beta_drtb_overall;
        mult_year_dnme_intsvtx = beta_year_dnme_intsvtx * sigma_year + beta_year_overall;
        mult_uhc_dnme_intsvtx  = beta_uhc_dnme_intsvtx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dnme_intsvtx = beta_oop_perc_che_dnme_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dnme_intsvtx = beta_med_income_dnme_intsvtx * sigma_med_income + beta_med_income_overall;

        mult_drtb_ie_intsvtx = beta_drtb_ie_intsvtx * sigma_drtb + beta_drtb_overall;
        mult_year_ie_intsvtx = beta_year_ie_intsvtx * sigma_year + beta_year_overall;
        mult_uhc_ie_intsvtx  = beta_uhc_ie_intsvtx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_ie_intsvtx = beta_oop_perc_che_ie_intsvtx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_ie_intsvtx = beta_med_income_ie_intsvtx * sigma_med_income + beta_med_income_overall;
        
     // continuation treatment period
    // Covariate multipliers
        mult_drtb_dme_contntx = beta_drtb_dme_contntx * sigma_drtb + beta_drtb_overall;
        mult_year_dme_contntx = beta_year_dme_contntx * sigma_year + beta_year_overall;
        mult_uhc_dme_contntx  = beta_uhc_dme_contntx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dme_contntx = beta_oop_perc_che_dme_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dme_contntx = beta_med_income_dme_contntx * sigma_med_income + beta_med_income_overall;

        mult_drtb_dnme_contntx = beta_drtb_dnme_contntx * sigma_drtb + beta_drtb_overall;
        mult_year_dnme_contntx = beta_year_dnme_contntx * sigma_year + beta_year_overall;
        mult_uhc_dnme_contntx  = beta_uhc_dnme_contntx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_dnme_contntx = beta_oop_perc_che_dnme_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_dnme_contntx = beta_med_income_dnme_contntx * sigma_med_income + beta_med_income_overall;

        mult_drtb_ie_contntx = beta_drtb_ie_contntx * sigma_drtb + beta_drtb_overall;
        mult_year_ie_contntx = beta_year_ie_contntx * sigma_year + beta_year_overall;
        mult_uhc_ie_contntx  = beta_uhc_ie_contntx * sigma_uhc + beta_uhc_overall;
        mult_oop_perc_che_ie_contntx = beta_oop_perc_che_ie_contntx * sigma_oop_perc_che + beta_oop_perc_che_overall;
        mult_med_income_ie_contntx = beta_med_income_ie_contntx * sigma_med_income + beta_med_income_overall;
    
    mult_natrep = randeff_natrep * sigma_randeff_natrep; 
    
    for (n in 1:N_country_new){
        if (review_ind[n] == 1){
            randeff_country_pred[n] = randeff_country[in_review_id[n]] * sigma_randeff_country; 
        } else{
            randeff_country_pred[n] = normal_rng(0, 1) * sigma_randeff_country; 
        }
        
       // Direct medical costs in pre-treatment period
        drtb_dme_prediag_cost_country[n] =
            exp(beta_dme_prediag + randeff_country_pred[n] +
                mult_med_income_dme_prediag * median_income_new[n] +
                mult_oop_perc_che_dme_prediag * oop_perc_che_new[n] +
                mult_uhc_dme_prediag * uhc_index_new[n]) *
            exp(mult_drtb_dme_prediag) * exp(mult_year_dme_prediag * pred_year) * 
            exp(mult_natrep);

        dstb_dme_prediag_cost_country[n] =
            exp(beta_dme_prediag + randeff_country_pred[n] +
                mult_med_income_dme_prediag * median_income_new[n] +
                mult_oop_perc_che_dme_prediag * oop_perc_che_new[n] +
                mult_uhc_dme_prediag * uhc_index_new[n]) *
            exp(mult_year_dme_prediag * pred_year)  *
            exp(mult_natrep);

        drtb_dnme_prediag_cost_country[n] =
            exp(beta_dnme_prediag + randeff_country_pred[n] +
                mult_med_income_dnme_prediag * median_income_new[n] +
                mult_oop_perc_che_dnme_prediag * oop_perc_che_new[n] +
                mult_uhc_dnme_prediag * uhc_index_new[n]) *
            exp(mult_drtb_dnme_prediag) *exp(mult_year_dnme_prediag  * pred_year) * 
            exp(mult_natrep);

        dstb_dnme_prediag_cost_country[n] =
            exp(beta_dnme_prediag + randeff_country_pred[n] +
                mult_med_income_dnme_prediag * median_income_new[n] +
                mult_oop_perc_che_dnme_prediag * oop_perc_che_new[n] +
                mult_uhc_dnme_prediag * uhc_index_new[n]) *
            exp(mult_year_dnme_prediag  * pred_year) * 
            exp(mult_natrep);

        drtb_ie_prediag_cost_country[n] =
            exp(beta_ie_prediag + randeff_country_pred[n] +
                mult_med_income_ie_prediag * median_income_new[n] +
                mult_oop_perc_che_ie_prediag * oop_perc_che_new[n] +
                mult_uhc_ie_prediag * uhc_index_new[n]) *
            exp(mult_drtb_ie_prediag) *exp(mult_year_ie_prediag  * pred_year) *
            exp(mult_natrep);

        dstb_ie_prediag_cost_country[n] =
            exp(beta_ie_prediag + randeff_country_pred[n] +
                mult_med_income_ie_prediag * median_income_new[n] +
                mult_oop_perc_che_ie_prediag * oop_perc_che_new[n] +
                mult_uhc_ie_prediag * uhc_index_new[n]) *
            exp(mult_year_ie_prediag  * pred_year) * 
            exp(mult_natrep);



        drtb_dme_intsvtx_cost_country[n] =
            exp(beta_dme_intsvtx + randeff_country_pred[n] +
                mult_med_income_dme_intsvtx * median_income_new[n] +
                mult_oop_perc_che_dme_intsvtx * oop_perc_che_new[n] +
                mult_uhc_dme_intsvtx * uhc_index_new[n]) *
            exp(mult_drtb_dme_intsvtx) *exp(mult_year_dme_intsvtx  * pred_year) * 
            exp(mult_natrep);

        dstb_dme_intsvtx_cost_country[n] =
            exp(beta_dme_intsvtx + randeff_country_pred[n] +
                mult_med_income_dme_intsvtx * median_income_new[n] +
                mult_oop_perc_che_dme_intsvtx * oop_perc_che_new[n] +
                mult_uhc_dme_intsvtx * uhc_index_new[n]) *
            exp(mult_year_dme_intsvtx  * pred_year) * 
            exp(mult_natrep);

        drtb_dnme_intsvtx_cost_country[n] =
            exp(beta_dnme_intsvtx + randeff_country_pred[n] +
                mult_med_income_dnme_intsvtx * median_income_new[n] +
                mult_oop_perc_che_dnme_intsvtx * oop_perc_che_new[n] +
                mult_uhc_dnme_intsvtx * uhc_index_new[n]) *
            exp(mult_drtb_dnme_intsvtx) *exp(mult_year_dnme_intsvtx  * pred_year) * 
            exp(mult_natrep);

        dstb_dnme_intsvtx_cost_country[n] =
            exp(beta_dnme_intsvtx + randeff_country_pred[n] +
                mult_med_income_dnme_intsvtx * median_income_new[n] +
                mult_oop_perc_che_dnme_intsvtx * oop_perc_che_new[n] +
                mult_uhc_dnme_intsvtx * uhc_index_new[n]) *
            exp(mult_year_dnme_intsvtx  * pred_year) * 
            exp(mult_natrep);

        drtb_ie_intsvtx_cost_country[n] =
            exp(beta_ie_intsvtx + randeff_country_pred[n] +
                mult_med_income_ie_intsvtx * median_income_new[n] +
                mult_oop_perc_che_ie_intsvtx * oop_perc_che_new[n] +
                mult_uhc_ie_intsvtx * uhc_index_new[n]) *
            exp(mult_drtb_ie_intsvtx) *exp(mult_year_ie_intsvtx  * pred_year) * 
            exp(mult_natrep);

        dstb_ie_intsvtx_cost_country[n] =
            exp(beta_ie_intsvtx + randeff_country_pred[n] +
                mult_med_income_ie_intsvtx * median_income_new[n] +
                mult_oop_perc_che_ie_intsvtx * oop_perc_che_new[n] +
                mult_uhc_ie_intsvtx * uhc_index_new[n]) *
            exp(mult_year_ie_intsvtx  * pred_year) * 
            exp(mult_natrep);

       // Direct medical costs in continuation treatment period

        drtb_dme_contntx_cost_country[n] =
            exp(beta_dme_contntx + randeff_country_pred[n] +
                mult_med_income_dme_contntx * median_income_new[n] +
                mult_oop_perc_che_dme_contntx * oop_perc_che_new[n] +
                mult_uhc_dme_contntx * uhc_index_new[n]) *
            exp(mult_drtb_dme_contntx) *exp(mult_year_dme_contntx  * pred_year) * 
            exp(mult_natrep);

        dstb_dme_contntx_cost_country[n] =
            exp(beta_dme_contntx + randeff_country_pred[n] +
                mult_med_income_dme_contntx * median_income_new[n] +
                mult_oop_perc_che_dme_contntx * oop_perc_che_new[n] +
                mult_uhc_dme_contntx * uhc_index_new[n]) *
            exp(mult_year_dme_contntx  * pred_year) *
            exp(mult_natrep);

        drtb_dnme_contntx_cost_country[n] =
            exp(beta_dnme_contntx + randeff_country_pred[n] +
                mult_med_income_dnme_contntx * median_income_new[n] +
                mult_oop_perc_che_dnme_contntx * oop_perc_che_new[n] +
                mult_uhc_dnme_contntx * uhc_index_new[n]) *
            exp(mult_drtb_dnme_contntx) *exp(mult_year_dnme_contntx  * pred_year) * 
            exp(mult_natrep);

        dstb_dnme_contntx_cost_country[n] =
            exp(beta_dnme_contntx + randeff_country_pred[n] +
                mult_med_income_dnme_contntx * median_income_new[n] +
                mult_oop_perc_che_dnme_contntx * oop_perc_che_new[n] +
                mult_uhc_dnme_contntx * uhc_index_new[n]) *
            exp(mult_year_dnme_contntx  * pred_year) *
            exp(mult_natrep);

        drtb_ie_contntx_cost_country[n] =
            exp(beta_ie_contntx + randeff_country_pred[n] +
                mult_med_income_ie_contntx   * median_income_new[n] +
                mult_oop_perc_che_ie_contntx * oop_perc_che_new[n] +
                mult_uhc_ie_contntx          * uhc_index_new[n]) *
            exp(mult_drtb_ie_contntx) * exp(mult_year_ie_contntx * pred_year) *
            exp(mult_natrep);

        dstb_ie_contntx_cost_country[n] =
            exp(beta_ie_contntx + randeff_country_pred[n] +
                mult_med_income_ie_contntx   * median_income_new[n] +
                mult_oop_perc_che_ie_contntx * oop_perc_che_new[n] +
                mult_uhc_ie_contntx          * uhc_index_new[n]) *
            exp(mult_year_ie_contntx         * pred_year)  *
            exp(mult_natrep); // we want to have the nationally representative so we apply this factor

        // Total costs
        //// Drug-resistant tuberculosis
        drtb_total_cost_country[n] = drtb_dme_prediag_cost_country[n] +
                                     drtb_dnme_prediag_cost_country[n] +
                                     drtb_ie_prediag_cost_country[n] +
                                     drtb_dme_intsvtx_cost_country[n] +
                                     drtb_dnme_intsvtx_cost_country[n] +
                                     drtb_ie_intsvtx_cost_country[n]+
                                     drtb_dme_contntx_cost_country[n] +
                                     drtb_dnme_contntx_cost_country[n] +
                                     drtb_ie_contntx_cost_country[n];

        drtb_dme_cost_country[n] = drtb_dme_prediag_cost_country[n] +
                                   drtb_dme_intsvtx_cost_country[n] +
                                   drtb_dme_contntx_cost_country[n];

        drtb_dnme_cost_country[n] = drtb_dnme_prediag_cost_country[n] +
                                    drtb_dnme_intsvtx_cost_country[n] +
                                    drtb_dnme_contntx_cost_country[n];

        drtb_ie_cost_country[n] = drtb_ie_prediag_cost_country[n] +
                                  drtb_ie_intsvtx_cost_country[n] +
                                  drtb_ie_contntx_cost_country[n];

        //// Drug-sensitive tuberculosis
        dstb_total_cost_country[n] = dstb_dme_prediag_cost_country[n] +
                                     dstb_dnme_prediag_cost_country[n] +
                                     dstb_ie_prediag_cost_country[n] +
                                     dstb_dme_intsvtx_cost_country[n] +
                                     dstb_dnme_intsvtx_cost_country[n] +
                                     dstb_ie_intsvtx_cost_country[n]+
                                     dstb_dme_contntx_cost_country[n] +
                                     dstb_dnme_contntx_cost_country[n] +
                                     dstb_ie_contntx_cost_country[n];

        dstb_dme_cost_country[n] = dstb_dme_prediag_cost_country[n] +
                                   dstb_dme_intsvtx_cost_country[n] +
                                   dstb_dme_contntx_cost_country[n];

        dstb_dnme_cost_country[n] = dstb_dnme_prediag_cost_country[n] +
                                    dstb_dnme_intsvtx_cost_country[n] +
                                    dstb_dnme_contntx_cost_country[n];

        dstb_ie_cost_country[n] = dstb_ie_prediag_cost_country[n] +
                                  dstb_ie_intsvtx_cost_country[n] +
                                  dstb_ie_contntx_cost_country[n];
                                  
                                  
       // without the national rep random effect
        dstb_total_cost_country_noNatRep[n] = (dstb_dme_prediag_cost_country[n] +
                                     dstb_dnme_prediag_cost_country[n] +
                                     dstb_ie_prediag_cost_country[n] +
                                     dstb_dme_intsvtx_cost_country[n] +
                                     dstb_dnme_intsvtx_cost_country[n] +
                                     dstb_ie_intsvtx_cost_country[n] +
                                     dstb_dme_contntx_cost_country[n] +
                                     dstb_dnme_contntx_cost_country[n] +
                                     dstb_ie_contntx_cost_country[n]) / exp(mult_natrep);
                                     
          drtb_total_cost_country_noNatRep[n] = (drtb_dme_prediag_cost_country[n] +
                                     drtb_dnme_prediag_cost_country[n] +
                                     drtb_ie_prediag_cost_country[n] +
                                     drtb_dme_intsvtx_cost_country[n] +
                                     drtb_dnme_intsvtx_cost_country[n] +
                                     drtb_ie_intsvtx_cost_country[n] +
                                     drtb_dme_contntx_cost_country[n] +
                                     drtb_dnme_contntx_cost_country[n] +
                                     drtb_ie_contntx_cost_country[n]) / exp(mult_natrep);
    }
}
