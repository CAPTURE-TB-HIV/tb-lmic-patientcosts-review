##############################################################################|
#### Script to create the data sets used in the TB PaCC webtool #### 
##############################################################################|

##### Setup packages ##########################################################

library(tidybayes) # spreadvars function 
library(dplyr)

##### Read in datafiles #######################################################

covariatesTime0 <- readRDS(here("data/covariatesTime.rds")) 

##### WHO region because it was not in the original covariates file, but 
##### we need this to look up which region to show on comparison plot. 
regionWHO <- read.csv(here("data/who-regions.csv")) %>% 
    rename(study.country=Entity, 
           country.code = Code)  %>% 
    mutate(who.region = gsub(" \\(WHO\\)", replacement="",
                             World.regions.according.to.WHO)) %>% 
    select(! c(World.regions.according.to.WHO, study.country))

#### This is a list of each of the years' models 
lmicSampsAll <- readRDS(here("output/allYears_08-07-noTrunc.rds"))

##### 1. Data frame of all covariates used in the model runs ##################
##### Number of rows = number of countries * number of years (129*16)
##### Number of columns = number of covariates + ids 
##### id column names = study.end.year, study.country, who.region, country.code
##### Must have unscaled covariates; scaled covariates are nice also to confirm
##### that the scaling in calculations works correctly
covariatesTime <- left_join(covariatesTime0, 
                            regionWHO, 
                            by=("country.code")) %>% 
    mutate(who.region = ifelse(country.code == "VCT", 
                               "Americas", who.region))

covariates24 <- covariatesTime0 %>% 
                filter(study.end.year == 2024) %>% 
                left_join(regionWHO, by=("country.code")) %>% 
                mutate(who.region = ifelse(country.code == "VCT", 
                       "Americas", who.region))

saveRDS(covariatesTime, "~/Desktop/nis100/PaCC/utilities/data/covariatesTime.rds")


##### 2. Data frame of country random effects  ################################
##### Number of rows = number of countries
##### Columns = 10000 sample of random effect + country name (name = Country)
countryRE <- left_join(spread_draws(lmicSamps, 
                       randeff_country_predC[country.id.new],
                       ndraws = 10000, seed = tbPtntCstSeed) , 
                       covariatesTime %>% filter(study.end.year ==2024) %>% 
                           select(study.country, country.id.new)) %>% 
             mutate(draw = 1:10000) %>% 
             ungroup() %>% 
             select(draw, study.country, randeff_country_predC) %>%
             pivot_wider(id_cols = draw, 
                         names_from = study.country, 
                         values_from = randeff_country_predC) %>% 
    select(!draw) %>% t() %>% as.data.frame()
    
countryRE$Country <- rownames(countryRE)
saveRDS(countryRE, "~/Desktop/nis100/PaCC/utilities/data/cntryRndEff.rds")

##### 3. Data frame of parameters #############################################
##### Number of rows = the number of samples (N=10000)
##### Number of columns = number of parameters needed to calculate costs + ids
##### id colnames = study.country, country.code, and study.end.year 
##### parameter colnames listed in call to spread draws below 
params <- spread_draws(lmicSamps,
             beta_dme_prediag, 
             beta_dme_prediag, 
             beta_dnme_prediag, 
             beta_ie_prediag, 
             beta_dme_intsvtx, 
             beta_dnme_intsvtx, 
             beta_ie_intsvtx, 
             beta_dme_contntx, 
             beta_dnme_contntx, 
             beta_ie_contntx,

             mult_year_dme_prediag, 
             mult_year_dnme_prediag, 
             mult_year_ie_prediag, 
             mult_year_dme_intsvtx, 
             mult_year_dnme_intsvtx, 
             mult_year_ie_intsvtx,
             mult_year_dme_contntx, 
             mult_year_dnme_contntx, 
             mult_year_ie_contntx,

             mult_med_income_dme_prediag, 
             mult_med_income_dnme_prediag, 
             mult_med_income_ie_prediag, 
             mult_med_income_dme_intsvtx, 
             mult_med_income_dnme_intsvtx, 
             mult_med_income_ie_intsvtx,
             mult_med_income_dme_contntx, 
             mult_med_income_dnme_contntx, 
             mult_med_income_ie_contntx,

             mult_oop_perc_che_dme_prediag, 
             mult_oop_perc_che_dnme_prediag, 
             mult_oop_perc_che_ie_prediag, 
             mult_oop_perc_che_dme_intsvtx, 
             mult_oop_perc_che_dnme_intsvtx, 
             mult_oop_perc_che_ie_intsvtx,
             mult_oop_perc_che_dme_contntx, 
             mult_oop_perc_che_dnme_contntx, 
             mult_oop_perc_che_ie_contntx,

             mult_cdr_dme_prediag, 
             mult_cdr_dnme_prediag, 
             mult_cdr_ie_prediag, 
             mult_cdr_dme_intsvtx, 
             mult_cdr_dnme_intsvtx, 
             mult_cdr_ie_intsvtx,
             mult_cdr_dme_contntx, 
             mult_cdr_dnme_contntx, 
             mult_cdr_ie_contntx,

             ndraws = 10000, seed = tbPtntCstSeed) %>% 
             select(!c(.chain, .iteration, .draw))

saveRDS(params, "~/Desktop/nis100/PaCC/utilities/data/parameters.rds")


##### 4. Data frame of mean and sd for scaling  #########################
##### Number of rows = 2; number of columns = number of scaled covariates 
##### colnames = log.median.income.hh; oop.perc.che; cdr.rate; study.end.year 
source(here("code/modelDataFormat.R"))

#### TB case detection rate data
cdrData <- read.csv("~/Downloads/TB_burden_countries_2026-07-23.csv") %>% 
    select(country, iso3, g_whoregion, year, 
           c_cdr) %>% 
    rename(country.code = iso3,
           study.end.year = year)


### Add to model data 
modelData <- left_join(modelData, cdrData) %>% 
    rename(cdr.rate = c_cdr)

modelDataFilt<- modelData %>% select(log.median.income.hh, oop.perc.che, cdr.rate, study.end.year)

modelDataScaling <- data.frame(
                               mean=apply(modelDataFilt, 2, mean), 
                               sd = apply(modelDataFilt, 2, sd)) %>% t() %>% 
                    as.data.frame()

saveRDS(modelDataScaling, "~/Desktop/nis100/PaCC/utilities/data/scalars.rds")

##### 5. Data frame of regional and total mean estimates  #####################
##### Number of rows = number of regions + 1 = 7 
##### Number of columns = 5: "geography","est", "lower", "upper", "formatted"
##### All LMICs and regional estimates 

regionalEst <- read.csv(here("output/tables/regionMeanCosts.csv")) %>% 
               filter(Category == "wgtTotal") %>% 
               rename("geography" = who.region) %>% 
               select(! c(X, Category))

allLMICEst <- read.csv(here("output/tables/allLMICMeanCosts.csv")) %>% 
    filter(Category == "wgtTotal") %>% 
    mutate(geography = "LMICs") %>% 
    select(geography, est, lower, upper, formatted)

allCompEsts <- rbind(regionalEst, allLMICEst)

saveRDS(allCompEsts, "~/Desktop/nis100/PaCC/utilities/data/compData.rds")


##### Test a calculation #######################################################
testCountry <- "China"
testYear <- 2024
covs <- covariates %>% 
    filter(study.country == testCountry,
           study.end.year == 2024) %>% 
    mutate(scaled.med.inc = (log.median.income.hh.trunc - modelDataScaling["mean", "log.median.income.hh"]) / modelDataScaling["sd", "log.median.income.hh"], 
           scaled.oop.che = (oop.perc.che.trunc - modelDataScaling["mean", "oop.perc.che"]) / modelDataScaling["sd", "oop.perc.che"], 
           scaled.cdr =     (cdr.trunc - modelDataScaling["mean", "cdr.rate"]) / modelDataScaling["sd", "cdr.rate"])

# covs$scaled.med.inc
cntryRE2 <- countryRE %>% filter(Country == testCountry) %>% select(!Country) %>% as.numeric() #%>% t()
scaledYear <- (testYear - modelDataScaling["mean", "study.end.year"]) / modelDataScaling["sd", "study.end.year"]

estimate <- exp(params[,"beta_dme_prediag"] + cntryRE2 + 
                   params[,"mult_med_income_dme_prediag"] * covs$scaled.med.inc +
                   params[,"mult_oop_perc_che_dme_prediag"] * covs$scaled.oop.che +
                   params[,"mult_cdr_dme_prediag"] * covs$scaled.cdr) *
            exp(params[,"mult_year_dme_prediag"] * scaledYear)

mean(unlist(estimate))

left_join(covariates,   
          spread_draws(lmicSamps, 
                       dstb_dme_prediag_cost_country[country.id.new],
                       dstb_dnme_prediag_cost_country[country.id.new],
                       dstb_ie_prediag_cost_country[country.id.new],
                       dstb_dme_intsvtx_cost_country[country.id.new],
                       dstb_dnme_intsvtx_cost_country[country.id.new],
                       dstb_ie_intsvtx_cost_country[country.id.new],
                       dstb_dme_contntx_cost_country[country.id.new],
                       dstb_dnme_contntx_cost_country[country.id.new],
                       dstb_ie_contntx_cost_country[country.id.new],
                       dstb_total_cost_country[country.id.new],
                       ndraws = 10000, seed = tbPtntCstSeed) %>% 
              select(!c(.chain, .iteration, .draw)), 
          join_by(country.id.new))  %>% 
    filter(study.country == "China") %>% 
    mean_qi(dstb_dme_prediag_cost_country, 
            dstb_dnme_prediag_cost_country, 
            dstb_ie_prediag_cost_country,
            dstb_dme_intsvtx_cost_country, 
            dstb_dnme_intsvtx_cost_country, 
            dstb_ie_intsvtx_cost_country,
            dstb_dme_contntx_cost_country, 
            dstb_dnme_contntx_cost_country, 
            dstb_ie_contntx_cost_country,
            dstb_total_cost_country) %>% 
    t()


