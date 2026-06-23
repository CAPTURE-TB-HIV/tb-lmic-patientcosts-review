library(ggpattern)
library(sf)
library(rnaturalearth)
library(magick)
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(rstan)
library(tidybayes)

##############################################################################|
##### Set seed, number of iterations (warmup and total)                   #####
##### for sampling in model fits.                                         ####|
##### Needs to be the same in the runPoolingModel.R script.               ####|
##############################################################################|

tbPtntCstSeed <- 513
tbPtntCstIter <- 4000
tbPtntCstWarmup <- 500

###############################################################################
##### Read in model and other data setup 
###############################################################################
### Best model as defined by the model selection tests
predModel <- stan_model("code/inst/tbPatientCosts-predictive.stan")

### Data used to fit the model 
source(here("code/modelDataFormat.R"))

### All LMICs in 2025
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
        rename(study.country = country.name) %>%
        select(! class25)

### External covariate datasets 
medianIncome0 <- readRDS(here("data/covariateData/medianIncomePrediction.rds")) %>% 
                 select(Country.Code, study.end.year, median.income.calc.usd) %>% 
                 rename(country.code = Country.Code, 
                        median.income = median.income.calc.usd) %>% 
                 filter(study.end.year > 2008)

##### Add in 2024 values as 2023 values due to missingness 
medianIncome <- rbind(medianIncome0,
                      medianIncome0[which(medianIncome0$study.end.year == 2023),] %>% 
                      mutate(study.end.year = 2024)) %>% 
                arrange(country.code, study.end.year) 

range(medianIncome$study.end.year)
dim(medianIncome)

oopSpending <- readRDS(here("data/covariateData/healthAccounts.rds")) %>% 
               select(country.code, study.end.year, 
                      oop.perc.che.interpolated) %>% 
               mutate(oop.perc.che = as.numeric(oop.perc.che.interpolated),
                      study.end.year = as.numeric(study.end.year)) %>% 
               #### Remove the three countries with no estimates 
               filter(!country.code %in% c("PRK", "PSE", "XKX"), 
                      study.end.year > 2008)

range(oopSpending$study.end.year)


uhcIndex0 <- readRDS(here("data/covariateData/uhcIndex.rds")) %>% 
             rename(uhc.index = UHCindex) %>%
             mutate(study.end.year = as.numeric(study.end.year))

##### Add in 2024 values as 2023 values due to missingness 
uhcIndex <- rbind(uhcIndex0,
                      uhcIndex0[which(uhcIndex0$study.end.year == 2023),] %>% 
                      mutate(study.end.year = 2024)) %>% 
                arrange(country.code, study.end.year)  %>% 
            filter(study.end.year > 2008)

range(uhcIndex$study.end.year)

###############################################################################
##### Setup covariates for the prediction 
###############################################################################
covariatesTime00 <- left_join(oopSpending, 
                              medianIncome, 
                             by = c("country.code", "study.end.year"))

covariatesTime0  <- left_join(covariatesTime00, 
                             uhcIndex  %>%
                                 filter(study.end.year >  2008,
                                        country.code %in% LMIC$country.code), 
                             by = c("country.code", "study.end.year")) %>% 
                    select(country.code, study.country, study.end.year,
                           median.income, oop.perc.che, uhc.index)

### Mean center study year 
covariatesTime <- covariatesTime0 %>% 
   group_by(study.country) %>% 
   na.omit() %>%
   mutate(country.id.new = cur_group_id(), 
          in.review = ifelse(country.code %in% modelData$country.code, 
                             1, 0)) %>% 
   left_join(modelData %>% ungroup() %>% 
                 select(country.code, country.id) %>% unique()) %>% 
   mutate(in.review.id = ifelse(is.na(country.id), 99, country.id), 
          oop.perc.che = as.numeric(oop.perc.che)) %>% 
   select(!country.id) 

# View(covariatesTime)

covariatesTime <- covariatesTime %>% 
   mutate(log.median.income = log(median.income),
          
          median.income.trunc = case_when(median.income < min(modelData$median.income) ~ min(modelData$median.income), 
                                          median.income > max(modelData$median.income) ~ max(modelData$median.income),
                                          .default = median.income), 
          
          log.median.income.trunc = case_when(log.median.income < min(modelData$log.median.income) ~ min(modelData$log.median.income), 
                                              log.median.income > max(modelData$log.median.income) ~ max(modelData$log.median.income),
                                              .default = log.median.income), 
          oop.perc.che.trunc = case_when(oop.perc.che < min(modelData$oop.perc.che) ~ min(modelData$oop.perc.che), 
                                         oop.perc.che > max(modelData$oop.perc.che) ~ max(modelData$oop.perc.che),
                                         .default = oop.perc.che), 
          uhc.index.trunc = case_when(uhc.index < min(modelData$UHCindex) ~ min(modelData$UHCindex), 
                                      uhc.index > max(modelData$UHCindex) ~ max(modelData$UHCindex),
                                      .default = uhc.index))

covariatesTime <- arrange(covariatesTime, country.id.new)

# saveRDS(covariatesTime, file = here("data/covariatesTime.rds"))

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|
lmicSampsTime <- list()
for (t in sort(unique(covariatesTime$study.end.year))) {
   
   print(t)
   
   covariateFilt <- covariatesTime %>% filter(study.end.year==t)
   
   predDataFilt <- list(
       N = length(modelData$cost),
       cost = modelData$cost,
       # se = rep(0, length(modelData$cost)),
       se = as.numeric(modelData$se)+ 0.01,
       dme_prediag = modelData$dme.prediag, 
       dnme_prediag =  modelData$dnme.prediag, 
       ie_prediag =  modelData$ie.prediag, 
       dme_intsvtx = modelData$dme.intsvtx, 
       dnme_intsvtx =  modelData$dnme.intsvtx, 
       ie_intsvtx =  modelData$ie.intsvtx,
       dme_contntx = modelData$dme.contntx, 
       dnme_contntx =  modelData$dnme.contntx, 
       ie_contntx =  modelData$ie.contntx, 
       N_survey = max(modelData$survey.id),
       survey_id = modelData$survey.id, 
       N_country = max(modelData$country.id),
       country_id = modelData$country.id, 
       ### Internal covariate
       drtb_ind= modelData$drtb.wt,
       survey_year = ((modelData$study.end.year - mean(modelData$study.end.year)) / sd(modelData$study.end.year)),
       ### External covariateFilt
       median_income = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income)),
       oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
       uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
       ### Prediction data 
       N_country_new = max(covariateFilt$country.id.new),
       review_ind = covariateFilt$in.review, 
       in_review_id = covariateFilt$in.review.id, # note that this is different from modelData country.id
       pred_year = (t - mean(modelData$study.end.year)) / sd(modelData$study.end.year), 
       median_income_new = (covariateFilt$log.median.income.trunc - mean(modelData$log.median.income)) / sd(modelData$log.median.income), 
       oop_perc_che_new = (covariateFilt$oop.perc.che.trunc - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che), 
       uhc_index_new = (covariateFilt$uhc.index.trunc - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
       prior_sigma = 2, 
       prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
   )
   
   lmicSampsTime[[as.character(t)]] <- rstan::sampling(
       predModel, predDataFilt,
       chains = 2, cores = 4,
       warmup = tbPtntCstWarmup,
       iter = tbPtntCstIter/2,
       control=list(max_treedepth=14),
       refresh = 0, seed = tbPtntCstSeed
   )
   
}

saveRDS(lmicSampsTime, here("output/allYears_06-09.rds"), version=2)

