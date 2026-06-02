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
### Missingness of 2024 is too high so we use 2023

medianIncome <- readRDS(here("data/covariateData/medianIncomePrediction.rds")) %>% 
    select(Country.Code, study.end.year, median.income.calc.usd) %>% 
    rename(country.code = Country.Code, 
           median.income = median.income.calc.usd) 

oopSpending <- readRDS(here("data/covariateData/healthAccounts.rds")) %>% 
               select(country.code, study.end.year, 
                      oop.perc.che.interpolated) %>% 
               mutate(oop.perc.che = as.numeric(oop.perc.che.interpolated),
                      study.end.year = as.numeric(study.end.year)) 

uhcIndex <- readRDS(here("data/covariateData/uhcIndex.rds")) %>% 
            rename(uhc.index = UHCindex) %>%
            mutate(study.end.year = as.numeric(study.end.year))

###############################################################################
##### Setup covariates for the prediction 
###############################################################################

covariates00 <- left_join(medianIncome %>% 
                          filter(study.end.year == 2023, 
                                  country.code %in% LMIC$country.code) , 
                          oopSpending  %>%
                          filter(study.end.year == 2023), 
                          by = c("country.code", "study.end.year"))

covariates0  <- left_join(covariates00, 
                          uhcIndex  %>%
                          filter(study.end.year == 2023, 
                                 country.code %in% LMIC$country.code), 
                          by = c("country.code", "study.end.year"))

### DRTB was internal to the review dataset and was a binary indicator
### We will model both so each entry above will get a 0 and 1 for drtb.ind
# drtb.ind <- 0:1
# covariates <- crossing(covariates, drtb.ind)

### Mean center study year 
covariates <- covariates0 %>% 
    # filter(median.income >= min(modelData$median.income) & 
    #            median.income <= max(modelData$median.income), 
    #        uhc.index >= min(modelData$UHCindex) & 
    #            uhc.index <= max(modelData$UHCindex),
    #        oop.perc.che >= min(modelData$oop.perc.che) & 
    #            oop.perc.che <= max(modelData$oop.perc.che)) %>% 
              mutate(pred.year = study.end.year - floor(mean(modelData$study.end.year, na.rm=TRUE))) %>% 
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

### Save the countries for which we don't have covariates
countriesNoCovs <- covariates0 %>% filter(! country.code %in% covariates$country.code) 

# View(covariates0 %>% filter(is.na(median.income) | is.na(uhc.index) | is.na(oop.perc.che)))

### Those with OOP missing -- check for alternative years 
oopSpending %>% filter(country.code %in% c("PRK", "PSE", "UKR", "XKX"))

### Ukraine use 2021 as last reported year 
covariates0[which(covariates0$country.code == "UKR"), "oop.perc.che"] <- unlist(oopSpending[which(oopSpending$study.end.year == 2021 &
                                                                                           oopSpending$country.code == "UKR"), "oop.perc.che"])
# covariatesFilt <- covariates %>%
#                   filter(median.income >= min(modelData$median.income) &
#                          median.income <= max(modelData$median.income),
#                          uhc.index >= min(modelData$UHCindex) &
#                          uhc.index <= max(modelData$UHCindex),
#                          oop.perc.che >= min(modelData$oop.perc.che) &
#                          oop.perc.che <= max(modelData$oop.perc.che))
# 
# countriesCovOutRange <- covariates %>% filter(! country.code %in% covariatesFilt$country.code) 

covariates <- covariates %>% 
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
                                                .default = uhc.index)
                   )
# format covariates for supplement table 

covariatesFormatted <- covariates %>% 
                       mutate("included" = ifelse(in.review == 1, 
                                                   "Yes", "No"), 
                               "median.income" = round(median.income),
                               "median.income.trunc" = round(median.income.trunc)) %>% 
                       select(study.country, included, 
                              median.income, median.income.trunc, 
                              oop.perc.che, oop.perc.che.trunc, 
                              uhc.index, uhc.index.trunc)

# write.csv(covariatesFormatted, here("output/supplement/countryPredictiveCovariates.csv"))

covariates <- arrange(covariates, country.id.new)
### save non formatted covariates for matching in exhibits
# saveRDS(covariates, file = here("output/covariates.rds"))
# covariates <- covariatesFilt
### Stan requires a list for data. 
predictionData <- list(
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
            ### External covariates
            median_income = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income)),
            oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
            uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
            ### Prediction data 
            N_country_new = max(covariates$country.id.new),
            review_ind = covariates$in.review, 
            in_review_id = covariates$in.review.id, # note that this is different from modelData country.id
            pred_year = unique(covariates$pred.year),
            median_income_new = (covariates$log.median.income.trunc- mean(modelData$log.median.income)) / sd(modelData$log.median.income), 
            oop_perc_che_new = (covariates$oop.perc.che.trunc - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che), 
            uhc_index_new = (covariates$uhc.index.trunc - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
            prior_sigma = 2, 
            prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|
# predModel <- stan_model("code/inst/tbPatientCosts-randEffCov.stan")

lmicSamps <- rstan::sampling(
                    predModel, predictionData,
                    chains = 4, cores = 4,
                    warmup = tbPtntCstWarmup,
                    iter = tbPtntCstIter,
                    control=list(max_treedepth=14),
                    refresh = 0, seed = tbPtntCstSeed
)

saveRDS(lmicSamps, here("output/goodModelRun-06-01.rds"))

##############################################################################|
##### Some model checks
##############################################################################|
range(summary(lmicSamps)[[1]][, "Rhat"])
View(summary(lmicSamps)[[1]])

### Are the random effects mean close to 0 
indTemp<- grep("randeff_country", rownames(summary(lmicSamps)[[1]]))
mean(summary(lmicSamps)[[1]][indTemp[1:34],"mean"])

indTemp<- grep("randeff_survey", rownames(summary(lmicSamps)[[1]]))
mean(summary(lmicSamps)[[1]][indTemp[-(length(indTemp))],"mean"])

##############################################################################|
### Join the predictions and the data 
##############################################################################|

allDraws <- spread_draws(lmicSamps, 
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
                drtb_dme_prediag_cost_country[country.id.new],
                drtb_dnme_prediag_cost_country[country.id.new],
                drtb_ie_prediag_cost_country[country.id.new],
                drtb_dme_intsvtx_cost_country[country.id.new],
                drtb_dnme_intsvtx_cost_country[country.id.new],
                drtb_ie_intsvtx_cost_country[country.id.new],
                drtb_dme_contntx_cost_country[country.id.new],
                drtb_dnme_contntx_cost_country[country.id.new],
                drtb_ie_contntx_cost_country[country.id.new],
                drtb_total_cost_country[country.id.new], 
                
                dstb_dme_cost_country[country.id.new], 
                dstb_dnme_cost_country[country.id.new], 
                dstb_ie_cost_country[country.id.new], 
                
                drtb_dme_cost_country[country.id.new], 
                drtb_dnme_cost_country[country.id.new], 
                drtb_ie_cost_country[country.id.new], 
                ndraws = 1000, seed = tbPtntCstSeed) %>% 
        ungroup() %>% 
       mutate(dstb_dme_prediag_perc = dstb_dme_prediag_cost_country / dstb_total_cost_country *100, 
              dstb_dnme_prediag_perc = dstb_dnme_prediag_cost_country / dstb_total_cost_country *100,
              dstb_ie_prediag_perc = dstb_ie_prediag_cost_country / dstb_total_cost_country *100,
              dstb_dme_intsvtx_perc = dstb_dme_intsvtx_cost_country / dstb_total_cost_country *100, 
              dstb_dnme_intsvtx_perc = dstb_dnme_intsvtx_cost_country / dstb_total_cost_country *100,
              dstb_ie_intsvtx_perc = dstb_ie_intsvtx_cost_country / dstb_total_cost_country *100, 
              dstb_dme_contntx_perc = dstb_dme_contntx_cost_country / dstb_total_cost_country *100, 
              dstb_dnme_contntx_perc = dstb_dnme_contntx_cost_country / dstb_total_cost_country *100,
              dstb_ie_contntx_perc = dstb_ie_contntx_cost_country / dstb_total_cost_country *100, 
              drtb_dme_prediag_perc = drtb_dme_prediag_cost_country / drtb_total_cost_country *100, 
              drtb_dnme_prediag_perc = drtb_dnme_prediag_cost_country / drtb_total_cost_country *100,
              drtb_ie_prediag_perc = drtb_ie_prediag_cost_country / drtb_total_cost_country *100,
              drtb_dme_intsvtx_perc = drtb_dme_intsvtx_cost_country / drtb_total_cost_country *100, 
              drtb_dnme_intsvtx_perc = drtb_dnme_intsvtx_cost_country / drtb_total_cost_country *100,
              drtb_ie_intsvtx_perc = drtb_ie_intsvtx_cost_country / drtb_total_cost_country *100, 
              drtb_dme_contntx_perc = drtb_dme_contntx_cost_country / drtb_total_cost_country *100, 
              drtb_dnme_contntx_perc = drtb_dnme_contntx_cost_country / drtb_total_cost_country *100,
              drtb_ie_contntx_perc = drtb_ie_contntx_cost_country / drtb_total_cost_country *100, 
              
              ### summarize by cost type 
              
              dstb_dme_perc = (dstb_dme_prediag_cost_country+ dstb_dme_intsvtx_cost_country+ 
                                  dstb_dme_contntx_cost_country) / dstb_total_cost_country * 100,
              
              dstb_dme_perc2 = dstb_dme_cost_country / dstb_total_cost_country * 100,
              
              dstb_dnme_perc = (dstb_dnme_prediag_cost_country+ dstb_dnme_intsvtx_cost_country+ 
                                  dstb_dnme_contntx_cost_country) / dstb_total_cost_country * 100,
              
              dstb_dnme_perc2 = dstb_dnme_cost_country / dstb_total_cost_country * 100,
              
              dstb_ie_perc = (dstb_ie_prediag_cost_country+ dstb_ie_intsvtx_cost_country+ 
                                  dstb_ie_contntx_cost_country) / dstb_total_cost_country * 100,
              
              drtb_dme_perc = (drtb_dme_prediag_cost_country+ drtb_dme_intsvtx_cost_country+ 
                                  drtb_dme_contntx_cost_country) / drtb_total_cost_country * 100,
              drtb_dnme_perc = (drtb_dnme_prediag_cost_country+ drtb_dnme_intsvtx_cost_country+ 
                                   drtb_dnme_contntx_cost_country) / drtb_total_cost_country * 100,
              drtb_ie_perc = (drtb_ie_prediag_cost_country+ drtb_ie_intsvtx_cost_country+ 
                                  drtb_ie_contntx_cost_country) / drtb_total_cost_country * 100,
                              
              drtbRatioTotal = drtb_total_cost_country/dstb_total_cost_country)
   
   
   allDraws%>%
       median_qi(
                 dstb_dme_prediag_perc, dstb_dnme_prediag_perc, dstb_ie_prediag_perc, 
                 dstb_dme_intsvtx_perc, dstb_dnme_intsvtx_perc, dstb_ie_intsvtx_perc,
                 dstb_dme_contntx_perc, dstb_dnme_contntx_perc, dstb_ie_contntx_perc)
   allDraws%>%
        median_qi(
                  drtb_dme_prediag_perc, drtb_dnme_prediag_perc, drtb_ie_prediag_perc, 
                  drtb_dme_intsvtx_perc, drtb_dnme_intsvtx_perc, drtb_ie_intsvtx_perc,
                  drtb_dme_contntx_perc, drtb_dnme_contntx_perc, drtb_ie_contntx_perc)
   
   allDraws%>% median_qi(dstb_total_cost_country, drtb_total_cost_country, drtbRatioTotal)
   
   allDraws %>% 
       mean_qi(dstb_dme_perc, 
                 dstb_dnme_perc, 
                 dstb_ie_perc,
                 drtb_dme_perc, drtb_dnme_perc, drtb_ie_perc) %>% 
      select(!c(.width, .point, .interval)) %>% round() %>% matrix(ncol=3, byrow = TRUE)


### Format predictions 
##############################################################################|
##### Save predictions as a CSV and R dataset 
##############################################################################|

# saveRDS(lmicPred, here("output/costPredictions.rds"), version=2)
# write.csv(lmicPred, here("output/costPredictions.csv"))
   