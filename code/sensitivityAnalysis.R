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

### Data used to fit the model 
source(here("code/modelDataFormat.R"))

### All LMICs in 2025
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
    rename(study.country = country.name) %>%
    select(! class25)

### External covariate datasets 
medianIncome0 <- readRDS(here("data/covariateData/medianIncomePrediction.rds")) %>% 
    select(country.code, study.end.year, 
           median.income.calc.intd.2025, 
           median.income.calc.intd.2025.hh) %>% 
    rename(
        # country.code = Country.Code, 
        median.income = median.income.calc.intd.2025, 
        median.income.hh = median.income.calc.intd.2025.hh) %>% 
    filter(study.end.year > 2008, 
           study.end.year < 2025) %>% 
    arrange(country.code, study.end.year) 


##### Add in 2024 values as 2023 values due to missingness 

missingIndx <- which(is.na(unlist(medianIncome0 %>% filter(study.end.year == 2024) %>% select(median.income))))

medianIncome0[which(medianIncome0$study.end.year == 2024)[missingIndx],"median.income"] <- 
    medianIncome0[which(medianIncome0$study.end.year == 2023)[missingIndx],"median.income"]

medianIncome0[which(medianIncome0$study.end.year == 2024)[missingIndx],"median.income.hh"] <- 
    medianIncome0[which(medianIncome0$study.end.year == 2023)[missingIndx],"median.income.hh"]

medianIncome <- medianIncome0
# View(medianIncome %>% filter(study.end.year == 2024))
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

#### TB case detection rate data
cdrData <- read.csv("~/Downloads/TB_burden_countries_2026-07-23.csv") %>% 
    select(country, iso3, g_whoregion, year, 
           c_cdr) %>% 
    rename(country.code = iso3,
           study.end.year = year)


### Add to model data 
modelData <- left_join(modelData, cdrData) %>% 
    rename(cdr.rate = c_cdr)

###############################################################################
##### Setup covariates for the prediction 
###############################################################################
covariatesTime000 <- left_join(oopSpending, 
                               medianIncome, 
                               by = c("country.code", "study.end.year"))

covariatesTime00 <- left_join(covariatesTime000, cdrData) %>% 
    # filter(!is.na(c_cdr))  %>% 
    rename(cdr.rate = c_cdr) %>%     
    mutate(country.id.new = cur_group_id(), 
           in.review = ifelse(country.code %in% modelData$country.code,1, 0)) %>% 
    left_join(modelData %>% ungroup() %>% 
                  select(country.code, country.id) %>% unique()) %>% 
    mutate(in.review.id = ifelse(is.na(country.id), 99, country.id))

covariatesTime0  <- left_join(covariatesTime00, 
                              uhcIndex, 
                              by = c("country.code", "study.end.year")) %>% 
    select(country.code, study.country, study.end.year,
           median.income, median.income.hh, oop.perc.che,
           uhc.index, cdr.rate)

### Add interpolation of cdr 
for (country in unique(covariatesTime0$study.country)){
    print(country)
    tmpDF <- covariatesTime0 %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$cdr.rate)) > 1){
        covariatesTime0[which(covariatesTime0$study.country == country), "cdr.rate.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$cdr.rate)],
                                                                                                            y=tmpDF$cdr.rate[!is.na(tmpDF$cdr.rate)], xout=tmpDF$study.end.year,
                                                                                                            rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$cdr.rate))== 1){
        covariatesTime0[which(covariatesTime0$study.country == country), "cdr.rate.interpolated"] <- na.omit(tmpDF$cdr.rate)[1]
    }
}

### Mean center study year 
covariatesTime <- covariatesTime0 %>% 
    # na.omit() %>% 
    # distinct() %>% 
    group_by(study.country) %>% 
    # filter(!is.na(hosp.rate.interpolated)) %>% 
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
           log.median.income.hh = log(median.income.hh),
           cdr.rate = cdr.rate.interpolated) %>% 
    select(!c(cdr.rate.interpolated)) %>% distinct()

covariatesTime <- arrange(covariatesTime, country.id.new) 

##############################################################################|
##### Sensitivity analysis #1 
##### Only the WHO national surveys (N=20)
##############################################################################|

#### Filter to the WHO national surveys for model data
modelDataSA1 <- modelData %>% 
                filter(covidence.id %in% 
                       ### These are the WHO patient costs surveys 
                       c(1:4, 8688, 7799, 8781, 7682, 9330,
                         8270, 9620, 7443,7355, 7919, 9422,
                         8310, 9585, 8634, 8677, 9301)) %>%
            ungroup() %>%
            group_by(covidence.id) %>%
            mutate(survey.id = cur_group_id()) %>% 
            ungroup() %>% 
            group_by(country.code) %>% 
            mutate(country.id = cur_group_id()) %>% ungroup()

#### Filter the covariates for 2024 values 
covariates24 <- covariatesTime %>%
                filter(study.end.year == 2024)

covariates24 <- covariates24 %>%
    group_by(study.country) %>%
    # na.omit() %>%
    mutate(country.id.new = cur_group_id(),
           in.review = ifelse(country.code %in% modelDataSA1$country.code,
                              1, 0)) %>%
    left_join(modelDataSA1 %>% ungroup() %>%
                  select(country.code, country.id) %>% unique()) %>%
    mutate(in.review.id = ifelse(is.na(country.id), 99, country.id),
           oop.perc.che = as.numeric(oop.perc.che)) %>%
    select(!country.id)

#### This is important for aligning the results to the covariates!! 
covariates24 <- arrange(covariates24, country.id.new)

### Stan requires a list for data. 
SA1Data <- list(
    N = length(modelDataSA1$cost),
    cost = modelDataSA1$cost,
    # se = rep(0, length(modelDataSA1$cost)),
    se = as.numeric(modelDataSA1$se)+ 0.01,
    dme_prediag = modelDataSA1$dme.prediag, 
    dnme_prediag =  modelDataSA1$dnme.prediag, 
    ie_prediag =  modelDataSA1$ie.prediag, 
    dme_intsvtx = modelDataSA1$dme.intsvtx, 
    dnme_intsvtx =  modelDataSA1$dnme.intsvtx, 
    ie_intsvtx =  modelDataSA1$ie.intsvtx,
    dme_contntx = modelDataSA1$dme.contntx, 
    dnme_contntx =  modelDataSA1$dnme.contntx, 
    ie_contntx =  modelDataSA1$ie.contntx, 
    N_survey = max(modelDataSA1$survey.id),
    survey_id = modelDataSA1$survey.id, 
    N_country = max(modelDataSA1$country.id),
    country_id = modelDataSA1$country.id, 
    id_matrix = modelDataSA1 %>% select(survey.id, country.id) %>% distinct(), 
    
    ### Internal covariate
    drtb_ind= modelDataSA1$drtb.wt,
    survey_year = ((modelDataSA1$study.end.year - mean(modelDataSA1$study.end.year)) / sd(modelDataSA1$study.end.year)),
    ### External covariateFilt24
    # median_income = ((modelDataSA1$log.median.income - mean(modelDataSA1$log.median.income)) / sd(modelDataSA1$log.median.income)),
    median_income = ((modelDataSA1$log.median.income.hh - mean(modelDataSA1$log.median.income.hh)) / sd(modelDataSA1$log.median.income.hh)),
    oop_perc_che =  (modelDataSA1$oop.perc.che - mean(modelDataSA1$oop.perc.che))/sd(modelDataSA1$oop.perc.che), 
    # uhc_index =     (modelDataSA1$UHCindex - mean(modelDataSA1$UHCindex)) / sd(modelDataSA1$UHCindex), 
    cdr_rate = (modelDataSA1$cdr.rate - mean(modelDataSA1$cdr.rate)) / sd(modelDataSA1$cdr.rate), 
    ### Prediction data 
    N_country_new = max(covariates24$country.id.new),
    review_ind = covariates24$in.review, 
    in_review_id = covariates24$in.review.id, # note that this is different from modelData country.id
    pred_year = (t - mean(modelData$study.end.year)) / sd(modelData$study.end.year), 
    
    median_income_new = (covariates24$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh),
    oop_perc_che_new = (covariates24$oop.perc.che - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che), 
    cdr_rate_new = (covariates24$cdr.rate - mean(modelData$cdr.rate)) / sd(modelData$cdr.rate),
    prior_sigma = 2, 
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|

lmicSampsSA1 <- stan(file = here("code/inst/model7.stan"),
                     data = SA1Data,
                     iter = tbPtntCstIter, chains = 3,
                     warmup = tbPtntCstWarmup,
                     control=list(max_treedepth=14), 
                     seed = tbPtntCstSeed)

saveRDS(lmicSampsSA1, here("output/sensAnalysis_1.rds"))


tidybayes::spread_draws(lmicSampsSA1, 
                        dstb_total_cost_country[country.id.new], 
                        ndraws = 10000, seed = tbPtntCstSeed) %>% 
    group_by(country.id.new) %>%
    # mean_qi(dstb_total_cost_country)  %>%
    mean_qi(dstb_total_cost_country) 



samplesSA1 <- spread_draws(lmicSampsSA1, 
                        ### non-RR/MDR-TB
                        dstb_dme_prediag_cost_country[country.id.new],
                        dstb_dnme_prediag_cost_country[country.id.new],
                        dstb_ie_prediag_cost_country[country.id.new],
                        dstb_dme_intsvtx_cost_country[country.id.new],
                        dstb_dnme_intsvtx_cost_country[country.id.new],
                        dstb_ie_intsvtx_cost_country[country.id.new],
                        dstb_dme_contntx_cost_country[country.id.new],
                        dstb_dnme_contntx_cost_country[country.id.new],
                        dstb_ie_contntx_cost_country[country.id.new],
                        dstb_dme_cost_country[country.id.new], 
                        dstb_dnme_cost_country[country.id.new], 
                        dstb_ie_cost_country[country.id.new], 
                        dstb_prediag_cost_country[country.id.new], 
                        dstb_intsvtx_cost_country[country.id.new], 
                        dstb_contntx_cost_country[country.id.new], 
                        dstb_total_cost_country[country.id.new], 
                        ndraws = 10000, seed = tbPtntCstSeed) 

##############################################################################|
##### Table SX                                      ###########################
##### Total Mean DSTB with uncertainty intervals  ###########################
##############################################################################|

summaryNotif <- notif %>%
    filter(study.end.year == 2024, 
           country.code %in% covariates$country.code) %>% 
    summarize(notifAll = round(sum(notif, na.rm=TRUE)/ 1e6, 2), 
              notifAllUR = sum(notif, na.rm=TRUE)); summaryNotif

summaryEstUcrntyTblSA10 <- samplesSA1 %>% 
    left_join(covariatesAll, by="country.id.new") %>% 
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    ungroup() %>% 
    # group_by(`.draw`, country.id.new) %>%
    summarise(wgtDME_PT   = weighted.mean(dstb_dme_prediag_cost_country, notif), 
              wgtDNME_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
              wgtIE_PT   = weighted.mean(dstb_ie_prediag_cost_country, notif), 
              wgtDME_IT   = weighted.mean(dstb_dme_intsvtx_cost_country, notif), 
              wgtDNME_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
              wgtIE_IT   = weighted.mean(dstb_ie_intsvtx_cost_country, notif), 
              wgtDME_CT   = weighted.mean(dstb_dme_contntx_cost_country, notif), 
              wgtDNME_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
              wgtIE_CT   = weighted.mean(dstb_ie_contntx_cost_country, notif), 
              wgtPT = weighted.mean(dstb_prediag_cost_country, notif), 
              wgtIT = weighted.mean(dstb_intsvtx_cost_country, notif), 
              wgtCT = weighted.mean(dstb_contntx_cost_country, notif), 
              wgtDME   = weighted.mean(dstb_dme_cost_country, notif), 
              wgtDNME  = weighted.mean(dstb_dnme_cost_country, notif),
              wgtIE    = weighted.mean(dstb_ie_cost_country, notif),
              wgtTotal = weighted.mean(dstb_total_cost_country, notif), 
              .by=c(`.draw`)) 

# summaryEstUcrntyTxSA1 <- summaryEstUcrntyTbl0 %>% 
#     ungroup() %>%
#     mutate(txCost = wgtIT + wgtCT) %>% 
#     mean_qi(txCost) %>% 
#     select(!c(.width, .point, .interval)) %>% round() ; summaryEstUcrntyTx


summaryEstUcrntyTblSA1 <- summaryEstUcrntyTblSA10%>% 
    ungroup() %>% 
    mean_qi(wgtDME_PT, wgtDNME_PT, wgtIE_PT,
            wgtDME_IT, wgtDNME_IT, wgtIE_IT,
            wgtDME_CT, wgtDNME_CT, wgtIE_CT,
            wgtPT, wgtIT, wgtCT, 
            wgtDME, wgtDNME, wgtIE, 
            wgtTotal) %>% 
    select(!c(.width, .point, .interval)) %>% round() 

colnames(summaryEstUcrntyTblSA1)[seq(1, ncol(summaryEstUcrntyTblSA1), 3)] <- paste(colnames(summaryEstUcrntyTblSA1)[seq(1, ncol(summaryEstUcrntyTblSA1), 3)], "est", sep=".")

summaryEstUcrntyTblSA1 <- summaryEstUcrntyTblSA1 %>% 
    pivot_longer(everything(), names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>% 
    pivot_wider(id_cols = Category, names_from = Type, values_from = Cost) %>% 
    mutate(across(est:upper, round), 
           formatted = paste0(est, " (", 
                              lower, ", ", 
                              upper, ")")) ; summaryEstUcrntyTblSA1


###############################################################################
###############################################################################
###############################################################################

###############################################################################
#### Filter to the non-WHO surveys for model data
###############################################################################
modelDataSA1b <- modelData %>% 
    filter(!covidence.id %in% 
               ### These are the WHO patient costs surveys 
               c(1:4, 8688, 7799, 8781, 7682, 9330,
                 8270, 9620, 7443,7355, 7919, 9422,
                 8310, 9585, 8634, 8677, 9301)) %>%
    ungroup() %>%
    group_by(covidence.id) %>%
    mutate(survey.id = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(country.code) %>% 
    mutate(country.id = cur_group_id()) %>% ungroup()


#### Filter the covariates for 2024 values 
covariates24 <- covariatesTime0 %>% 
    filter(study.end.year == 2024)

### Truncate the covariates 
covariates24 <- covariates24 %>% 
    mutate(log.median.income = log(median.income),
           
           median.income.trunc = case_when(median.income < min(modelDataSA1b$median.income) ~ min(modelDataSA1b$median.income), 
                                           median.income > max(modelDataSA1b$median.income) ~ max(modelDataSA1b$median.income),
                                           .default = median.income), 
           
           log.median.income.trunc = case_when(log.median.income < min(modelDataSA1b$log.median.income) ~ min(modelDataSA1b$log.median.income), 
                                               log.median.income > max(modelDataSA1b$log.median.income) ~ max(modelDataSA1b$log.median.income),
                                               .default = log.median.income), 
           oop.perc.che.trunc = case_when(oop.perc.che < min(modelDataSA1b$oop.perc.che) ~ min(modelDataSA1b$oop.perc.che), 
                                          oop.perc.che > max(modelDataSA1b$oop.perc.che) ~ max(modelDataSA1b$oop.perc.che),
                                          .default = oop.perc.che), 
           uhc.index.trunc = case_when(uhc.index < min(modelDataSA1b$UHCindex) ~ min(modelDataSA1b$UHCindex), 
                                       uhc.index > max(modelDataSA1b$UHCindex) ~ max(modelDataSA1b$UHCindex),
                                       .default = uhc.index))

covariates24 <- covariates24 %>% 
    group_by(study.country) %>% 
    # na.omit() %>%
    mutate(country.id.new = cur_group_id(), 
           in.review = ifelse(country.code %in% modelDataSA1b$country.code, 
                              1, 0)) %>% 
    left_join(modelDataSA1b %>% ungroup() %>% 
                  select(country.code, country.id) %>% unique()) %>% 
    mutate(in.review.id = ifelse(is.na(country.id), 99, country.id), 
           oop.perc.che = as.numeric(oop.perc.che)) %>% 
    select(!country.id) 

#### This is important for aligning the results to the covariates!! 
covariates24 <- arrange(covariates24, country.id.new)

### Stan requires a list for data. 
SA1bData <- list(
    N = length(modelDataSA1b$cost),
    cost = modelDataSA1b$cost,
    # se = rep(0, length(modelDataSA1b$cost)),
    se = as.numeric(modelDataSA1b$se)+ 0.01,
    dme_prediag = modelDataSA1b$dme.prediag, 
    dnme_prediag =  modelDataSA1b$dnme.prediag, 
    ie_prediag =  modelDataSA1b$ie.prediag, 
    dme_intsvtx = modelDataSA1b$dme.intsvtx, 
    dnme_intsvtx =  modelDataSA1b$dnme.intsvtx, 
    ie_intsvtx =  modelDataSA1b$ie.intsvtx,
    dme_contntx = modelDataSA1b$dme.contntx, 
    dnme_contntx =  modelDataSA1b$dnme.contntx, 
    ie_contntx =  modelDataSA1b$ie.contntx, 
    N_survey = max(modelDataSA1b$survey.id),
    survey_id = modelDataSA1b$survey.id, 
    N_country = max(modelDataSA1b$country.id),
    country_id = modelDataSA1b$country.id, 
    ### Internal covariate
    drtb_ind= modelDataSA1b$drtb.wt,
    survey_year = ((modelDataSA1b$study.end.year - mean(modelDataSA1b$study.end.year)) / sd(modelDataSA1b$study.end.year)),
    ### External covariates24
    median_income = ((modelDataSA1b$log.median.income - mean(modelDataSA1b$log.median.income)) / sd(modelDataSA1b$log.median.income)),
    oop_perc_che =  (modelDataSA1b$oop.perc.che - mean(modelDataSA1b$oop.perc.che))/sd(modelDataSA1b$oop.perc.che), 
    uhc_index =     (modelDataSA1b$UHCindex - mean(modelDataSA1b$UHCindex)) / sd(modelDataSA1b$UHCindex), 
    ### Prediction data 
    N_country_new = max(covariates24$country.id.new),
    review_ind = covariates24$in.review, 
    in_review_id = covariates24$in.review.id, # note that this is different from modelDataSA1b country.id
    pred_year = (2024 - mean(modelDataSA1b$study.end.year)) / sd(modelDataSA1b$study.end.year), 
    median_income_new = (covariates24$log.median.income.trunc - mean(modelDataSA1b$log.median.income)) / sd(modelDataSA1b$log.median.income), 
    oop_perc_che_new = (covariates24$oop.perc.che.trunc - mean(modelDataSA1b$oop.perc.che)) / sd(modelDataSA1b$oop.perc.che), 
    uhc_index_new = (covariates24$uhc.index.trunc - mean(modelDataSA1b$UHCindex)) / sd(modelDataSA1b$UHCindex), 
    prior_sigma = 2, 
    prob_cost_zero = sum(modelDataSA1b$cost == 0) / nrow(modelDataSA1b),
    natrep_id = modelDataSA1b$whoSurv
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|

lmicSampsSA1b <- rstan::sampling(
    predModelNew, SA1bData,
    chains = 1, cores = 4,
    warmup = tbPtntCstWarmup/2,
    iter = tbPtntCstIter/4,
    control=list(max_treedepth=14),
    refresh = 0, seed = tbPtntCstSeed
)


tidybayes::spread_draws(lmicSampsSA1b, 
                        dstb_total_cost_country[country.id.new], 
                        ndraws = 750, seed = tbPtntCstSeed) %>% 
    group_by(country.id.new) %>%
    mean_qi(dstb_total_cost_country)  %>%
    mean_qi(dstb_total_cost_country) 

###############################################################################
###############################################################################
###############################################################################

##############################################################################|
##### Sensitivity analyses #2 
##### DRTB =1 or 0
##############################################################################|

modelDataSA2 <- modelData %>% 
                filter(drtb.wt ==1 | drtb.wt ==0)  %>%
    ungroup() %>%
    group_by(covidence.id) %>%
    mutate(survey.id = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(country.code) %>% 
    mutate(country.id = cur_group_id()) %>% ungroup()

### Stan requires a list for data. 
SA2Data <- list(
    N = length(modelDataSA2$cost),
    cost = modelDataSA2$cost + 0.01,
    # se = rep(0, length(modelDataSA2$cost)),
    se = as.numeric(modelDataSA2$se)+ 0.01,
    dme_prediag = modelDataSA2$dme.prediag, 
    dnme_prediag =  modelDataSA2$dnme.prediag, 
    ie_prediag =  modelDataSA2$ie.prediag, 
    dme_intsvtx = modelDataSA2$dme.intsvtx, 
    dnme_intsvtx =  modelDataSA2$dnme.intsvtx, 
    ie_intsvtx =  modelDataSA2$ie.intsvtx,
    dme_contntx = modelDataSA2$dme.contntx, 
    dnme_contntx =  modelDataSA2$dnme.contntx, 
    ie_contntx =  modelDataSA2$ie.contntx, 
    N_survey = max(modelDataSA2$survey.id),
    survey_id = modelDataSA2$survey.id, 
    N_country = max(modelDataSA2$country.id),
    country_id = modelDataSA2$country.id,
    id_matrix = modelDataSA2 %>% select(survey.id, country.id) %>% distinct(), 
    
    ### Internal covariate
    drtb_ind= modelDataSA2$drtb.wt,
    survey_year = modelDataSA2$year_z,
    ### External covariates
    median_income = ((modelDataSA2$log.median.income - mean(modelDataSA2$log.median.income)) / sd(modelDataSA2$log.median.income)),
    oop_perc_che =  (modelDataSA2$oop.perc.che - mean(modelDataSA2$oop.perc.che))/sd(modelDataSA2$oop.perc.che), 
    uhc_index =     (modelDataSA2$UHCindex - mean(modelDataSA2$UHCindex)) / sd(modelDataSA2$UHCindex), 
    ### Prediction data 
    N_country_new = max(covariateFilt24$country.id.new),
    review_ind = covariateFilt24$in.review, 
    in_review_id = covariateFilt24$in.review.id, # note that this is different from modelData country.id
    pred_year = (2024 - mean(modelDataSA2$study.end.year)) / sd(modelDataSA2$study.end.year), 
    median_income_new = (covariateFilt24$log.median.income.trunc - mean(modelDataSA2$log.median.income)) / sd(modelDataSA2$log.median.income), 
    oop_perc_che_new = (covariateFilt24$oop.perc.che.trunc - mean(modelDataSA2$oop.perc.che)) / sd(modelDataSA2$oop.perc.che), 
    uhc_index_new = (covariateFilt24$uhc.index.trunc - mean(modelDataSA2$UHCindex)) / sd(modelDataSA2$UHCindex), 
    prior_sigma =2, 
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
    
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|
predModel <- stan_model("code/inst/model5.stan")

lmicSampsSA2 <- rstan::sampling(
    predModel, SA2Data,
    chains = 4, cores = 4,
    warmup = tbPtntCstWarmup,
    iter = tbPtntCstIter,
    control=list(max_treedepth=14),
    refresh = 0, seed = tbPtntCstSeed
)

saveRDS(lmicSampsSA2, here("output/sensAnalysis2.rds"))


##############################################################################|
##### Sensitivity analyses #3 
##### Preferred surveys 
##############################################################################|

modelDataSA3 <- modelData %>% 
    filter()

### Stan requires a list for data. 
SA3Data <- list(
    N = length(modelDataSA3$cost),
    cost = modelDataSA3$cost + 0.01,
    # se = rep(0, length(modelDataSA3$cost)),
    se = as.numeric(modelDataSA3$se)+ 0.01,
    dme_prediag = modelDataSA3$dme.prediag, 
    dnme_prediag =  modelDataSA3$dnme.prediag, 
    ie_prediag =  modelDataSA3$ie.prediag, 
    dme_intsvtx = modelDataSA3$dme.intsvtx, 
    dnme_intsvtx =  modelDataSA3$dnme.intsvtx, 
    ie_intsvtx =  modelDataSA3$ie.intsvtx,
    dme_contntx = modelDataSA3$dme.contntx, 
    dnme_contntx =  modelDataSA3$dnme.contntx, 
    ie_contntx =  modelDataSA3$ie.contntx, 
    N_survey = max(modelDataSA3$survey.id),
    survey_id = modelDataSA3$survey.id, 
    N_country = max(modelDataSA3$country.id),
    country_id = modelDataSA3$country.id, 
    id_matrix = modelData %>% select(survey.id, country.id) %>% distinct(), 
    ### Internal covariate
    drtb_ind= modelDataSA3$drtb.wt,
    survey_year = modelDataSA3$year_z,
    ### External covariates
    median_income = ((modelDataSA3$median.income - mean(modelDataSA3$median.income)) / sd(modelDataSA3$median.income)),
    oop_perc_che =  (modelDataSA3$oop.perc.che - mean(modelDataSA3$oop.perc.che))/sd(modelDataSA3$oop.perc.che), 
    uhc_index =     (modelDataSA3$UHCindex - mean(modelDataSA3$UHCindex)) / sd(modelDataSA3$UHCindex), 
    ### Prediction data 
    N_country_new = max(covariates$country.id.new),
    review_ind = covariates$in.review, 
    in_review_id = covariates$in.review.id, # note that this is different from modelDataSA3 country.id
    pred_year = unique(covariates$pred.year),
    median_income_new = (covariates$median.income.trunc- mean(modelDataSA3$median.income)) / sd(modelDataSA3$median.income), 
    oop_perc_che_new = (covariates$oop.perc.che.trunc - mean(modelDataSA3$oop.perc.che)) / sd(modelDataSA3$oop.perc.che), 
    uhc_index_new = (covariates$uhc.index.trunc - mean(modelDataSA3$UHCindex)) / sd(modelDataSA3$UHCindex), 
    prior_sigma = 3
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|
# predModel <- stan_model("code/inst/tbPatientCosts-randEffCov.stan")

lmicSampsSA3 <- rstan::sampling(
    predModel, predictionData,
    chains = 4, cores = 4,
    warmup = tbPtntCstWarmup,
    iter = tbPtntCstIter,
    control=list(max_treedepth=14),
    refresh = 0, seed = tbPtntCstSeed
)

saveRDS(lmicSampsSA3, here("output/sensAnalysis3.rds"))