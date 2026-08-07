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
           
           median.income.trunc = case_when(median.income < min(modelData$median.income) ~ min(modelData$median.income), 
                                           median.income > max(modelData$median.income) ~ max(modelData$median.income),
                                           .default = median.income), 
           
           log.median.income.trunc = case_when(log.median.income < min(modelData$log.median.income) ~ min(modelData$log.median.income), 
                                               log.median.income > max(modelData$log.median.income) ~ max(modelData$log.median.income),
                                               .default = log.median.income), 
           
           median.income.hh.trunc = case_when(median.income.hh < min(modelData$median.income.hh) ~ min(modelData$median.income.hh), 
                                              median.income.hh > max(modelData$median.income.hh) ~ max(modelData$median.income.hh),
                                              .default = median.income.hh), 
           
           log.median.income.hh.trunc = case_when(log.median.income.hh < min(modelData$log.median.income.hh) ~ min(modelData$log.median.income.hh), 
                                                  log.median.income.hh > max(modelData$log.median.income.hh) ~ max(modelData$log.median.income.hh),
                                                  .default = log.median.income.hh), 
           oop.perc.che.trunc = case_when(oop.perc.che < min(modelData$oop.perc.che) ~ min(modelData$oop.perc.che), 
                                          oop.perc.che > max(modelData$oop.perc.che) ~ max(modelData$oop.perc.che),
                                          .default = oop.perc.che), 
           uhc.index.trunc = case_when(uhc.index < min(modelData$UHCindex) ~ min(modelData$UHCindex), 
                                       uhc.index > max(modelData$UHCindex) ~ max(modelData$UHCindex),
                                       .default = uhc.index),
           cdr.rate = cdr.rate.interpolated,
           cdr.trunc = case_when(cdr.rate.interpolated < min(modelData$cdr.rate) ~ min(modelData$cdr.rate), 
                                 cdr.rate.interpolated > max(modelData$cdr.rate) ~ max(modelData$cdr.rate),
                                 .default = cdr.rate.interpolated)#, 
           # hosp.rate.trunc = case_when(hosp.rate.interpolated < min(modelData$hosp.rate.interpolated) ~ min(modelData$hosp.rate.interpolated), 
           #                             hosp.rate.interpolated > max(modelData$hosp.rate.interpolated) ~ max(modelData$hosp.rate.interpolated),
           #                             .default = hosp.rate.interpolated)
           ) %>% 
    select(!c(cdr.rate.interpolated)) %>% distinct()

covariatesTime <- arrange(covariatesTime, country.id.new) 
covariateFilt24 <- covariatesTime %>% filter(study.end.year==2024) 

# saveRDS(covariatesTime, file = here("data/covariatesTime.rds"))
# 
#############################################################################|
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
        id_matrix = modelData %>% select(survey.id, country.id) %>% distinct(), 
        
        ### Internal covariate
        drtb_ind= modelData$drtb.wt,
        survey_year = ((modelData$study.end.year - mean(modelData$study.end.year)) / sd(modelData$study.end.year)),
        ### External covariateFilt
        # median_income = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income)),
        median_income = ((modelData$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh)),
        oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
        # uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
        cdr_rate = (modelData$cdr.rate - mean(modelData$cdr.rate)) / sd(modelData$cdr.rate), 
        ### Prediction data 
        N_country_new = max(covariateFilt$country.id.new),
        review_ind = covariateFilt$in.review, 
        in_review_id = covariateFilt$in.review.id, # note that this is different from modelData country.id
        pred_year = (t - mean(modelData$study.end.year)) / sd(modelData$study.end.year), 
        
        median_income_new = (covariateFilt$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh),
        oop_perc_che_new = (covariateFilt$oop.perc.che - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che), 
        cdr_rate_new = (covariateFilt$cdr.rate - mean(modelData$cdr.rate)) / sd(modelData$cdr.rate),
        prior_sigma = 2, 
        prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData) #, 
        # natrep_id = modelData$whoSurv, 
        # tau_scalar = 1 / sqrt(n.participants)
    )
    
    # lmicSampsTime[[as.character(t)]] <- stan(file = "code/inst/tbPatientCosts-predictive.stan",
    lmicSampsTime[[as.character(t)]]  <- stan(file = here("code/inst/model7.stan"),
                                             data = predDataFilt,
                                             iter = tbPtntCstIter, chains = 3,
                                             warmup = tbPtntCstWarmup,
                                             control=list(max_treedepth=14), 
                                             seed = tbPtntCstSeed)
    
}
# lmicSampsAll <- readRDS(here("output/allYears_07-30.rds"))
saveRDS(lmicSampsTime, here("output/allYears_08-07.rds"), version=2)

################ RUN FOR ONLY 2024 ################ ################ 

covariateFilt24 <- covariatesTime %>% filter(study.end.year==2024) 


predDataFilt24 <- list(
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

    prior_sigma = 2,
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData),
    natrep_id = modelData$whoSurv,
    # tau_scalar = 1 / sqrt(modelData$n.participants),
    tau_scalar = rep(1,nrow(modelData)),

    ### External covariateFilt
    median_income = ((modelData$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh)),
    oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che),
    uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex),
    ### Prediction data
    N_country_new = max(covariateFilt24$country.id.new),
    review_ind = covariateFilt24$in.review,
    in_review_id = covariateFilt24$in.review.id, # note that this is different from modelData country.id
    pred_year = (2024 - mean(modelData$study.end.year)) / sd(modelData$study.end.year),
    median_income_new = (covariateFilt24$log.median.income - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh),
    oop_perc_che_new = (covariateFilt24$oop.perc.che - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che),
    uhc_index_new = (covariateFilt24$uhc.index - mean(modelData$UHCindex)) / sd(modelData$UHCindex)
)


lmicSamps24 <- stan(file = "code/inst/model7.stan",
                    data = predDataFilt,
                    iter = tbPtntCstIter, chains = 3,
                    warmup = tbPtntCstWarmup,
                    control=list(max_treedepth=14),
                    seed = tbPtntCstSeed)

# saveRDS(lmicSamps24, file = here("output/mainEst2024_08-07-noTrunc.rds"))
# betas24<- tidybayes::spread_draws(lmicSamps24,
#                                   beta_dme_prediag,
#                                   beta_dnme_prediag,
#                                   beta_ie_prediag,
#                                   beta_dme_intsvtx,
#                                   beta_dnme_intsvtx,
#                                   beta_ie_intsvtx,
#                                   beta_dme_contntx,
#                                   beta_dnme_contntx,
#                                   beta_ie_contntx,
#                                   mult_drtb_dme_prediag,
#                                   mult_drtb_dnme_prediag,
#                                   mult_drtb_ie_prediag,
#                                   mult_drtb_dme_intsvtx,
#                                   mult_drtb_dnme_intsvtx,
#                                   mult_drtb_ie_intsvtx,
#                                   mult_drtb_dme_contntx,
#                                   mult_drtb_dnme_contntx,
#                                   mult_drtb_ie_contntx,
                                  # mult_natrep,
                                  # ndraws = 1000, seed = tbPtntCstSeed)

# betas24 %>% mean_qi()  %>% select(! c(.width, .point, .interval)) %>% exp() %>% matrix(ncol=3, byrow = TRUE)

costCntry24 <- tidybayes::spread_draws(lmicSamps24,
                                          dstb_total_cost_country[country.id.new],
                                          ndraws = 1000, seed = tbPtntCstSeed);

costCntryEst24 <- left_join(costCntry24  %>%
                           group_by(country.id.new) %>%
                           mean_qi(dstb_total_cost_country) %>%
                                 select(! c(.width, .point, .interval)),
                           covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country))

ggplot(data=costCntryEst24) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    geom_errorbar(aes(x=study.country, ymax=.upper, ymin=.lower, color=study.country)) + 
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from model with international dollars") + 
    expand_limits(y=c(0,50000))

costCntry24 %>%
    left_join(covariateFilt24, by="country.id.new") %>%
    left_join(notif) %>%
    # mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    filter(!is.na(notif)) %>%
    group_by(`.draw`, country.id.new) %>%
    mutate(wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>%
    ungroup() %>%
    mean_qi(wgtTotal) %>%
    select(!c(.width, .point, .interval)) %>% round()


######################################################################################
##### household income 
predDataFilt24hh <- predDataFilt24
predDataFilt24hh[["median_income"]] <- (modelData$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh) 
predDataFilt24hh[["median_income_new"]] <- (covariateFilt24$log.median.income.hh.trunc - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh) 



lmicSamps24hh <- stan(file = "code/inst/tbPatientCosts-predictive.stan",
                    data = predDataFilt24hh,
                    iter = tbPtntCstIter/4, chains = 2,
                    warmup = tbPtntCstWarmup/2,
                    control=list(max_treedepth=14), 
                    seed = tbPtntCstSeed)

costCntry24hh <- tidybayes::spread_draws(lmicSamps24hh,
                                       dstb_total_cost_country[country.id.new],
                                       ndraws = 1000, seed = tbPtntCstSeed);

costCntryEst24hh <- left_join(costCntry24hh  %>%
                                group_by(country.id.new) %>%
                                mean_qi(dstb_total_cost_country) %>%
                                select(! c(.width, .point, .interval)),
                            covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country))

ggplot(data=costCntryEst24hh) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    geom_errorbar(aes(x=study.country, ymax=.upper, ymin=.lower, color=study.country)) + 
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from model with hh international dollars") + 
    expand_limits(y=c(0,50000))

costCntry24hh %>%
    left_join(covariateFilt24, by="country.id.new") %>%
    left_join(notif) %>%
    # mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    filter(!is.na(notif)) %>%
    group_by(`.draw`, country.id.new) %>%
    mutate(wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>%
    ungroup() %>%
    mean_qi(wgtTotal) %>%
    select(!c(.width, .point, .interval)) %>% round()


ggplot(data=medianIncome %>% 
           filter(country.code %in% unique(LMIC$country.code[67:132]))) + 
    # geom_line(aes(x=study.end.year, y = median.income.calc.usd), color = "orange") + 
    geom_line(aes(x=study.end.year, y = median.income), color = "green") + 

    theme_minimal() + 
    facet_wrap(facets=vars(country.code), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################
##### household income 
predDataFilt24hhTau <- predDataFilt24hh
predDataFilt24hhTau[["tau_scalar"]] <- 1 / sqrt(modelData$n.participants)



lmicSamps24hhTau <- stan(file = "code/inst/tbPatientCosts-predictive.stan",
                      data = predDataFilt24hhTau,
                      iter = tbPtntCstIter/4, chains = 2,
                      warmup = tbPtntCstWarmup/2,
                      control=list(max_treedepth=14), 
                      seed = tbPtntCstSeed)

costCntry24hhTau <- tidybayes::spread_draws(lmicSamps24hhTau,
                                         dstb_total_cost_country[country.id.new],
                                         ndraws = 1000, seed = tbPtntCstSeed);

costCntryEst24hhTau <- left_join(costCntry24hhTau  %>%
                                  group_by(country.id.new) %>%
                                  mean_qi(dstb_total_cost_country) %>%
                                  select(! c(.width, .point, .interval)),
                              covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country))

ggplot(data=costCntryEst24hhTau) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    geom_errorbar(aes(x=study.country, ymax=.upper, ymin=.lower, color=study.country)) + 
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from model with hh international dollars + scale tau") + 
    expand_limits(y=c(0,50000))

costCntry24hhTau %>%
    left_join(covariateFilt24, by="country.id.new") %>%
    left_join(notif) %>%
    # mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    filter(!is.na(notif)) %>%
    group_by(`.draw`, country.id.new) %>%
    mutate(wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>%
    ungroup() %>%
    mean_qi(wgtTotal) %>%
    select(!c(.width, .point, .interval)) %>% round()


costCntry24hhTau  %>%
    group_by(country.id.new) %>%
    mean_qi(dstb_total_cost_country) %>% 
    mutate(dispersion = .upper / .lower) %>% 
    mean_qi(dispersion)

costCntry24hh  %>%
    group_by(country.id.new) %>%
    mean_qi(dstb_total_cost_country) %>% 
    mutate(dispersion = .upper / .lower) %>% 
    mean_qi(dispersion)

modelData %>% select(covidence.id, n.participants,study.country) %>% distinct() %>% ggplot() %>% geom_point(aes(x=covidence.id, y=n.participants, color = study.country)) 
