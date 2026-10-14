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
    select(country.code, study.end.year, median.income.calc.intd.2025, median.income.calc.intd.2025.hh) %>% 
    rename(
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

#### Hospital data
hospData0 <- read.csv("~/Downloads/TB_expenditure_utilisation_2026-07-21.csv") %>% 
    select(iso3, year, 
           # hcfvisit_dstb, 
           # hcfvisit_mdr,
           hospd_dstb_prct
           # hospd_mdr_prct
           ) %>% 
    rename(country.code = iso3,
           study.end.year = year,
           hosp.rate = hospd_dstb_prct) %>% 
    right_join(LMIC %>% crossing(study.end.year = 2000:2024))

### Add interpolation of hosp rate 
for (country in unique(hospData0$study.country)){
    print(country)
    tmpDF <- hospData0 %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$hosp.rate)) > 1){
        hospData0[which(hospData0$study.country == country), "hosp.rate.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$hosp.rate)],
                                                                                                            y=tmpDF$hosp.rate[!is.na(tmpDF$hosp.rate)], xout=tmpDF$study.end.year,
                                                                                                            rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$hosp.rate))== 1){
        hospData0[which(hospData0$study.country == country), "hosp.rate.interpolated"] <- na.omit(tmpDF$hosp.rate)[1]
    }
}

ggplot(data = hospData0) + 
    geom_point(aes(x=study.end.year, y=hosp.rate)) + 
    geom_line(aes(x=study.end.year, y=hosp.rate.interpolated)) + 
    facet_wrap(vars(study.country))


### Add to model data 
modelData <- left_join(modelData, cdrData) %>% 
             left_join(hospData0 %>% 
                       select(country.code, study.end.year, hosp.rate.interpolated)) %>%
             filter(!is.na(hosp.rate.interpolated)) %>%
             rename(cdr.rate = c_cdr)

###############################################################################
##### Setup covariates for the prediction 
###############################################################################
covariatesTime000 <- left_join(oopSpending, 
                              medianIncome, 
                              by = c("country.code", "study.end.year"))

covariatesTime00 <- left_join(covariatesTime000, cdrData) %>% 
                    left_join(hospData0) %>% 
                    # filter(!is.na(c_cdr))  %>% 
                    rename(cdr.rate = c_cdr) %>%     
                    mutate(country.id.new = cur_group_id(), 
                           in.review = ifelse(country.code %in% modelData$country.code,1, 0)) %>% 
                    left_join(modelData %>% ungroup() %>% 
                                  select(country.code, country.id) %>% unique()) %>% 
                    mutate(in.review.id = ifelse(is.na(country.id), 99, country.id)) %>% 
                    select(! study.country)

covariatesTime0  <- left_join(covariatesTime00, 
                              uhcIndex  %>%
                                  filter(study.end.year >  2008,
                                         country.code %in% LMIC$country.code), 
                              by = c("country.code", "study.end.year")) %>% 
                    select(country.code, study.country, study.end.year,
                           median.income, median.income.hh, oop.perc.che,
                           uhc.index, cdr.rate, hosp.rate.interpolated)

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
    group_by(study.country) %>% 
    filter(!is.na(hosp.rate.interpolated)) %>% 
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
           cdr.trunc = case_when(cdr.rate.interpolated < min(modelData$cdr.rate) ~ min(modelData$cdr.rate), 
                                 cdr.rate.interpolated > max(modelData$cdr.rate) ~ max(modelData$cdr.rate),
                                       .default = cdr.rate.interpolated), 
           hosp.rate.trunc = case_when(hosp.rate.interpolated < min(modelData$hosp.rate.interpolated) ~ min(modelData$hosp.rate.interpolated), 
                                       hosp.rate.interpolated > max(modelData$hosp.rate.interpolated) ~ max(modelData$hosp.rate.interpolated),
                                       .default = hosp.rate.interpolated)) %>% 
    select(!c(cdr.rate, cdr.rate.interpolated, hosp.rate.interpolated))

covariatesTime <- arrange(covariatesTime, country.id.new)
covariateFilt24 <- covariatesTime %>% filter(study.end.year==2024) 

##############################################################################|
##### Format all the model datasets for stan                              #####
##############################################################################|

stanData1 <- list(
    N = length(modelData$cost),
    cost = modelData$cost,
    se = rep(0, nrow(modelData)),
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
    ### Prediction data 
    N_country_new = max(covariateFilt24$country.id.new),
    review_ind = covariateFilt24$in.review, 
    in_review_id = covariateFilt24$in.review.id, 
    prior_sigma = 2, 
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
)

stanData2 <- stanData1
stanData2[["tau_scalar"]]  = ifelse(is.na(1/sqrt(modelData$n.participants)), 
                                    1/sqrt(mean(modelData$n.participants, na.rm=T)), 
                                    1/sqrt(modelData$n.participants))

stanData3 <- stanData2
stanData3[["survey_year"]]  = (modelData$study.end.year - mean(modelData$study.end.year)) / 
    sd(modelData$study.end.year)

stanData3[["pred_year"]]  = ((covariateFilt24$study.end.year - mean(modelData$study.end.year)) / 
                                 sd(modelData$study.end.year))[1]

stanData4 <- stanData3
stanData4[["median_income"]] = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income))
stanData4[["median_income_new"]] = (covariateFilt24$log.median.income.trunc - mean(modelData$log.median.income)) / sd(modelData$log.median.income)

stanData5 <- stanData3
stanData5[["median_income"]] = ((modelData$log.median.income.hh - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh))
stanData5[["median_income_new"]] = (covariateFilt24$log.median.income.hh.trunc - mean(modelData$log.median.income.hh)) / sd(modelData$log.median.income.hh)

stanData6 <- stanData4
stanData6[["oop_perc_che"]] = (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che)
stanData6[["oop_perc_che_new"]] = (covariateFilt24$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che)

stanData7 <- stanData6
stanData7[["uhc_index"]] = (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex) 
stanData7[["uhc_index_new"]] = (covariateFilt24$uhc.index.trunc - mean(modelData$UHCindex)) / sd(modelData$UHCindex) 

stanData8 <- stanData7
stanData8[["cdr_rate"]] = (modelData$cdr.rate - mean(modelData$cdr.rate)) / sd(modelData$cdr.rate) 
stanData8[["cdr_rate_new"]] = (covariateFilt24$cdr.trunc - mean(modelData$cdr.rate)) / sd(modelData$cdr.rate) 

stanData9 <- stanData8
stanData9[["hosp_rate"]] = (modelData$hosp.rate.interpolated - mean(modelData$hosp.rate.interpolated)) / sd(modelData$hosp.rate.interpolated) 
stanData9[["hosp_rate_new"]] = (covariateFilt24$hosp.rate.trunc - mean(modelData$hosp.rate.interpolated)) / sd(modelData$hosp.rate.interpolated) 


##############################################################################|
##### MODEL ONE: MODEL WITH ONLY DRTB + CNTRY RE + SURV RE                #####
##############################################################################|

model1 <- stan(file = here("code/inst/model1.stan"),
               data = stanData1,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps1 <- tidybayes::spread_draws(model1,
                                           dstb_total_cost_country[country.id.new],
                                           ndraws = 1000, seed = tbPtntCstSeed);

est1<- left_join(samps1  %>%
                                 group_by(country.id.new) %>%
                                 mean_qi(dstb_total_cost_country) %>%
                                 select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est1$dstb_total_cost_country)

coef1 <- as.data.frame(model1) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          mult_drtb_dme_prediag,
                                          mult_drtb_dnme_prediag,
                                          mult_drtb_ie_prediag, 
                                          mult_drtb_dme_intsvtx,
                                          mult_drtb_dnme_intsvtx,
                                          mult_drtb_ie_intsvtx,
                                          mult_drtb_dme_contntx,
                                          mult_drtb_dnme_contntx,
                                          mult_drtb_ie_contntx) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef1) <- gsub("mult_drtb", "drtbMult", colnames(coef1), fixed=TRUE)
colnames(coef1) <- gsub(".", "_", colnames(coef1), fixed=TRUE)

coef1 <- coef1 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### MODEL TWO: MODEL WITH DRTB + YEAR + CNTRY RE + SURV RE + SE Scaling #####
##############################################################################|

model2 <- stan(file = here("code/inst/model2.stan"),
               data = stanData2,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps2 <- tidybayes::spread_draws(model2,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est2<- left_join(samps2  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est2$dstb_total_cost_country)

coef2 <- as.data.frame(model2) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          mult_drtb_dme_prediag,
                                          mult_drtb_dnme_prediag,
                                          mult_drtb_ie_prediag, 
                                          mult_drtb_dme_intsvtx,
                                          mult_drtb_dnme_intsvtx,
                                          mult_drtb_ie_intsvtx,
                                          mult_drtb_dme_contntx,
                                          mult_drtb_dnme_contntx,
                                          mult_drtb_ie_contntx) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef2) <- gsub("mult_drtb", "drtbMult", colnames(coef2), fixed=TRUE)
colnames(coef2) <- gsub(".", "_", colnames(coef2), fixed=TRUE)

coef2 <- coef2 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### MODEL THREE: MODEL WITH DRTB + YEAR + CNTRY RE + SURV RE              #####
##############################################################################|

model3 <- stan(file = here("code/inst/model3.stan"),
               data = stanData3,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps3 <- tidybayes::spread_draws(model3,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est3<- left_join(samps3  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est3$dstb_total_cost_country)

coef3 <- as.data.frame(model3) %>% select(beta_dme_prediag,
                               beta_dnme_prediag,
                               beta_ie_prediag,
                               beta_dme_intsvtx,
                               beta_dnme_intsvtx,
                               beta_ie_intsvtx,
                               beta_dme_contntx,
                               beta_dnme_contntx,
                               beta_ie_contntx,
                               mult_drtb_dme_prediag,
                               mult_drtb_dnme_prediag,
                               mult_drtb_ie_prediag, 
                               mult_drtb_dme_intsvtx,
                               mult_drtb_dnme_intsvtx,
                               mult_drtb_ie_intsvtx,
                               mult_drtb_dme_contntx,
                               mult_drtb_dnme_contntx,
                               mult_drtb_ie_contntx,
                               
                               mult_year_dme_prediag,
                               mult_year_dnme_prediag,
                               mult_year_ie_prediag, 
                               mult_year_dme_intsvtx,
                               mult_year_dnme_intsvtx,
                               mult_year_ie_intsvtx,
                               mult_year_dme_contntx,
                               mult_year_dnme_contntx,
                               mult_year_ie_contntx) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef3) <- gsub("mult_year", "drtbYear", colnames(coef3), fixed=TRUE)
colnames(coef3) <- gsub("mult_drtb", "drtbMult", colnames(coef3), fixed=TRUE)
colnames(coef3) <- gsub(".", "_", colnames(coef3), fixed=TRUE)

coef3 <- coef3 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### MODEL FOUR: MODEL WITH DRTB + YEAR + MED.INC + CNTRY RE + SURV RE  #####
##############################################################################|

model4 <- stan(file = here("code/inst/model4.stan"),
               data = stanData4,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps4 <- tidybayes::spread_draws(model4,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est4<- left_join(samps4  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est4$dstb_total_cost_country)


samps4 <- tidybayes::spread_draws(model4,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);


coef4 <- as.data.frame(model4) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef4) <- gsub("mult_year", "yearMult", colnames(coef4), fixed=TRUE)
colnames(coef4) <- gsub("mult_drtb", "drtbMult", colnames(coef4), fixed=TRUE)
colnames(coef4) <- gsub("mult_med_income", "medIncMult", colnames(coef4), fixed=TRUE)
colnames(coef4) <- gsub(".", "_", colnames(coef4), fixed=TRUE)

coef4 <- coef4 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### MODEL FOUR: MODEL WITH DRTB + YEAR + MED.INC + CNTRY RE + SURV RE  #####
##############################################################################|

model5 <- stan(file = here("code/inst/model4.stan"),
               data = stanData5,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps5 <- tidybayes::spread_draws(model5,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est5<- left_join(samps5  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est5$dstb_total_cost_country)

ggplot(data=est5) + 
    geom_point(aes(x= study.country, y = dstb_total_cost_country, color=as.factor(in.review))) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper, color=as.factor(in.review))) + 
    theme_minimal() + coord_flip() + 
    scale_color_manual(values=c("grey", "red"))



samps5 <- tidybayes::spread_draws(model5,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);


coef5 <- as.data.frame(model5) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef5) <- gsub("mult_year", "yearMult", colnames(coef5), fixed=TRUE)
colnames(coef5) <- gsub("mult_drtb", "drtbMult", colnames(coef5), fixed=TRUE)
colnames(coef5) <- gsub("mult_med_income", "medIncMult", colnames(coef5), fixed=TRUE)
colnames(coef5) <- gsub(".", "_", colnames(coef5), fixed=TRUE)

coef5 <- coef5 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### CHECK HOUSEHOLD VS INDIVIDUAL MEDIAN INCOME BETTER                   ####
##############################################################################|

loo::loo_compare(loo(model4), loo(model5))

##############################################################################|
##### MODEL SIX: MODEL W/ DRTB + YEAR + MED.INC + OOP%CHE + CNTRY.RE + SURV.RE ####
##############################################################################|

model6 <- stan(file = here("code/inst/model5.stan"),
               data = stanData6,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps6 <- tidybayes::spread_draws(model6,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est6<- left_join(samps6  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new, in.review, median.income.hh) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

ggplot(data=est6) + 
    geom_point(aes(x= study.country, y = dstb_total_cost_country, color=as.factor(in.review))) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper, color=as.factor(in.review))) + 
    theme_minimal() + coord_flip() + 
    scale_color_manual(values=c("grey", "red"))

ggplot(data=est6) + 
    geom_point(aes(x= study.country, y = median.income.hh, color=as.factor(in.review))) + 
    # geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper, color=as.factor(in.review))) + 
    theme_minimal() + coord_flip() + 
    scale_color_manual(values=c("grey", "red"))

range(est6$dstb_total_cost_country)

coef6 <- as.data.frame(model6) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income"), 
                                          contains("mult_oop_perc_che")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef6) <- gsub("mult_year", "yearMult", colnames(coef6), fixed=TRUE)
colnames(coef6) <- gsub("mult_drtb", "drtbMult", colnames(coef6), fixed=TRUE)
colnames(coef6) <- gsub("mult_med_income", "medIncMult", colnames(coef6), fixed=TRUE)
colnames(coef6) <- gsub("mult_oop_perc_che", "oopMult", colnames(coef6), fixed=TRUE)
colnames(coef6) <- gsub(".", "_", colnames(coef6), fixed=TRUE)

coef6 <-coef6 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

##############################################################################|
##### MODEL SEVEN: MODEL W/ DRTB + YEAR + MED.INC + OOP%CHE + UHC + CNTRY.RE + SURV.RE ####
##############################################################################|

model7 <- stan(file = here("code/inst/model6.stan"),
               data = stanData7,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps7 <- tidybayes::spread_draws(model7,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est7<- left_join(samps7  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
        arrange(dstb_total_cost_country) %>% 
        mutate(study.country=factor(study.country, levels=study.country)) 

range(est7$dstb_total_cost_country)


coef7 <- as.data.frame(model7) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income"), 
                                          contains("mult_oop_perc_che"),
                                          contains("mult_uhc")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef7) <- gsub("mult_year", "yearMult", colnames(coef7), fixed=TRUE)
colnames(coef7) <- gsub("mult_drtb", "drtbMult", colnames(coef7), fixed=TRUE)
colnames(coef7) <- gsub("mult_med_income", "medIncMult", colnames(coef7), fixed=TRUE)
colnames(coef7) <- gsub("mult_oop_perc_che", "oopMult", colnames(coef7), fixed=TRUE)
colnames(coef7) <- gsub("mult_uhc", "uhcMult", colnames(coef7), fixed=TRUE)

colnames(coef7) <- gsub(".", "_", colnames(coef7), fixed=TRUE)

coef7 <- coef7 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

ggplot(data=est7) + 
    geom_point(aes(x=study.country, y = dstb_total_cost_country)) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper)) + 
    theme_minimal() + coord_flip()

##############################################################################|
##### MODEL EIGHT: MODEL W/ DRTB + YEAR + MED.INC + OOP%CHE + CDR + CNTRY.RE + SURV.RE ####
##############################################################################|

model8 <- stan(file = here("code/inst/model7.stan"),
               data = stanData8,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps8 <- tidybayes::spread_draws(model8,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est8<- left_join(samps8  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est8$dstb_total_cost_country)


coef8 <- as.data.frame(model8) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income"), 
                                          contains("mult_oop_perc_che"),
                                          contains("mult_cdr")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef8) <- gsub("mult_year", "yearMult", colnames(coef8), fixed=TRUE)
colnames(coef8) <- gsub("mult_drtb", "drtbMult", colnames(coef8), fixed=TRUE)
colnames(coef8) <- gsub("mult_med_income", "medIncMult", colnames(coef8), fixed=TRUE)
colnames(coef8) <- gsub("mult_oop_perc_che", "oopMult", colnames(coef8), fixed=TRUE)
colnames(coef8) <- gsub("mult_cdr", "cdrMult", colnames(coef8), fixed=TRUE)

colnames(coef8) <- gsub(".", "_", colnames(coef8), fixed=TRUE)

coef8 <- coef8 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

ggplot(data=est8) + 
    geom_point(aes(x=study.country, y = dstb_total_cost_country)) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper)) + 
    theme_minimal() + coord_flip()

##############################################################################|
##### MODEL EIGHT: MODEL W/ DRTB + YEAR + MED.INC + OOP%CHE + CDR + CNTRY.RE + SURV.RE ####
##############################################################################|

model9 <- stan(file = here("code/inst/model8.stan"),
               data = stanData9,
               iter = tbPtntCstIter, chains = 3,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

samps9 <- tidybayes::spread_draws(model9,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

est9<- left_join(samps9  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariateFilt24 %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(est9$dstb_total_cost_country)


coef9 <- as.data.frame(model9) %>% select(beta_dme_prediag,
                                          beta_dnme_prediag,
                                          beta_ie_prediag,
                                          beta_dme_intsvtx,
                                          beta_dnme_intsvtx,
                                          beta_ie_intsvtx,
                                          beta_dme_contntx,
                                          beta_dnme_contntx,
                                          beta_ie_contntx,
                                          contains("mult_drtb"),
                                          contains("mult_year"), 
                                          contains("mult_med_income"), 
                                          contains("mult_oop_perc_che"),
                                          contains("mult_hosp")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coef9) <- gsub("mult_year", "yearMult", colnames(coef9), fixed=TRUE)
colnames(coef9) <- gsub("mult_drtb", "drtbMult", colnames(coef9), fixed=TRUE)
colnames(coef9) <- gsub("mult_med_income", "medIncMult", colnames(coef9), fixed=TRUE)
colnames(coef9) <- gsub("mult_oop_perc_che", "oopMult", colnames(coef9), fixed=TRUE)
colnames(coef9) <- gsub("mult_hosp", "hospMult", colnames(coef9), fixed=TRUE)

colnames(coef9) <- gsub(".", "_", colnames(coef9), fixed=TRUE)

coef9 <- coef9 %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

ggplot(data=est9) + 
    geom_point(aes(x=study.country, y = dstb_total_cost_country)) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper)) + 
    theme_minimal() + coord_flip()


##############################################################################|
##### FORMAT ALL RESULTS FOR MODEL SPECIFICATION TABLE  ####
##############################################################################|

c1 <- coef1$formatted; c2 <- coef2$formatted; c3 <- coef3$formatted;
c4 <- coef4$formatted; c5 <- coef5$formatted; c6 <- coef6$formatted; 
c7 <- coef7$formatted; c8 <- coef8$formatted; c9 <- coef9$formatted;

length(c1) <- length(c2) <- length(c3) <- length(c4) <- length(c5) <- 
    length(c6) <-  length(c7) <- length(c8) <- nrow(coef9)

cbind(coef7 %>% select(type, category, phase), 
      c1, c2, c3, c4, c5, c6, c7, c8, coef9$formatted) %>% 
    write.csv(here("output/modelSpecificationTable/mainMods-hh.csv"))


##############################################################################|
##### MODEL EIGHT: MODEL W/ DRTB + YEAR + MED.INC + OOP%CHE + UHC + CNTRY.RE + SURV.RE ####
##############################################################################|


loo::loo_compare(loo(model1), 
                 loo(model2), 
                 loo(model3), 
                 loo(model4), 
                 loo(model5),
                 loo(model6),
                 loo(model7), 
                 loo(model8),
                 loo(model9))

models <- list(model1, model2, model3, 
               model4, model5, model6, 
               model7, model8, model9)
saveRDS(models, here("output/models.rds"))

model7 <- readRDS(here("output/models.rds"))[[7]]


allEst <- rbind(est1, est2, est3, 
                est4, est5[,1:5],
                est6[,1:5],  est7[,1:5],
                est8[,1:5],  est9[,1:5])

# allEst$model <- c(rep(paste("model", 1:8), each=129), rep("model 9", 128))
allEst$model <- rep(paste("model", 1:9), each=128)


allModPlot <- ggplot(data = allEst %>% filter(study.country %in% LMIC$study.country)) + 
    geom_point(aes(x=reorder(study.country, dstb_total_cost_country), y=dstb_total_cost_country, color = model), position = position_dodge(width=0.5)) + 
    geom_errorbar(aes(x=reorder(study.country, dstb_total_cost_country), ymin=.lower, ymax=.upper, color = model), position = position_dodge(width=0.5)) + 
    theme_minimal() + coord_flip() + theme(legend.position = "bottom")

ggsave(plot = allModPlot, filename = "~/Desktop/allModPlot.pdf", width = 11, height = 32.5, units = "in")

allModNoHospPlot <- ggplot(data = allEst %>% filter(model != "model 9")) + 
    geom_errorbar(aes(x=reorder(study.country, dstb_total_cost_country), ymin=.lower, ymax=.upper, color = model), position = position_dodge(width=1), alpha=.5) + 
    geom_point(aes(x=reorder(study.country, dstb_total_cost_country), y=dstb_total_cost_country, color = model), position = position_dodge(width=1)) + 
    theme_minimal() + coord_flip() + theme(legend.position = "bottom")

ggsave(plot = allModNoHospPlot, filename = "~/Desktop/allModNoHospPlot.pdf", width = 11, height = 32.5, units = "in")

ggplot(data = allEst %>% filter(study.country %in% LMIC$study.country[1:20])) + 
    geom_point(aes(x=study.country, y=dstb_total_cost_country, color = model), position = position_dodge(width=0.5)) + 
    theme_minimal() + coord_flip() + theme(legend.position = "bottom")

