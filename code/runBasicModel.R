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
basicModel <- stan_model("code/inst/basicModel.stan")

### Data used to fit the model 
source(here("code/modelDataFormat.R"))

### All LMICs in 2025
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
    rename(study.country = country.name) %>%
    select(! class25)

# modelData <- modelData  %>% filter(cost > 0)
basicModelData <- list(
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
    id_matrix = modelData %>% select(survey.id, country.id) %>% distinct(), 
    country_id = modelData$country.id,
    ### Internal covariate
    drtb_ind= modelData$drtb.wt,
    survey_year = ((modelData$study.end.year - mean(modelData$study.end.year)) / sd(modelData$study.end.year)),
    ### External covariateFilt
    median_income = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income)),
    oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
    uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
    ### Prediction data 
    N_country_new = length(modelData$cost),
    review_ind = rep(1, length(modelData$cost)),
    in_review_id = modelData$country.id,
    pred_year = (2024 - mean(modelData$study.end.year)) / sd(modelData$study.end.year), 
    median_income_new = ((modelData$log.median.income - mean(modelData$log.median.income)) / sd(modelData$log.median.income)),
    oop_perc_che_new = (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
    uhc_index_new = (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
    prior_sigma = 2, 
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData),
    natrep_id = modelData$whoSurv
)

basicModEst <- rstan::sampling(
    basicModel, basicModelData,
    chains = 2, cores = 4,
    warmup = tbPtntCstWarmup/2,
    iter = tbPtntCstIter/4,
    control=list(max_treedepth=14),
    refresh = 0, seed = tbPtntCstSeed
)

basicModSamps <- tidybayes::spread_draws(basicModEst,
                                         dstb_total_cost_data[id.numb],
                                         ndraws = 1000, seed = tbPtntCstSeed);

newBasicModEst<- cbind(basicModSamps  %>%
                       group_by(id.numb) %>%
                       mean_qi(dstb_total_cost_data) %>%
                       select(! c(.width, .point, .interval)),
                       modelData %>% 
                           select(whoSurv, study.country, study.end.year, survey.id, median.income) %>%
                           distinct()) %>%
    mutate(who.survey = ifelse(whoSurv==1, "Yes", "No"))

ggplot(data=newBasicModEst) + 
       geom_point(aes(x=survey.id, y=`dstb_total_cost_data`, shape = who.survey, color=study.country), size=2) +
       theme_minimal() +
       coord_flip() + 
    theme(legend.position = "none")  +
    # ggpubr::ggtext(aes(x=survey.id, y=`dstb_total_cost_data`, label = study.county)) + 
    ggtitle("Survey-level estimates from basic model ")


ggplot() +
    geom_point(data = newBasicModEst %>% 
                   select(study.country, study.end.year, median.income, survey.id, dstb_total_cost_data), aes(x=dstb_total_cost_data, y=median.income))  + 
    geom_point(data= newBasicModEst %>% select(study.country, study.end.year, median.income, survey.id, dstb_total_cost_data) %>% 
                   filter(study.country == "China"), aes(x=dstb_total_cost_data, y=median.income), color = "red")  + 
    theme_minimal() + coord_flip()


basicModSampsC2 <- tidybayes::spread_draws(basicModEst,
                                          dstb_total_cost_country[country.id],
                                          ndraws = 1000, seed = tbPtntCstSeed);

newBasicModEstC2<- left_join(basicModSampsC2  %>%
                            group_by(country.id) %>%
                            mean_qi(dstb_total_cost_country) %>%
                            select(! c(.width, .point, .interval)),
                        modelData %>% select(study.country, country.id) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country))

ggplot(data=newBasicModEstC2) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from basic model")

##############################################################################|
##############################################################################|

betas<- tidybayes::spread_draws(basicModEst,
                                 beta_dme_prediag,
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
                                 mult_who,
                                 ndraws = 1000, seed = tbPtntCstSeed) 
betas %>% 
    mutate(totalDS = (exp(beta_dme_prediag) +
                          exp(beta_dnme_prediag) +
                          exp(beta_ie_prediag) +
                          exp(beta_dme_intsvtx) +
                          exp(beta_dnme_intsvtx) +
                          exp(beta_ie_intsvtx) +
                          exp(beta_dme_contntx) +
                          exp(beta_dnme_contntx) +
                          exp(beta_ie_contntx)), 
           totalDSWHO = (exp(beta_dme_prediag) +
                          exp(beta_dnme_prediag) +
                          exp(beta_ie_prediag) +
                          exp(beta_dme_intsvtx) +
                          exp(beta_dnme_intsvtx) +
                          exp(beta_ie_intsvtx) +
                          exp(beta_dme_contntx) +
                          exp(beta_dnme_contntx) +
                          exp(beta_ie_contntx)) * exp(mult_who),
           totalDR = (exp(beta_dme_prediag + mult_drtb_dme_prediag) + 
                          exp(beta_dnme_prediag + mult_drtb_dnme_prediag) +
                          exp(beta_ie_prediag + mult_drtb_ie_prediag) +
                          exp(beta_dme_intsvtx + mult_drtb_dme_intsvtx) +
                          exp(beta_dnme_intsvtx + mult_drtb_dnme_intsvtx) +
                          exp(beta_ie_intsvtx + mult_drtb_ie_intsvtx) +
                          exp(beta_dme_contntx + mult_drtb_dme_contntx) +
                          exp(beta_dnme_contntx + mult_drtb_dnme_contntx) +
                          exp(beta_ie_contntx + mult_drtb_ie_contntx)),
           totalDRWHO = (exp(beta_dme_prediag + mult_drtb_dme_prediag) + 
                          exp(beta_dnme_prediag + mult_drtb_dnme_prediag) +
                          exp(beta_ie_prediag + mult_drtb_ie_prediag) +
                          exp(beta_dme_intsvtx + mult_drtb_dme_intsvtx) +
                          exp(beta_dnme_intsvtx + mult_drtb_dnme_intsvtx) +
                          exp(beta_ie_intsvtx + mult_drtb_ie_intsvtx) +
                          exp(beta_dme_contntx + mult_drtb_dme_contntx) +
                          exp(beta_dnme_contntx + mult_drtb_dnme_contntx) +
                          exp(beta_ie_contntx + mult_drtb_ie_contntx)) * exp(mult_who)) %>% 
    mean_qi(totalDS, totalDSWHO, totalDR, totalDRWHO) %>% t()


betas %>% mean_qi(mult_who) %>% select(! c(.width, .point, .interval)) %>% exp() %>% t()

##############################################################################|
##############################################################################|
##############################################################################|
##############################################################################|
### Run basic models in R
##############################################################################|
##############################################################################|

library(lme4)

### Use LM to run basic linear model with no random effects in R
basicLinearModel <- lm(cost ~ 0 + dme.prediag * drtb.wt + 
                                  dnme.prediag * drtb.wt + 
                                  ie.prediag * drtb.wt+
                                  dme.intsvtx * drtb.wt + 
                                  dnme.intsvtx * drtb.wt + 
                                  ie.intsvtx * drtb.wt + 
                                  dme.contntx * drtb.wt + 
                                  dnme.contntx * drtb.wt + 
                                  ie.contntx * drtb.wt, 
                                  data=modelData  %>% filter(cost > 0)); basicLinearModel

### DSTB estimates 
sum(basicLinearModel$coefficients[c(1,3:10)])
### DRTB estimates 
sum(basicLinearModel$coefficients)

basicLinearModelWHO <- lm(cost ~ 0 + dme.prediag * drtb.wt + 
                           dnme.prediag * drtb.wt + 
                           ie.prediag * drtb.wt+
                           dme.intsvtx * drtb.wt + 
                           dnme.intsvtx * drtb.wt + 
                           ie.intsvtx * drtb.wt + 
                           dme.contntx * drtb.wt + 
                           dnme.contntx * drtb.wt + 
                           ie.contntx * drtb.wt + 
                           whoSurv, 
                       data=modelData  %>% filter(cost > 0) %>% 
                       mutate(whoSurv = as.factor(whoSurv))); basicLinearModelWHO

### DSTB estimates 
sum(basicLinearModelWHO$coefficients[c(1,3:11)])
sum(basicLinearModelWHO$coefficients[c(1,3:10, 12)])
### DRTB estimates 
sum(basicLinearModelWHO$coefficients[-12])
sum(basicLinearModelWHO$coefficients[-11])

### Use LMER to run basic linear model with random effects in R
basicLinearMixedModel <-lmer(cost ~ 0 + dme.prediag * drtb.wt + 
                                        dnme.prediag * drtb.wt + 
                                        ie.prediag * drtb.wt+
                                        dme.intsvtx * drtb.wt + 
                                        dnme.intsvtx * drtb.wt + 
                                        ie.intsvtx * drtb.wt + 
                                        dme.contntx * drtb.wt + 
                                        dnme.contntx * drtb.wt + 
                                        ie.contntx * drtb.wt + 
                                       (1 | country.id) + (1 |survey.id), 
                              data=modelData  %>% 
                                   filter(cost > 0)); basicLinearMixedModel

### DSTB estimates 
sum(summary(basicLinearMixedModel)[["coefficients"]][c(1,3:10),1])
### DRTB estimates 
sum(summary(basicLinearMixedModel)[["coefficients"]][,1])

basicLinearMixedModelWHO <-lmer(cost ~ 0 + dme.prediag * drtb.wt + 
                                 dnme.prediag * drtb.wt + 
                                 ie.prediag * drtb.wt+
                                 dme.intsvtx * drtb.wt + 
                                 dnme.intsvtx * drtb.wt + 
                                 ie.intsvtx * drtb.wt + 
                                 dme.contntx * drtb.wt + 
                                 dnme.contntx * drtb.wt + 
                                 ie.contntx * drtb.wt + 
                                 whoSurv +
                                 (1 | country.id) + (1 |survey.id), 
                                 data=modelData  %>% filter(cost > 0) %>% 
                                      mutate(whoSurv = as.factor(whoSurv))); basicLinearMixedModelWHO

### DSTB estimates 
sum(summary(basicLinearMixedModelWHO)[["coefficients"]][c(1,3:11),1])
sum(summary(basicLinearMixedModelWHO)[["coefficients"]][c(1,3:10, 12),1])
### DRTB estimates 
sum(summary(basicLinearMixedModelWHO)[["coefficients"]][-12,1])
sum(summary(basicLinearMixedModelWHO)[["coefficients"]][-11,1])


### Use GLMER to run basic linear model with random effects in R 
### using gamma family 
basicGammaMixedModel <-glmer(cost ~ 0 + dme.prediag + dnme.prediag + ie.prediag +
          dme.intsvtx + dnme.intsvtx + ie.intsvtx + 
          dme.contntx + dnme.contntx + ie.contntx + drtb.wt + 
          (1 | country.id) + (1 |survey.id), 
      data=modelData %>% filter(cost > 0), family = Gamma(link = "identity")); basicGammaMixedModel

### DSTB estimates 
sum(summary(basicGammaMixedModel)[["coefficients"]][-10,1])
### DRTB estimates 
sum(summary(basicGammaMixedModel)[["coefficients"]][,1]) 

### Use GLMER to run basic linear model with random effects in R 
### using gamma family 
basicGammaMixedModelLL <-glmer(cost ~ 0 + dme.prediag + dnme.prediag + ie.prediag +
                                          dme.intsvtx + dnme.intsvtx + ie.intsvtx + 
                                          dme.contntx + dnme.contntx + ie.contntx + drtb.wt + 
                                         (1 | country.id) + (1 | survey.id), 
                             data=modelData %>% filter(cost > 0), family = Gamma(link = "log")); basicGammaMixedModelLL

prod(exp(summary(basicGammaMixedModelLL)[["coefficients"]][-10,1]))
prod(exp(summary(basicGammaMixedModelLL)[["coefficients"]][,1]))

### Use GLMER to run basic linear model with random effects in R 
### using gamma family 
basicGammaMixedModelLL2 <-glmer(cost ~ 0 + 
                                      (dme.prediag +  (1 | country.id) + (1 | survey.id) + drtb.wt) + 
                                      (dnme.prediag + (1 | country.id) + (1 | survey.id) + drtb.wt) +
                                      (ie.prediag + (1 | country.id) + (1 | survey.id) + drtb.wt) +
                                      (dme.intsvtx +  (1 | country.id) + (1 | survey.id) + drtb.wt) + 
                                      (dnme.intsvtx + (1 | country.id) + (1 | survey.id) + drtb.wt) +
                                      (ie.intsvtx + (1 | country.id) + (1 | survey.id) + drtb.wt) +
                                      (dme.contntx +  (1 | country.id) + (1 | survey.id) + drtb.wt) + 
                                      (dnme.contntx + (1 | country.id) + (1 | survey.id) + drtb.wt) +
                                      (ie.contntx + (1 | country.id) + (1 | survey.id) + drtb.wt) , 
                               data=modelData %>% filter(cost > 0), family = Gamma(link = "log")); basicGammaMixedModelLL2

prod(exp(summary(basicGammaMixedModelLL2)[["coefficients"]][-10,1]))
prod(exp(summary(basicGammaMixedModelLL2)[["coefficients"]][,1]))

