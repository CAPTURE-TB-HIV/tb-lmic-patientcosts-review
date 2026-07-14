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
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
)

basicModEstTEST <- rstan::sampling(
    basicModel, basicModelData,
    chains = 2, cores = 4,
    warmup = tbPtntCstWarmup/2,
    iter = tbPtntCstIter/4,
    control=list(max_treedepth=14),
    refresh = 0, seed = tbPtntCstSeed
)

# basicModEstTEST<- stan(file = "code/inst/basicModel.stan",
#         data = basicModelData,
#         iter = tbPtntCstIter/4, chains = 2,
#         warmup = tbPtntCstWarmup/2,
#         control=list(max_treedepth=14), 
#         seed = tbPtntCstSeed)

# basicModelRes  <- get_posterior_mean(basicModEst)[,"mean-all chains"]
# 
# ### Create indexes for the models
# indexA <- which(names(basicModelRes) == "mu[1]"):which(names(basicModelRes) == "mu[439]")
# basicModelRes[indexA]
# 
# modelEstComp <- data.frame("study.country" = modelData$study.country,
#                            "study.end.year" = modelData$study.end.year,
#                            "who.survey" = modelData$whoSurv, 
#                            "pop.id" = modelData$pop.id, 
#                            "label" = paste(modelData$study.country, modelData$study.end.year),
#                            "basic.model" = basicModelRes[indexA],
#                            "cost.id" = 1:(nrow(modelData))#, 
#                            # dme.pretx = modelData$dme.prediag, 
#                            # dnme.pretx = modelData$dnme.prediag, 
#                            # ind.pretx = modelData$ie.prediag, 
#                            # dme.intsvtx = modelData$dme.intsvtx, 
#                            # dnme.intsvtx = modelData$dnme.intsvtx, 
#                            # ind.intsvtx = modelData$ie.intsvtx, 
#                            # dme.contntx = modelData$dme.contntx, 
#                            # dnme.contntx = modelData$dnme.contntx, 
#                            # ind.contntx = modelData$ie.contntx
#                            ) %>% 
#     mutate(pop.id = ifelse(pop.id %in% c("GenPopTotal", "wgtTotal"), "Total", pop.id),
#            who.survey = ifelse(who.survey==1, "Yes", "No"))
# 
# ggplot(data = modelEstComp) + 
#        geom_point(aes(x=label, y=`basic.model`, color = who.survey)) + 
#        theme_minimal() + 
#        facet_wrap(facets=vars(pop.id)) + coord_flip()



basicModSamps <- tidybayes::spread_draws(basicModEstTEST,
                                         dstb_total_cost_data[id.numb],
                                         ndraws = 1000, seed = tbPtntCstSeed);

newBasicModEst<- cbind(basicModSamps  %>%
                       group_by(id.numb) %>%
                       mean_qi(dstb_total_cost_data) %>%
                       select(! c(.width, .point, .interval)),
                       modelData %>% 
                           select(whoSurv, study.country, study.end.year, survey.id) %>%
                           distinct()) %>%
    mutate(who.survey = ifelse(whoSurv==1, "Yes", "No"))


ggplot(data=newBasicModEst) + 
       geom_point(aes(x=survey.id, y=`dstb_total_cost_data`, shape = who.survey, color=study.country), size=2) +
       theme_minimal() +
       coord_flip() + theme(legend.position = "none")  + 
    ggtitle("Survey-level estimates from basic model ")




basicModSampsC <- tidybayes::spread_draws(basicModEstTEST,
                                          dstb_total_cost_country[cnty.numb],
                                         ndraws = 1000, seed = tbPtntCstSeed);

newBasicModEstC<- cbind(basicModSampsC  %>%
                           group_by(cnty.numb) %>%
                           mean_qi(dstb_total_cost_country) %>%
                           select(! c(.width, .point, .interval)),
                       modelData %>% 
                           select(study.country) %>%
                           distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, study.country))


ggplot(data=newBasicModEstC) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from basic model")


basicModSampsC2 <- tidybayes::spread_draws(basicModEstTEST,
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
    ggtitle("Country-level estimates from basic model 3")



betas <- tidybayes::spread_draws(basicModEstTEST,
                                 beta_dme_prediag,
                                 beta_dnme_prediag,
                                 beta_ie_prediag,
                                 beta_dme_intsvtx,
                                 beta_dnme_intsvtx,
                                 beta_ie_intsvtx,
                                 beta_dme_contntx,
                                 beta_dnme_contntx,
                                 beta_ie_contntx,
                                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
    mean_qi()

sum(exp(betas[,seq(1,27,3)]))

##############################################################################|
##############################################################################|
##############################################################################|
##############################################################################|
### Run basic linear model 
##############################################################################|

# basicLinearModel <- stan_model("code/inst/basicLinearModel.stan") 
# 
# basicLinearModelData <- basicModelData 
# basicLinearModelData[["prior_sigma"]] <- round(exp(basicModelData[["prior_sigma"]]))
# 
# basicLinearModEst <- rstan::sampling(
#                     basicLinearModel, basicLinearModelData,
#                     chains = 2, cores = 4,
#                     warmup = tbPtntCstWarmup/2,
#                     iter = tbPtntCstIter/4,
#                     control=list(max_treedepth=14),
#                     refresh = 0, seed = tbPtntCstSeed
# )
# 
# stan(file = "code/inst/basicLinearModel.stan",
#      data = basicLinearModelData,
#      iter = tbPtntCstIter/4, chains = 2,
#      warmup=tbPtntCstWarmup/2,
#      control=list(max_treedepth=14), 
#      seed = tbPtntCstSeed)
# 
# 
# basicLinearModSamps <- tidybayes::spread_draws(basicLinearModEst,
#                                                dstb_total_cost_data[id.numb],
#                                                ndraws = 1000, seed = tbPtntCstSeed);
# 
# basicLinearModEst<- cbind(basicLinearModSamps  %>%
#                           group_by(id.numb) %>%
#                            mean_qi(dstb_total_cost_data) %>%
#                            select(! c(.width, .point, .interval)),
#                        modelData %>% 
#                            select(whoSurv, study.country, study.end.year, survey.id) %>%
#                            distinct()) %>%
#     mutate(who.survey = ifelse(whoSurv==1, "Yes", "No"))
# 
# 
# ggplot(data=basicLinearModEst) + 
#     geom_point(aes(x=survey.id, y=`dstb_total_cost_data`, shape = who.survey, color=study.country), size=2) +
#     theme_minimal() +
#     coord_flip() + theme(legend.position = "none")  + 
#     ggtitle("Survey-level estimates from basic linear model ")
# 
# 
# 
# basicLinearModSampsCntry <- tidybayes::spread_draws(basicLinearModEst,
#                                            dstb_total_cost_country[country.id],
#                                            ndraws = 1000, seed = tbPtntCstSeed);
# 
# basicLinearModEstCntry<- left_join(basicLinearModSampsCntry  %>%
#                                  group_by(country.id) %>%
#                                  mean_qi(dstb_total_cost_country) %>%
#                                  select(! c(.width, .point, .interval)),
#                              modelData %>% select(study.country, country.id) %>% distinct()) %>% 
#     arrange(dstb_total_cost_country) %>% 
#     mutate(study.country=factor(study.country, levels=study.country))
# 
# ggplot(data=basicLinearModEstCntry) + 
#     geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
#     theme_minimal() +
#     coord_flip() + theme(legend.position = "none") + 
#     ggtitle("Country-level estimates from basic linear model")



##############################################################################|
##############################################################################|
##############################################################################|
##############################################################################|
### Run basic models in R
##############################################################################|
##############################################################################|

library(lme4)

### Use LM to run basic linear model with no random effects in R
basicLinearModel <- lm(cost ~ 0 + dme.prediag + dnme.prediag + ie.prediag +
                                  dme.intsvtx + dnme.intsvtx + ie.intsvtx + 
                                  dme.contntx + dnme.contntx + ie.contntx + drtb.wt , 
                                  data=modelData  %>% filter(cost > 0)); basicLinearModel

### DSTB estimates 
sum(basicLinearModel$coefficients[-10])
### DRTB estimates 
sum(basicLinearModel$coefficients)

### Use LMER to run basic linear model with random effects in R
basicLinearMixedModel <-lmer(cost ~ 0 + dme.prediag + dnme.prediag + ie.prediag +
                        dme.intsvtx + dnme.intsvtx + ie.intsvtx + 
                        dme.contntx + dnme.contntx + ie.contntx + drtb.wt + 
                        (1 | country.id) + (1 |survey.id), 
                        data=modelData  %>% filter(cost > 0)); basicLinearMixedModel

basicLinearMixedModel <-lmer(cost ~ 0 + dme.prediag + dnme.prediag + ie.prediag +
                                 dme.intsvtx + dnme.intsvtx + ie.intsvtx + 
                                 dme.contntx + dnme.contntx + ie.contntx + drtb.wt + 
                                 (1 | country.id) + (1 |survey.id), 
                             data=modelData  %>% filter(cost > 0)); basicLinearMixedModel

### DSTB estimates 
sum(summary(basicLinearMixedModel)[["coefficients"]][-10,1])
### DRTB estimates 
sum(summary(basicLinearMixedModel)[["coefficients"]][,1])


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

### Bayes model 

betas <- tidybayes::spread_draws(basicModEstTEST,
                                 beta_dme_prediag,
                                 beta_dnme_prediag,
                                 beta_ie_prediag,
                                 beta_dme_intsvtx,
                                 beta_dnme_intsvtx,
                                 beta_ie_intsvtx,
                                 beta_dme_contntx,
                                 beta_dnme_contntx,
                                 beta_ie_contntx,
                                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
    mean_qi()

sum(exp(betas[,seq(1,27,3)]))