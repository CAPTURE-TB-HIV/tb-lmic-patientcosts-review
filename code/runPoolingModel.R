###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(rstan)
###############################################################################
##### Source in the formatted model data                                  #####
###############################################################################
source(here("code/modelDataFormat.R"))
                       
##############################################################################|
##### Set seed, number of iterations (warmup and total)                   #####
##### for sampling in model fits.                                         ####|
##### Needs to be the same in the runPoolingModel.R script.               ####|
##############################################################################|

tbPtntCstSeed <- 513
tbPtntCstIter <- 4000
tbPtntCstWarmup <- 500

###############################################################################
### We're going to test quite a few different model specifications below    ###
### We'll need to format the data for stan, then call the models.           ###
###############################################################################

###############################################################################
### Covariates have the a common effect and a random effect across cost category
### Add the minimum value to all zero costs (just to get model up and running)
###############################################################################
### Format the data for stan                                                ###
###############################################################################
costVec <- modelData$cost 
costVec[which(costVec==0)] <- min(costVec[-which(costVec==0)])
stanModelDataFloor <- list(
                        N = length(modelData$cost),
                        cost = costVec,
                        se = rep(0, length(modelData$cost)),
                        # se = as.numeric(modelData$se)+ 0.01,
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
                        median_income = ((modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income)),
                        oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
                        uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex),
                        prior_sigma = 2,
                        ### test with world region shut off , 
                        beta_exp =1,
                        world_region = rep(0, length(modelData$cost)), 
                        holdout =  rep(0, length(modelData$cost))
                    )
###############################################################################
##### Call the stan models                                                #####
###############################################################################

totalModelFloor <- stan(file = "code/inst/tbPatientCosts.stan",
                         data = stanModelDataFloor,
                         iter = tbPtntCstIter, chains = 3,
                         warmup=tbPtntCstWarmup,
                         control=list(max_treedepth=14), 
                         seed = tbPtntCstSeed)

### Pooled patient costs as mean median income and oop perc and study year
looTotal <- rstan::loo(totalModelFloor)
exp(summary(totalModelFloor)[[1]][1:9,"mean"])
sum(exp(summary(totalModelFloor)[[1]][1:9,"mean"]))
# exp(summary(totalModelFloor)[[1]][1:9,"50%"])
# exp(summary(totalModelFloor)[[1]][1:9,"2.5%"])
# exp(summary(totalModelFloor)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
# costCategories <- paste(c(rep("dme",3), rep("dnme",3), rep("ie",3)), 
#                         c("prediag", "intsvtx", "contntx"), sep="_")
# yearCoeffs <- paste0("beta_year_", costCategories)
# exp(summary(totalModelFloor)[[1]][yearCoeffs,"mean"])
# 
# ### Print the estimated impact of drtb
# drtbCoeffs <- paste0("beta_drtb_", costCategories)
# exp(summary(totalModelFloor)[[1]][drtbCoeffs,"mean"])


###############################################################################
### Covariates have the a common effect and a random effect across cost category
### Hurdle model 
###############################################################################
### Format the data for stan                                                ###
###############################################################################
stanModelDataZeros <- list(
                            N = length(modelData$cost),
                            cost = modelData$cost,
                            se = rep(0, length(modelData$cost)),
                            # se = as.numeric(modelData$se)+ 0.01,
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
                            survey_year = (modelData$study.end.year - mean(modelData$study.end.year)) / sd(modelData$study.end.year),
                            ### External covariates
                            median_income = (modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income),
                            oop_perc_che =  (modelData$oop.perc.che   - mean(modelData$oop.perc.che))  / sd(modelData$oop.perc.che), 
                            uhc_index =     (modelData$UHCindex       - mean(modelData$UHCindex))      / sd(modelData$UHCindex),
                            prior_sigma = 2,
                            beta_exp = 1,
                            world_region = rep(0, length(modelData$cost)), 
                            holdout =  rep(0, length(modelData$cost)), 
                            prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
)


totalModelHurdle <- stan(file = "code/inst/tbPatientCosts-hurdle.stan",
                   data = stanModelDataZeros,
                   iter = tbPtntCstIter/4, chains = 3,
                   warmup = tbPtntCstWarmup/2,
                   control=list(max_treedepth=14), 
                   seed = tbPtntCstSeed)

looHurdle <- loo(totalModelHurdle)

# totalModelTimeOnly <- stan(file = "code/inst/tbPatientCosts-TimeOnly.stan",
#                            data = stanModelDataZeros,
#                            iter = tbPtntCstIter, chains = 3,
#                            warmup=tbPtntCstWarmup,
#                            control=list(max_treedepth=14), 
#                            seed = tbPtntCstSeed)
# 
# totalModelTimeaCovs <- stan(file = "code/inst/tbPatientCosts-allCovs-Time.stan",
#                            data = stanModelDataZeros,
#                            iter = tbPtntCstIter, chains = 3,
#                            warmup=tbPtntCstWarmup,
#                            control=list(max_treedepth=14), 
#                            seed = tbPtntCstSeed)



###############################################################################
### Covariates have the a common effect and a random effect across cost category
### Run with zeroes omitted as it should closely approximate the hurdle 
### conditional costs. 
###############################################################################
### Format the data for stan                                                ###
###############################################################################

# modelDataNoZeros <- modelData %>% filter(cost !=0)
# stanModelDataNoZeros <- list(
#                             N = length(modelDataNoZeros$cost),
#                             cost = modelDataNoZeros$cost,
#                             se = rep(0, length(modelDataNoZeros$cost)),
#                             dme_prediag = modelDataNoZeros$dme.prediag, 
#                             dnme_prediag =  modelDataNoZeros$dnme.prediag, 
#                             ie_prediag =  modelDataNoZeros$ie.prediag, 
#                             dme_intsvtx = modelDataNoZeros$dme.intsvtx, 
#                             dnme_intsvtx =  modelDataNoZeros$dnme.intsvtx, 
#                             ie_intsvtx =  modelDataNoZeros$ie.intsvtx,
#                             dme_contntx = modelDataNoZeros$dme.contntx, 
#                             dnme_contntx =  modelDataNoZeros$dnme.contntx, 
#                             ie_contntx =  modelDataNoZeros$ie.contntx, 
#                             N_survey = max(modelDataNoZeros$survey.id),
#                             survey_id = modelDataNoZeros$survey.id, 
#                             N_country = max(modelDataNoZeros$country.id),
#                             country_id = modelDataNoZeros$country.id, 
#                             ### Internal covariate
#                             drtb_ind= modelDataNoZeros$drtb.wt,
#                             survey_year = modelDataNoZeros$year_z,
#                             ### External covariates
#                             median_income = ((modelDataNoZeros$median.income - mean(modelDataNoZeros$median.income)) / sd(modelDataNoZeros$median.income)),
#                             oop_perc_che =  (modelDataNoZeros$oop.perc.che - mean(modelDataNoZeros$oop.perc.che))/sd(modelDataNoZeros$oop.perc.che), 
#                             uhc_index =     (modelDataNoZeros$UHCindex - mean(modelDataNoZeros$UHCindex)) / sd(modelDataNoZeros$UHCindex),
#                             prior_sigma = 2,
#                             ### test with world region shut off , 
#                             beta_exp =1,
#                             world_region = rep(0, length(modelDataNoZeros$cost)), 
#                             holdout =  rep(0, length(modelDataNoZeros$cost))
# )
# 
# totalModelNoZero <- stan(file = "code/inst/tbPatientCosts.stan",
#                    data = stanModelDataNoZeros,
#                    iter = tbPtntCstIter, chains = 3,
#                    warmup=tbPtntCstWarmup,
#                    control=list(max_treedepth=14), 
#                    seed = tbPtntCstSeed)

# looTotal <- rstan::loo(totalModelNoZero)

###############################################################################
### Covariates have the same effect across cost category
###############################################################################
# simpModelFloor <- stan(file = "code/inst/tbPatientCosts-simpCov.stan",
#                    data = stanModelDataFloor,
#                    iter = tbPtntCstIter/4, chains = 2,
#                    warmup= tbPtntCstWarmup/2,
#                    control=list(max_treedepth=14, adapt_delta = 0.99), 
#                    seed = tbPtntCstSeed)

stanData5[["holdout"]] <- rep(0,nrow(modelData))

simpModelHurdle <- stan(file = "code/inst/tbPatientCosts-simpCov-hurdle.stan",
                        data = stanData5,
                        iter = tbPtntCstIter/4, chains = 3,
                        warmup=tbPtntCstWarmup/2,
                        control=list(max_treedepth=14, adapt_delta = 0.99),
                        seed = tbPtntCstSeed)

# saveRDS(simpModelHurdle, "~/Desktop/simpHurdle.rds", version =2)

coefSH <- as.data.frame(simpModelHurdle) %>% select(beta_dme_prediag,
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


colnames(coefSH) <- gsub("mult_year", "yearMult", colnames(coefSH), fixed=TRUE)
colnames(coefSH) <- gsub("mult_drtb", "drtbMult", colnames(coefSH), fixed=TRUE)
colnames(coefSH) <- gsub("mult_med_income", "medIncMult", colnames(coefSH), fixed=TRUE)
colnames(coefSH) <- gsub("mult_oop_perc_che", "oopMult", colnames(coefSH), fixed=TRUE)
colnames(coefSH) <- gsub("mult_uhc", "uhcMult", colnames(coefSH), fixed=TRUE)

colnames(coefSH) <- gsub(".", "_", colnames(coefSH), fixed=TRUE)

coefSH <- coefSH %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

View(coefSH)

costCntrySH <- tidybayes::spread_draws(simpModelHurdle,
                                         dstb_total_cost_country[country.id.new],
                                         ndraws = 1000, seed = tbPtntCstSeed);

costCntryEstSH <- left_join(costCntrySH  %>%
                                  group_by(country.id.new) %>%
                                  mean_qi(dstb_total_cost_country) %>%
                                  select(! c(.width, .point, .interval)),
                              covariateFilt24 %>% select(study.country, country.id.new) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country))

ggplot(data=costCntryEstSH) + 
    geom_point(aes(x=study.country, y=`dstb_total_cost_country`, color=study.country)) +
    geom_errorbar(aes(x=study.country, ymax=.upper, ymin=.lower, color=study.country)) + 
    theme_minimal() +
    coord_flip() + theme(legend.position = "none") + 
    ggtitle("Country-level estimates from model with hh international dollars") + 
    expand_limits(y=c(0,50000))

costCntrySH %>% ungroup() %>% mean_qi(dstb_total_cost_country)

# looSimp <- loo(simpModelFloor)
### Pooled patient costs as mean median income and oop perc and study year
# exp(summary(simpModelFloor)[[1]][1:9,"mean"])
# exp(summary(totalModel)[[1]][1:9,"50%"])
# exp(summary(totalModel)[[1]][1:9,"2.5%"])
# exp(summary(totalModel)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
# exp(summary(simpModelFloor)[[1]]["beta_year","mean"])

### Print the estimated impact of drtb
# exp(summary(simpModelFloor)[[1]]["beta_drtb","mean"])


###############################################################################
### Covariates have the a common effect and a random effect across cost category
###############################################################################
# reCovModel <- stan(file = "code/inst/tbPatientCosts-randEffCov.stan", 
#                    data = stanModelData,
#                    iter = tbPtntCstIter/4, chains = 2,
#                    warmup=tbPtntCstWarmup/2,
#                    control=list(max_treedepth=14),
#                    seed = tbPtntCstSeed)

# looReCov <-rstan::loo(reCovModel) 

# sum(exp(summary(reCovModel)[[1]][1:9,"mean"]))
# exp(summary(reCovModel)[[1]][1:9,"mean"])
# 
# loo_compare(rstan::loo(totalModel), rstan::loo(reCovModel))
# 
# reCovModelNoUnit <- reCovModel
# reCovModelUnit <- reCovModel

###############################################################################
### Compare with LOO
###############################################################################
### First check that pointwise log likelihood approximates that stan outputs
### for the total log likelihood 

# logLiks <- paste0("log_lik[", 1:nrow(modelDataFloor), "]")
# sum(summary(totalModelFloor)[[1]][logLiks,"mean"])
# summary(totalModelFloor)[[1]]["lp__","mean"]
# 
# sum(summary(simpModelFloor)[[1]][logLiks,"mean"])
# summary(simpModelFloor)[[1]]["lp__","mean"]


# looTotalN5 <- rstan::loo(totalN5Model)
# looReCov <- rstan::loo(reCovModel)
# looSimpN5 <- rstan::loo(simpCovN5Mod)

# loo_compare(looTotal, looReCov, looSimp)

# modelList5 <- list("Data" = modelData, "9covs" = totalN5Model, 
#                    "9covsRE" = reCovN5Model, "1cov" =simpCovN5Mod)
# looList10 <- list(looTotal, looReCov, looSimp)
# saveRDS(modelList5, here("output/modelswithunitnormal5.rds"), version=2)


###############################################################################
##### Do WHO region fixed effects improve model? 
###############################################################################

regModelData <- stanModelDataZeros
regModelData[["world_region"]] <- as.numeric(modelData$region.code) - 1

reg_reCovModel <- stan(file = "code/inst/tbPatientCosts-hurdle-region.stan", 
                       data = regModelData,
                       iter = tbPtntCstIter/4, chains = 2,
                       warmup=tbPtntCstWarmup/2,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)

looHurdleRegion <-rstan::loo(reg_reCovModel) 

loo::loo_compare(looHurdle, looHurdleRegion)


###############################################################################
### Only fixed effect for who surveys
###############################################################################
whoModelData <- stanModelDataZeros
whoModelData[["natrep_id"]] <- modelData$whoSurv

whoHurdleModel <- stan(file = "code/inst/tbPatientCosts-hurdle-fixedNatRep.stan", 
                       data = whoModelData,
                       iter = tbPtntCstIter/4, chains = 3,
                       warmup=tbPtntCstWarmup/2,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)

looWhoHurdleMod <-rstan::loo(whoHurdleModel) 

loo::loo_compare(looHurdle, looWhoHurdleMod)


###############################################################################
### fixed and rand effect sigma for who surveys
###############################################################################
whoModel2Data <- stanModelDataZeros
whoModel2Data[["natrep_id"]] <- modelData$whoSurv

whoHurdle2Model <- stan(file = "code/inst/tbPatientCosts-hurdle-bothNatRep.stan", 
                       data = whoModelData,
                       iter = tbPtntCstIter/4, chains = 3,
                       warmup=tbPtntCstWarmup/2,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)

looWhoHurdle2Mod <-rstan::loo(whoHurdle2Model) 

loo::loo_compare(looHurdle, looWhoHurdleMod, looWhoHurdle2Mod)


###############################################################################
##### We use hurdle model. Now check if the addition of different random 
##### effects between nationally representative and non-nat rep surveys helps
###############################################################################

modelData2 <- left_join(modelData, 
                        dataInds %>% select(covidence.id, natRep)) %>% 
              ungroup() %>%
              group_by(natRep) %>%
              mutate(natrep.id = cur_group_id())

stanModelDataZerosAddRE <- list(
    N = length(modelData2$cost),
    cost = modelData2$cost,
    se = rep(0, length(modelData2$cost)),
    # se = as.numeric(modelData2$se)+ 0.01,
    dme_prediag = modelData2$dme.prediag, 
    dnme_prediag =  modelData2$dnme.prediag, 
    ie_prediag =  modelData2$ie.prediag, 
    dme_intsvtx = modelData2$dme.intsvtx, 
    dnme_intsvtx =  modelData2$dnme.intsvtx, 
    ie_intsvtx =  modelData2$ie.intsvtx,
    dme_contntx = modelData2$dme.contntx, 
    dnme_contntx =  modelData2$dnme.contntx, 
    ie_contntx =  modelData2$ie.contntx, 
    N_survey = max(modelData2$survey.id),
    survey_id = modelData2$survey.id, 
    N_country = max(modelData2$country.id),
    country_id = modelData2$country.id, 
    N_natrep = max(modelData2$natrep.id),
    natrep_id = modelData2$natrep.id, 
    ### Internal covariate
    drtb_ind= modelData2$drtb.wt,
    survey_year = (modelData2$study.end.year - mean(modelData2$study.end.year)) / sd(modelData2$study.end.year),
    ### External covariates
    median_income = (modelData2$median.income - mean(modelData2$median.income)) / sd(modelData2$median.income),
    oop_perc_che =  (modelData2$oop.perc.che   - mean(modelData2$oop.perc.che))  / sd(modelData2$oop.perc.che), 
    uhc_index =     (modelData2$UHCindex       - mean(modelData2$UHCindex))      / sd(modelData2$UHCindex),
    prior_sigma = 2,
    beta_exp = 1,
    world_region = rep(0, length(modelData2$cost)), 
    holdout =  rep(0, length(modelData2$cost)), 
    prob_cost_zero = sum(modelData2$cost == 0) / nrow(modelData2)
)

totalModelHurdleRE <- stan(file = "code/inst/tbPatientCosts-hurdle-addRandEff.stan",
                           data = stanModelDataZerosAddRE,
                           iter = tbPtntCstIter, chains = 3,
                           warmup = tbPtntCstWarmup,
                           control=list(max_treedepth=14), 
                           seed = tbPtntCstSeed)

###############################################################################
##### Compare with loo  
###############################################################################

looHurdleRE <- loo(totalModelHurdleRE)
loo::loo_compare(looHurdle, looHurdleRE) 










###############################################################################
##### Check whether squared betas are an improvement 
###############################################################################
stanModelDataSqrd <- list(
    N = length(modelData$cost),
    cost = modelData$cost + 1,
    se = rep(0, length(modelData$cost)),
    # se = as.numeric(modelData$se)+ 0.01,
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
    survey_year = modelData$year_z,
    ### External covariates
    median_income = ((modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income)),
    oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$oop.perc.che), 
    uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex),
    prior_sigma = 2, 
    beta_exp = 2
)

totalModelSqrd <- stan(file = "code/inst/tbPatientCosts.stan",
                   data = stanModelDataSqrd,
                   iter = tbPtntCstIter/4, chains = 2,
                   warmup=tbPtntCstWarmup/2,
                   control=list(max_treedepth=14), 
                   seed = tbPtntCstSeed)

looTotalSqrd <- rstan::loo(totalModelSqrd)


reCovModelSqrd <- stan(file = "code/inst/tbPatientCosts-randEffCov.stan", 
                       data = stanModelDataSqrd,
                       iter = tbPtntCstIter/4, chains = 2,
                       warmup=tbPtntCstWarmup/2,
                       control=list(max_treedepth=14), 
                       seed = tbPtntCstSeed)

looReCovSqrd <- rstan::loo(reCovModelSqrd)


simpModelFloorSqrd <- stan(file = "code/inst/tbPatientCosts-simpCov.stan",
                   data = stanModelDataSqrd,
                   iter = tbPtntCstIter/4, chains = 2,
                   warmup= tbPtntCstWarmup/2,
                   control=list(max_treedepth=14), 
                   seed = tbPtntCstSeed)

looSimpSqrd <- loo(simpModelFloorSqrd)


###############################################################################
##### Check different priors for the two more complex models 
###############################################################################
totModList <- list() 
reModList <- list()

for (i in c(2,4,6)){
    
    tmpModelData <- stanModelDataZeros
    tmpModelData[["prior_sigma"]] <- i
    
    totModList[[i]] <- stan(file = "code/inst/tbPatientCosts-hurdle.stan",
                       data = tmpModelData,
                       iter = tbPtntCstIter/2, chains = 3,
                       warmup=tbPtntCstWarmup,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)
    
    # reModList[[i]] <- stan(file = "code/inst/tbPatientCosts-randEffCov.stan", 
    #                    data = tmpModelData, 
    #                    iter = tbPtntCstIter, chains = 3,
    #                    warmup=tbPtntCstWarmup,
    #                    control=list(max_treedepth=14),
    #                    seed = tbPtntCstSeed)
}


loo_compare(loo(totModList[[2]]), loo(totModList[[4]]),  loo(totModList[[6]]))
            # loo(reModList[[2]]), loo(reModList[[4]]))

### Random effects model with prior sigma = 2 is best performance

bestModel <- list("DataList" = stanModelData, 
                  "Dataframe" = modelData, 
                  "Model" = reModList[[2]])
saveRDS(bestModel, here("output/bestModel.rds"), version=2)


sum(exp(summary(reModList[[2]])[[1]][1:9,"mean"]))
exp(summary(reModList[[2]])[[1]][1:9,"mean"])


# listGood <-list(totModList[[2]], totModList[[4]], 
#                 reModList[[2]], reModList[[4]])
# saveRDS(listGood, here("output/modelRuns04-09.rds"), version=2)



loo_compare(looRegReCov, looReCov)


# bayes_R2_res <- function(fit) {
#     y <- rstanarm::get_y(fit)
#     ypred <- rstanarm::posterior_epred(fit)
#     if (family(fit)$family == "binomial" && NCOL(y) == 2) {
#         trials <- rowSums(y)
#         y <- y[, 1]
#         ypred <- ypred %*% diag(trials)
#     }
#     e <- -1 * sweep(ypred, 2, y)
#     var_ypred <- apply(ypred, 1, var)
#     var_e <- apply(e, 1, var)
#     var_ypred / (var_ypred + var_e)
# }


# bayes_R2_res(summary(reg_reCovModel)[["summary"]])
