###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(rstan)
###############################################################################
##### Source in the formatted model data                                  #####
###############################################################################
source(here("code/modelDataFormat.R"))
                       
###############################################################################
### Format the data for stan                                                ###
###############################################################################
stanModelData <- list(
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
                        drtb_ind= modelData$drtb.ind,
                        survey_year = modelData$year_z,
                        ### External covariates
                        median_income = ((modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income)),
                        oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$UHCindex), 
                        uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex),
                        prior_sigma = 2
                    )

###############################################################################
##### Call the stan models                                                #####
###############################################################################

###############################################################################
### Covariates allowed to vary across  each cost category
###############################################################################

totalModel <- stan(file = "code/inst/tbPatientCosts.stan",
                   data = stanModelData,
                   iter = tbPtntCstIter/4, chains = 2,
                   warmup=tbPtntCstWarmup/4,
                   control=list(max_treedepth=14), 
                   seed = tbPtntCstSeed)

rstan::loo(totalModel)

### Pooled patient costs as mean median income and oop perc and study year
exp(summary(totalModel)[[1]][1:9,"mean"])
sum(exp(summary(totalModel)[[1]][1:9,"mean"]))
# exp(summary(totalModel)[[1]][1:9,"50%"])
# exp(summary(totalModel)[[1]][1:9,"2.5%"])
# exp(summary(totalModel)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
# costCategories <- paste(c(rep("dme",3), rep("dnme",3), rep("ie",3)), 
#                         c("prediag", "intsvtx", "contntx"), sep="_")
# yearCoeffs <- paste0("beta_year_", costCategories)
# exp(summary(totalModel)[[1]][yearCoeffs,"mean"])
# 
# ### Print the estimated impact of drtb
# drtbCoeffs <- paste0("beta_drtb_", costCategories)
# exp(summary(totalModel)[[1]][drtbCoeffs,"mean"])

###############################################################################
### Covariates have the a common effect and a random effect across cost category
###############################################################################
reCovModel <- stan(file = "code/inst/tbPatientCosts-randEffCov.stan", 
                   data = stanModelData,
                   iter = tbPtntCstIter/2, chains = 2,
                   warmup=tbPtntCstWarmup/2,
                   control=list(max_treedepth=14),
                   seed = tbPtntCstSeed)

sum(exp(summary(reCovModel)[[1]][1:9,"mean"]))
exp(summary(reCovModel)[[1]][1:9,"mean"])

rstan::loo(reCovModel) 
loo_compare(rstan::loo(totalModel), rstan::loo(reCovModel))

reCovModelNoUnit <- reCovModel
reCovModelUnit <- reCovModel
###############################################################################
### Covariates have the same effect across cost category
###############################################################################
simpCovMod <- stan(file = "code/inst/tbPatientCosts-simpCov.stan",
                   data = stanModelData,
                   iter = tbPtntCstIter, chains = 3,
                   warmup= tbPtntCstWarmup,
                   control=list(max_treedepth=14), 
                   seed = tbPtntCstSeed)

### Pooled patient costs as mean median income and oop perc and study year
exp(summary(simpCovMod)[[1]][1:9,"mean"])
# exp(summary(totalModel)[[1]][1:9,"50%"])
# exp(summary(totalModel)[[1]][1:9,"2.5%"])
# exp(summary(totalModel)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
exp(summary(simpCovMod)[[1]]["beta_year","mean"])

### Print the estimated impact of drtb
exp(summary(simpCovMod)[[1]]["beta_drtb","mean"])

###############################################################################
### Compare with LOO
###############################################################################
### First check that pointwise log likelihood approximates that stan outputs
### for the total log likelihood 

logLiks <- paste0("log_lik[", 1:nrow(modelData), "]")
sum(summary(totalModel)[[1]][logLiks,"mean"])
summary(totalModel)[[1]]["lp__","mean"]

sum(summary(simpCovMod)[[1]][logLiks,"mean"])
summary(simpCovMod)[[1]]["lp__","mean"]


looTotalN5 <- rstan::loo(totalN5Model)
looReCov <- rstan::loo(reCovModel)
looSimpN5 <- rstan::loo(simpCovN5Mod)

loo_compare(looTotal, looTotalN5, looReCov, looReCovN5, looSimp, looSimpN5)

modelList5 <- list("Data" = modelData, "9covs" = totalN5Model, 
                   "9covsRE" = reCovN5Model, "1cov" =simpCovN5Mod)
looList10 <- list(looTotal, looReCov, looSimp)
saveRDS(modelList5, here("output/modelswithunitnormal5.rds"), version=2)


###############################################################################
##### Check different priors for the two more complex models 
###############################################################################
totModList <- list() 
reModList <- list()

for (i in c(2,4)){
    
    tmpModelData <- stanModelData
    tmpModelData[["prior_sigma"]] <- i
    
    totModList[[i]] <- stan(file = "code/inst/tbPatientCosts.stan",
                       data = tmpModelData,
                       iter = tbPtntCstIter, chains = 3,
                       warmup=tbPtntCstWarmup,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)
    
    reModList[[i]] <- stan(file = "code/inst/tbPatientCosts-randEffCov.stan", 
                       data = tmpModelData, 
                       iter = tbPtntCstIter, chains = 3,
                       warmup=tbPtntCstWarmup,
                       control=list(max_treedepth=14),
                       seed = tbPtntCstSeed)
}


loo_compare(loo(totModList[[2]]), loo(totModList[[4]]),
            loo(reModList[[2]]), loo(reModList[[4]]))

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
