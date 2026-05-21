###############################################################################
### After narrowing down our model options in the runPoolingModel.R script, 
### We use this script, to test the leading two options in leave one country
### out analysis. Even though we have parallelized a bit, this script still 
### takes 4-5 hours to implement with four cores. Plan accordingly. 
### Operationalized as kfold where each fold is a country. 
###############################################################################
### Inspired by code here
### https://www.r-bloggers.com/2018/07/k-fold-cross-validation-in-stan/
###############################################################################

library(rstan)
library(loo)
library(here)
tbPtntCstSeed <- 513
tbPtntCstIter <- 1000
tbPtntCstWarmup <- 500

### Create a list of data based on our ids 
source(here("code/modelDataFormat.R"))

stanModelData <- list(
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
    survey_year = ((modelData$study.end.year - mean(modelData$study.end.year)) / sd(modelData$study.end.year)),
    ### External covariates
    median_income = ((modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income)),
    oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$UHCindex), 
    uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex),
    prior_sigma = 2,
    ### test with world region shut off , 
    beta_exp = 1,
    world_region = rep(0, length(modelData$cost)),
    prob_cost_zero = sum(modelData$cost == 0) / nrow(modelData)
    
) 

fold_data_list <- rep(list(stanModelData),
                      length(unique(modelData$country.code)))

holdout_list <- list()

for (country in unique(modelData$country.code)){
    index <- which(unique(modelData$country.code) == country)
    holdout_list[[index]] <- as.numeric(modelData$country.code == country)
    fold_data_list[[index]]$holdout <- holdout_list[[index]]

}

# fold_data_list <- rep(list(stanModelData),
#                       2)
# 
# holdout_list <- list(c(rep(1, 220), rep(0, 219)),
#                      c(rep(0, 220), rep(1, 219)))
# 
# fold_data_list[[1]]$holdout <- holdout_list[[1]]
# fold_data_list[[2]]$holdout <- holdout_list[[2]]

stan_kfold <-  function(file = "code/inst/tbPatientCosts-hurdle.stan",
                        list_of_datas = fold_data_list,
                        chains = 3, cores = 4){
    library(pbmcapply)
    badRhat <- 1.1 
    n_fold <- length(list_of_datas)
    model <- stan_model(file=file)
    
    # First parallelize all chains:
    sflist <- pbmclapply(1:(n_fold*chains), mc.cores = cores, 
                         function(i){
                             # Fold number:
                             k <- ceiling(i / chains)
                             s <- sampling(model, 
                                           data = list_of_datas[[k]], 
                                           chains = 1, chain_id = i,
                                           warmup = tbPtntCstWarmup,
                                           iter = tbPtntCstIter,
                                           control=list(max_treedepth=14),
                                           refresh = 0, seed = tbPtntCstSeed)
                             return(s)
                        })
    
    # Then merge the K * chains to create K stanfits:
    stanfit <- list()
    for(k in 1:n_fold){
        inchains <- (chains*k - (chains - 1)):(chains*k)
        #  Merge `chains` of each fold
        stanfit[[k]] <- sflist2stanfit(sflist[inchains])
    }  
    return(stanfit) 
}

#extract log-likelihoods of held-out data
extract_log_lik_K <- function(list_of_stanfits,
                              list_of_holdout){
    require(loo)
    K <- length(list_of_stanfits)
    list_of_log_liks <- plyr::llply(1:K, function(k){
        extract_log_lik(list_of_stanfits[[k]])
    })
    # `log_lik_heldout` will include the loglike of all the held out data of all the folds.
    # We define `log_lik_heldout` as a (samples x N_obs) matrix
    # (similar to each log_lik matrix)
    log_lik_heldout <- list_of_log_liks[[1]] * NA
    for(k in 1:K){
        log_lik <- list_of_log_liks[[k]]
        samples <- dim(log_lik)[1] 
        N_obs <- dim(log_lik)[2]
        # This is a matrix with the same size as log_lik_heldout
        # with 1 if the data was held out in the fold k
        heldout <- matrix(rep(list_of_holdout[[k]], each = samples), nrow = samples)
        # Sanity check that the previous log_lik is not being overwritten:
        if(any(!is.na(log_lik_heldout[heldout==1]))){
            warning("Heldout log_lik has been overwritten!!!!")
        }
        # We save here the log_lik of the fold k in the matrix:
        log_lik_heldout[heldout==1] <- log_lik[heldout==1]
    }
    return(log_lik_heldout)
}


#compute ELPD
kfold_elpd <- function(log_lik_heldout)  {
    library(matrixStats)
    logColMeansExp <- function(x) {
        # should be more stable than log(colMeans(exp(x)))
        S <- nrow(x)
        colLogSumExps(x) - log(S)
    }
    # See equation (20) of @VehtariEtAl2016
    pointwise <-  matrix(logColMeansExp(log_lik_heldout), ncol= 1)
    colnames(pointwise) <- "elpd"
    # See equation (21) of @VehtariEtAl2016
    elpd_kfold <- sum(pointwise)
    se_elpd_kfold <-  sqrt(ncol(log_lik_heldout) * var(pointwise))
    out <- list(
        pointwise = pointwise,
        elpd_kfold = elpd_kfold,
        se_elpd_kfold = se_elpd_kfold)
    #structure(out, class = "loo")
    return(out)
}

###############################################################################
### Covariates have the a common effect and a random effect across cost category
###############################################################################
ss_re <- stan_kfold(file = "code/inst/tbPatientCosts-hurdle.stan")

ee_re <- extract_log_lik_K(ss_re, holdout_list)

kk_re <- kfold_elpd(ee_re); print(kk_re)

mainModelComp_re <- list(ss_re, ee_re, kk_re)

saveRDS(mainModelComp_re, here("output/mainModelLOCO-ELPD.rds"))

###############################################################################
### Covariates have the same effect across cost category
###############################################################################
ss_simp <- stan_kfold(file = "code/inst/tbPatientCosts-simpCov-hurdle.stan")

ee_simp <- extract_log_lik_K(ss_simp, holdout_list)

kk_simp <- kfold_elpd(ee_simp); print(kk_simp)

mainModelComp_simp <- list(ss_simp, ee_simp, kk_simp)
saveRDS(mainModelComp_simp, here("output/simpModelLOCO-ELPD.rds"))
