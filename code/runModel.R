###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)
library(rstan)

###############################################################################
##### Read in the model data                                              #####
###############################################################################
dataList <- readRDS(here("data/tempCleanData.rds"))

# modelData <- dataList$data %>% filter(pop.id == "Total") %>% 
#     select(covidence.id, 
#            dme.prediag.total.mean, dme.prediag.total.sd,# dme.prediag.total.ci, 
#            dmne.prediag.total.mean, dmne.prediag.total.sd, #dmne.prediag.total.ci, 
#            ie.prediag.lostincome.total.mean, ie.prediag.lostincome.total.sd #, ie.prediag.lostincome.total.ci
#            ) %>% 
#      mutate(dme.prediag.total.mean = as.numeric(dme.prediag.total.mean), 
#             dme.prediag.total.sd = as.numeric(dme.prediag.total.sd), 
#             dmne.prediag.total.mean = as.numeric(dmne.prediag.total.mean), 
#             dmne.prediag.total.sd = as.numeric(dmne.prediag.total.sd), 
#             ie.prediag.lostincome.total.mean = as.numeric(ie.prediag.lostincome.total.mean), 
#             ie.prediag.lostincome.total.sd = as.numeric(ie.prediag.lostincome.total.sd)) %>% 
#     pivot_longer(cols=dme.prediag.total.mean:ie.prediag.lostincome.total.sd, 
#                  names_to = "category", values_to = "cost", values_drop_na = TRUE)

###############################################################################
##### Format the model data to be fed into the stan model.                #####
###############################################################################
modelDataMeans<- dataList$data %>% #filter(pop.id == "Total") %>% 
    select(covidence.id, 
           ### MEANS
           ### total tb care costs
           de.tbcare.mean,
           dme.tbcare.total.mean, 
           dmne.tbcare.total.mean, 
           ie.tbcare.lostincome.total.mean, 
           ### pre-diagnosis costs
           de.prediag.mean,
           dme.prediag.total.mean,  
           dmne.prediag.total.mean, 
           ie.prediag.lostincome.total.mean, 
           ### post-diagnosis
           de.postdiag.mean,
           dme.postdiag.total.mean,  
           dmne.postdiag.total.mean, 
           ie.postdiag.lostincome.total.mean, 
           ### intensive treatment
           de.intens.tx.mean,
           dme.intens.tx.total.mean,  
           dmne.intens.tx.total.mean, 
           ie.intens.tx.lostincome.total.mean, 
           ### continuation treatment
           de.cont.tx.mean,
           dme.cont.tx.total.mean,  
           dmne.cont.tx.total.mean, 
           ie.cont.tx.lostincome.total.mean, 
           ### STANDARD DEVIATIONS
           ### total tb care costs
           de.tbcare.sd,
           dme.tbcare.total.sd, 
           dmne.tbcare.total.sd,
           ie.tbcare.lostincome.total.sd, 
           ### pre-diagnostic costs
           de.prediag.sd, 
           dme.prediag.total.sd, 
           dmne.prediag.total.sd,
           ie.prediag.lostincome.total.sd, 
           ### post-diagnostic costs
           de.postdiag.sd, 
           dme.postdiag.total.sd, 
           dmne.postdiag.total.sd,
           ie.postdiag.lostincome.total.sd,
           ### intensive treatment
           de.intens.tx.sd,  
           dme.intens.tx.total.sd,  
           dmne.intens.tx.total.sd, 
           ie.intens.tx.lostincome.total.sd, 
           ### continuation treatment
           de.cont.tx.sd,
           dme.cont.tx.total.sd,  
           dmne.cont.tx.total.sd, 
           ie.cont.tx.lostincome.total.sd
    ) %>% 
    pivot_longer(cols=de.tbcare.mean:ie.cont.tx.lostincome.total.mean, 
                 names_to = "category", values_to = "cost", values_drop_na = TRUE) %>%
    mutate(se = case_when(category=="de.tbcare.mean" ~ de.tbcare.sd, 
                          category=="dme.tbcare.total.mean" ~ dme.tbcare.total.sd, 
                          category=="dmne.tbcare.total.mean" ~ dmne.tbcare.total.sd, 
                          category=="ie.tbcare.lostincome.total.mean" ~ ie.tbcare.lostincome.total.sd, 
                          category=="de.prediag.mean" ~ de.prediag.sd, 
                          category=="dme.prediag.total.mean" ~ dme.prediag.total.sd, 
                          category=="dmne.prediag.total.mean" ~ dmne.prediag.total.sd, 
                          category=="ie.prediag.lostincome.total.mean" ~ ie.prediag.lostincome.total.sd,
                          category=="de.postdiag.mean" ~ de.postdiag.sd, 
                          category=="dme.postdiag.total.mean" ~ dme.postdiag.total.sd, 
                          category=="dmne.postdiag.total.mean" ~ dmne.postdiag.total.sd, 
                          category=="ie.postdiag.lostincome.total.mean" ~ ie.postdiag.lostincome.total.sd,
                          category=="de.intens.tx.mean" ~ de.intens.tx.sd, 
                          category=="dme.intens.tx.total.mean" ~ dme.intens.tx.total.sd, 
                          category=="dmne.intens.tx.total.mean" ~ dmne.intens.tx.total.sd, 
                          category=="ie.intens.tx.lostincome.total.mean" ~ ie.intens.tx.lostincome.total.sd,
                          category=="de.cont.tx.mean" ~ de.cont.tx.sd, 
                          category=="dme.cont.tx.total.mean" ~ dme.cont.tx.total.sd, 
                          category=="dmne.cont.tx.total.mean" ~ dmne.cont.tx.total.sd, 
                          category=="ie.cont.tx.lostincome.total.mean" ~ ie.cont.tx.lostincome.total.sd), 
           dme.prediag = ifelse(category %in% c("de.tbcare.mean", "dme.tbcare.total.mean", 
                                                "dme.tbcare.mean", "dme.prediag.total.mean"), 1,0), 
           dnme.prediag = ifelse(category %in% c("de.tbcare.mean", "dmne.tbcare.total.mean", 
                                                 "de.prediag.mean", "dmne.prediag.total.mean"), 1,0), 
           ie.prediag = ifelse(category %in% c("ie.tbcare.lostincome.total.mean", "ie.prediag.lostincome.total.mean"), 1,0), 
           dme.intsvtx = ifelse(category %in% c("de.tbcare.mean", "dme.tbcare.total.mean", 
                                                "de.postdiag.mean", "dme.postdiag.total.mean",
                                                "de.intens.tx.mean", "dme.intens.tx.total.mean"), 1,0), 
           dnme.intsvtx = ifelse(category %in% c("de.tbcare.mean", "dmne.tbcare.total.mean", 
                                                 "de.postdiag.mean", "dmne.postdiag.total.mean",
                                                 "de.intens.tx.mean","dmne.intens.tx.total.mean"), 1,0), 
           ie.intsvtx = ifelse(category %in% c("ie.tbcare.lostincome.total.mean", "ie.postdiag.lostincome.total.mean",
                                               "ie.intens.tx.lostincome.total.mean"), 1,0), 
           dme.contntx = ifelse(category %in% c("de.tbcare.mean", "dme.tbcare.total.mean", 
                                                "de.postdiag.mean", "dme.postdiag.total.mean",
                                                "de.cont.tx.mean", "dme.cont.tx.total.mean"), 1,0), 
           dnme.contntx = ifelse(category %in% c("de.tbcare.mean", "dmne.tbcare.total.mean", 
                                                 "de.postdiag.mean", "dmne.postdiag.total.mean",
                                                 "de.cont.tx.mean", "dmne.cont.tx.total.mean"), 1,0), 
           ie.contntx = ifelse(category %in% c("ie.tbcare.lostincome.total.mean", "ie.postdiag.lostincome.total.mean",
                                               "ie.cont.tx.lostincome.total.mean"), 1,0), 
           mean = 1, 
           median = 0, 
           cost = as.numeric(cost)) %>%
    dplyr::select(covidence.id, cost, se, 
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median) %>% na.omit() 

modelDataMedians <- dataList$data %>% #filter(pop.id == "Total") %>% 
    select(covidence.id, 
           ### MEDIANS
           ### total tb care costs 
           de.tbcare.median, 
           dme.tbcare.total.median, 
           dmne.tbcare.total.median, 
           ie.tbcare.lostincome.total.median, 
           ### pre-diagnosis costs
           de.prediag.median, 
           dme.prediag.total.median,  
           dmne.prediag.total.median, 
           ie.prediag.lostincome.total.median, 
           ### post-diagnosis
           de.postdiag.median, 
           dme.postdiag.total.median,  
           dmne.postdiag.total.median, 
           ie.postdiag.lostincome.total.median, 
           ### intensive treatment
           de.intens.tx.median, 
           dme.intens.tx.total.median,  
           dmne.intens.tx.total.median, 
           ie.intens.tx.lostincome.total.median, 
           ### continuation treatment
           de.cont.tx.median, 
           dme.cont.tx.total.median,  
           dmne.cont.tx.total.median, 
           ie.cont.tx.lostincome.total.median
    ) %>% 
    pivot_longer(cols=de.tbcare.median:ie.postdiag.lostincome.total.median, 
                 names_to = "category", values_to = "cost", values_drop_na = TRUE) %>%
    mutate(se = case_when(category=="de.tbcare.median" ~ as.numeric(cost)/2, 
                          category=="dme.tbcare.total.median" ~ as.numeric(cost)/2, 
                          category=="dmne.tbcare.total.median" ~ as.numeric(cost)/2, 
                          category=="ie.tbcare.lostincome.total.median" ~ as.numeric(cost)/2,
                          category=="de.prediag.median" ~ as.numeric(cost)/2, 
                          category=="dme.prediag.total.median" ~ as.numeric(cost)/2, 
                          category=="dmne.prediag.total.median" ~ as.numeric(cost)/2, 
                          category=="ie.prediag.lostincome.total.median" ~ as.numeric(cost)/2,
                          category=="de.postdiag.median" ~ as.numeric(cost)/2, 
                          category=="dme.postdiag.total.median" ~ as.numeric(cost)/2, 
                          category=="dmne.postdiag.total.median" ~ as.numeric(cost)/2, 
                          category=="ie.postdiag.lostincome.total.median" ~ as.numeric(cost)/2,
                          category=="de.intens.tx.median" ~ as.numeric(cost)/2, 
                          category=="dme.intens.tx.total.median" ~ as.numeric(cost)/2, 
                          category=="dmne.intens.tx.total.median" ~ as.numeric(cost)/2, 
                          category=="ie.intens.tx.lostincome.total.median" ~ as.numeric(cost)/2,
                          category=="de.cont.tx.median" ~ as.numeric(cost)/2, 
                          category=="dme.cont.tx.total.median" ~ as.numeric(cost)/2, 
                          category=="dmne.cont.tx.total.median" ~ as.numeric(cost)/2, 
                          category=="ie.cont.tx.lostincome.total.median" ~ as.numeric(cost)/2), 
           dme.prediag = ifelse(category %in% c("de.tbcare.median", "dme.tbcare.total.median", 
                                                "dme.tbcare.median", "dme.prediag.total.median"), 1,0), 
           dnme.prediag = ifelse(category %in% c("de.tbcare.median", "dmne.tbcare.total.median", 
                                                 "de.prediag.median", "dmne.prediag.total.median"), 1,0), 
           ie.prediag = ifelse(category %in% c("ie.tbcare.lostincome.total.median", "ie.prediag.lostincome.total.median"), 1,0), 
           dme.intsvtx = ifelse(category %in% c("de.tbcare.median", "dme.tbcare.total.median", 
                                                "de.postdiag.median", "dme.postdiag.total.median",
                                                "de.intens.tx.median", "dme.intens.tx.total.median"), 1,0), 
           dnme.intsvtx = ifelse(category %in% c("de.tbcare.median", "dmne.tbcare.total.median", 
                                                 "de.postdiag.median", "dmne.postdiag.total.median",
                                                 "de.intens.tx.median","dmne.intens.tx.total.median"), 1,0), 
           ie.intsvtx = ifelse(category %in% c("ie.tbcare.lostincome.total.median", "ie.postdiag.lostincome.total.median",
                                               "ie.intens.tx.lostincome.total.median"), 1,0), 
           dme.contntx = ifelse(category %in% c("de.tbcare.median", "dme.tbcare.total.median", 
                                                "de.postdiag.median", "dme.postdiag.total.median",
                                                "de.cont.tx.median", "dme.cont.tx.total.median"), 1,0), 
           dnme.contntx = ifelse(category %in% c("de.tbcare.median", "dmne.tbcare.total.median", 
                                                 "de.postdiag.median", "dmne.postdiag.total.median",
                                                 "de.cont.tx.median", "dmne.cont.tx.total.median"), 1,0), 
           ie.contntx = ifelse(category %in% c("ie.tbcare.lostincome.total.median", "ie.postdiag.lostincome.total.median",
                                               "ie.cont.tx.lostincome.total.median"), 1,0), 
           mean = 0, 
           median = 1, 
           cost = as.numeric(cost)) %>%
    dplyr::select(covidence.id, cost, se, 
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median) %>% na.omit() 

modelData <- rbind(modelDataMeans, modelDataMedians)[-46,]

stanModelData <- list(
                        N = length(modelData$cost),
                        cost = modelData$cost + 0.01,
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
                        ie_contntx =  modelData$ie.contntx
                    )

###############################################################################
##### Call the stan model.                                                #####
###############################################################################
totalPreDiag <- stan(file = here("code/inst/tbPatientCosts.stan"), 
                     data = stanModelData, 
                     iter = 1000, chains = 3, 
                     control=list(max_treedepth=14))

#### Empirical ################################################################

sd(modelData$cost)
mean(as.numeric(dataList$data$dme.prediag.total.mean), na.rm = T)

mean(as.numeric(dataList$data$dmne.prediag.total.mean), na.rm = T)

mean(as.numeric(dataList$data$ie.prediag.lostincome.total.mean), na.rm = T)
