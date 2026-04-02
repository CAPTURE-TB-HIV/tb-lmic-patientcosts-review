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
dataList <- readRDS(here("data/reviewData/finalData.rds"))
dataInds <- readRDS(here("data/dataIndicators.rds"))

fullData <- dataList$data %>% 
            left_join(dataInds) %>% 
            ### Add in study end year
            left_join(dataList$descriptive %>% select(covidence.id, study.end.year))
            
medianIncome <- readRDS(here("data/covariateData/medianIncome.rds")) %>% distinct()
oopSpending <- readRDS(here("data/covariateData/healthAccounts.rds")) %>% distinct()

###############################################################################
##### Format the model data to be fed into the stan model.                #####
###############################################################################
modelDataMeans<- fullData %>% 
    select(covidence.id, 
           study.end.year,
           pop.id, n.participants,
           drtb, dstb, drtb.desc,
           whohb, urban, public, 
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
    ##########################################################################################
    ### Filter pop.ids we aren't interested in  ##############################################
    ##########################################################################################
    filter(! pop.id %in% c("EPTB", "EPDRTB", "ACF", "Total6H", "Total3HP", 
                           "Adult6H", "Adult3HP", "Child6H", "Child3HP")) %>% 
    ##########################################################################################
    ### Create a DR-TB indicator  ############################################################
    ##########################################################################################
    mutate(
           drtb.ind = ifelse(pop.id == "DRTB" | drtb == 1, 1, 0),
           dstb.ind = ifelse(pop.id == "DSTB" | 
                            dstb == 1 | 
                            grepl("all DS-TB|all DSTB|DSTB only|DS-TB only", drtb.desc), 1, 0),

    ##########################################################################################
    ### Arithmetic if we are missing some calculable values ##################################
    ##########################################################################################
    ###  if we have total tb care and either pre-diag or post-diag costs
            dme.postdiag.total.mean = ifelse(!is.na(dme.tbcare.total.mean) & 
                                         !is.na(dme.prediag.total.mean) &
                                         is.na(dme.postdiag.total.mean),
                                     dme.tbcare.total.mean - dme.prediag.total.mean, 
                                     dme.postdiag.total.mean),
           dmne.postdiag.total.mean = ifelse(!is.na(dmne.tbcare.total.mean) & 
                                                !is.na(dmne.prediag.total.mean) &
                                                is.na(dmne.postdiag.total.mean),
                                            dmne.tbcare.total.mean - dmne.prediag.total.mean, 
                                            dmne.postdiag.total.mean), 
           ie.postdiag.lostincome.total.mean = ifelse(!is.na(ie.tbcare.lostincome.total.mean) & 
                                                 !is.na(ie.prediag.lostincome.total.mean) &
                                                 is.na(ie.postdiag.lostincome.total.mean),
                                                 ie.tbcare.lostincome.total.mean - ie.prediag.lostincome.total.mean, 
                                             ie.postdiag.lostincome.total.mean),
    ### If we have direct expenditure totals and direct medical or direct non-medical
            dmne.tbcare.total.mean = ifelse(!is.na(de.tbcare.mean) & 
                                         !is.na(dme.tbcare.total.mean) &
                                         is.na(dmne.tbcare.total.mean),
                                         de.tbcare.mean - dme.tbcare.total.mean, 
                                     dmne.tbcare.total.mean),
            dmne.prediag.total.mean = ifelse(!is.na(de.prediag.mean) & 
                                                 !is.na(dme.prediag.total.mean) &
                                                 is.na(dmne.prediag.total.mean),
                                             de.prediag.mean - dme.prediag.total.mean, 
                                             dmne.prediag.total.mean)
    ) %>%
    ##########################################################################################
    ### Drop duplicates from the data ########################################################
    ##########################################################################################
    mutate(de.postdiag.mean = ifelse(!is.na(de.intens.tx.mean) & !is.na(de.cont.tx.mean), 
                                     NA, de.postdiag.mean),
           dme.postdiag.total.mean = ifelse(!is.na(dme.intens.tx.total.mean) & 
                                                !is.na(dme.cont.tx.total.mean), 
                                     NA, dme.postdiag.total.mean), 
           dmne.postdiag.total.mean = ifelse(!is.na(dmne.intens.tx.total.mean) & 
                                                 !is.na(dmne.cont.tx.total.mean), 
                                     NA, dmne.postdiag.total.mean), 
           ie.postdiag.lostincome.total.mean = ifelse(!is.na(ie.intens.tx.lostincome.total.mean) & 
                                         !is.na(ie.cont.tx.lostincome.total.mean), 
                                     NA, ie.postdiag.lostincome.total.mean),
           ### Total TB Care 
           de.tbcare.mean = ifelse((!is.na(de.prediag.mean) & !is.na(de.postdiag.mean)) | 
                                   (!is.na(de.prediag.mean)  &
                                    !is.na(de.intens.tx.mean) & !is.na(de.cont.tx.mean)), 
                                     NA, de.tbcare.mean),
           dme.tbcare.total.mean = ifelse((!is.na(dme.prediag.total.mean) & 
                                               !is.na(dme.postdiag.total.mean)) | 
                                          (!is.na(dme.prediag.total.mean)  &
                                           !is.na(dme.intens.tx.total.mean) & 
                                           !is.na(dme.cont.tx.total.mean)), 
                                          NA, dme.tbcare.total.mean), 
           dmne.tbcare.total.mean = ifelse((!is.na(dmne.prediag.total.mean) & !is.na(dmne.postdiag.total.mean)) | 
                                         (!is.na(dmne.prediag.total.mean)  &
                                          !is.na(dmne.intens.tx.total.mean) & !is.na(dmne.cont.tx.total.mean)), 
                                          NA, dmne.tbcare.total.mean), 
           ie.tbcare.lostincome.total.mean = ifelse((!is.na(ie.prediag.lostincome.total.mean) & 
                                                     !is.na(ie.postdiag.lostincome.total.mean)) |
                                                    (!is.na(ie.prediag.lostincome.total.mean) & 
                                                     !is.na(ie.intens.tx.lostincome.total.mean) &
                                                     !is.na(ie.cont.tx.lostincome.total.mean)), 
                                     NA, ie.tbcare.lostincome.total.mean),
           ### Only keep direct expenditures if we do not have dme and dmne
           de.prediag.mean = ifelse(!is.na(dme.prediag.total.mean) & !is.na(dmne.prediag.total.mean), 
                                   NA, de.prediag.mean),
           de.postdiag.mean = ifelse(!is.na(dme.postdiag.total.mean) & !is.na(dmne.postdiag.total.mean), 
                                    NA, de.postdiag.mean),
           de.intens.tx.mean = ifelse(!is.na(dme.intens.tx.total.mean) & !is.na(dmne.intens.tx.total.mean), 
                                    NA, de.intens.tx.mean),
           de.cont.tx.mean = ifelse(!is.na(dme.cont.tx.total.mean) & !is.na(dmne.cont.tx.total.mean), 
                                    NA, de.cont.tx.mean),
           de.tbcare.mean = ifelse(!is.na(dme.tbcare.total.mean) & !is.na(dmne.tbcare.total.mean), 
                                    NA, de.tbcare.mean)
           ) %>% 
    
    pivot_longer(cols=de.tbcare.mean:ie.cont.tx.lostincome.total.mean, 
                 names_to = "category", values_to = "cost", values_drop_na = TRUE) %>%
    ##########################################################################################
    ### Assign standard deviations ###########################################################
    ##########################################################################################
    mutate(n.participants = as.numeric(n.participants), 
           sd = case_when(category=="de.tbcare.mean" ~ de.tbcare.sd, 
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
           se = case_when(category=="de.tbcare.mean" ~ de.tbcare.sd / sqrt(n.participants), 
                          category=="dme.tbcare.total.mean" ~ dme.tbcare.total.sd / sqrt(n.participants), 
                          category=="dmne.tbcare.total.mean" ~ dmne.tbcare.total.sd / sqrt(n.participants), 
                          category=="ie.tbcare.lostincome.total.mean" ~ ie.tbcare.lostincome.total.sd / sqrt(n.participants), 
                          category=="de.prediag.mean" ~ de.prediag.sd / sqrt(n.participants), 
                          category=="dme.prediag.total.mean" ~ dme.prediag.total.sd / sqrt(n.participants), 
                          category=="dmne.prediag.total.mean" ~ dmne.prediag.total.sd / sqrt(n.participants), 
                          category=="ie.prediag.lostincome.total.mean" ~ ie.prediag.lostincome.total.sd / sqrt(n.participants),
                          category=="de.postdiag.mean" ~ de.postdiag.sd / sqrt(n.participants), 
                          category=="dme.postdiag.total.mean" ~ dme.postdiag.total.sd / sqrt(n.participants), 
                          category=="dmne.postdiag.total.mean" ~ dmne.postdiag.total.sd / sqrt(n.participants), 
                          category=="ie.postdiag.lostincome.total.mean" ~ ie.postdiag.lostincome.total.sd / sqrt(n.participants),
                          category=="de.intens.tx.mean" ~ de.intens.tx.sd / sqrt(n.participants), 
                          category=="dme.intens.tx.total.mean" ~ dme.intens.tx.total.sd / sqrt(n.participants), 
                          category=="dmne.intens.tx.total.mean" ~ dmne.intens.tx.total.sd / sqrt(n.participants), 
                          category=="ie.intens.tx.lostincome.total.mean" ~ ie.intens.tx.lostincome.total.sd / sqrt(n.participants),
                          category=="de.cont.tx.mean" ~ de.cont.tx.sd / sqrt(n.participants), 
                          category=="dme.cont.tx.total.mean" ~ dme.cont.tx.total.sd / sqrt(n.participants), 
                          category=="dmne.cont.tx.total.mean" ~ dmne.cont.tx.total.sd / sqrt(n.participants), 
                          category=="ie.cont.tx.lostincome.total.mean" ~ ie.cont.tx.lostincome.total.sd) / sqrt(n.participants), 
           ##########################################################################################
           ### Assign cost category indicators ######################################################
           ##########################################################################################
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
           ##########################################################################################
           ### Covariates from the data #############################################################
           ##########################################################################################
           cost = as.numeric(cost)) %>%
           ### Filter to only those that report DRTB or DSTB costs ##################################
           filter(xor(drtb.ind==1, dstb.ind==1)) %>%
    dplyr::select(covidence.id, pop.id,
                  study.end.year,
                  n.participants, cost, sd, se, 
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median, 
                  #### Covariates ####
                  drtb.ind, dstb.ind,
                  drtb, dstb,
                  whohb, urban, public) 

##############################################################################|
### Estimate gamma shape parameter for the conversion from means to medians ###
##############################################################################|
coeffVar <- median(modelDataMeans$sd / modelDataMeans$cost, na.rm = TRUE)

modelDataMeans <- modelDataMeans %>% select (!sd) %>% mutate(se = 0) %>% na.omit()
# modelDataMeans <- modelDataMeans %>% na.omit()

##############################################################################|
### Medians  ##################################################################
##############################################################################|
modelDataMedians <- fullData %>%  
    select(covidence.id, 
           study.end.year,
           pop.id, 
           drtb, dstb, drtb.desc, 
           whohb, urban, public,
           n.participants,
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
    ##########################################################################################
    ### Filter pop.ids we aren't interested in  ##############################################
    ##########################################################################################
    filter(! pop.id %in% c("EPTB", "EPDRTB", "ACF", "Total6H", "Total3HP", 
                           "Adult6H", "Adult3HP", "Child6H", "Child3HP")) %>% 
    
    ##########################################################################################
    ### Create a DR-TB indicator  ############################################################
    ##########################################################################################
    mutate(
            drtb.ind = ifelse(pop.id == "DRTB" | drtb == 1, 1, 0),
            dstb.ind = ifelse(pop.id == "DSTB" | 
                                  dstb == 1 | 
                                  grepl("all DS-TB|all DSTB|DSTB only|DS-TB only", drtb.desc), 1, 0),
    #########################################################################################|
    ### Arithmetic if we are missing some calculable values ##################################
    #########################################################################################|
    ###  if we have total tb care and either pre-diag or post-diag costs
            dme.postdiag.total.median = ifelse(!is.na(dme.tbcare.total.median) & 
                                                !is.na(dme.prediag.total.median) &
                                                is.na(dme.postdiag.total.median),
                                            dme.tbcare.total.median - dme.prediag.total.median, 
                                            dme.postdiag.total.median),
            dmne.postdiag.total.median = ifelse(!is.na(dmne.tbcare.total.median) & 
                                                 !is.na(dmne.prediag.total.median) &
                                                 is.na(dmne.postdiag.total.median),
                                             dmne.tbcare.total.median - dmne.prediag.total.median, 
                                             dmne.postdiag.total.median), 
            ie.postdiag.lostincome.total.median = ifelse(!is.na(ie.tbcare.lostincome.total.median) & 
                                                          !is.na(ie.prediag.lostincome.total.median) &
                                                          is.na(ie.postdiag.lostincome.total.median),
                                                      ie.tbcare.lostincome.total.median - ie.prediag.lostincome.total.median, 
                                                      ie.postdiag.lostincome.total.median),
            ### If we have direct expenditure totals and direct medical or direct non-medical
            dmne.tbcare.total.median = ifelse(!is.na(de.tbcare.median) & 
                                               !is.na(dme.tbcare.total.median) &
                                               is.na(dmne.tbcare.total.median),
                                           de.tbcare.median - dme.tbcare.total.median, 
                                           dmne.tbcare.total.median),
           dmne.prediag.total.median = ifelse(!is.na(de.prediag.median) & 
                                                !is.na(dme.prediag.total.median) &
                                                is.na(dmne.prediag.total.median),
                                            de.prediag.median - dme.prediag.total.median, 
                                            dmne.prediag.total.median),
           dmne.postdiag.total.median = ifelse(!is.na(de.postdiag.median) & 
                                                  !is.na(dme.postdiag.total.median) &
                                                  is.na(dmne.postdiag.total.median),
                                              de.postdiag.median - dme.postdiag.total.median, 
                                              dmne.postdiag.total.median)
    ) %>%
    ##########################################################################################
    ### Drop duplicates from the data ########################################################
    ##########################################################################################
    mutate(de.postdiag.median = ifelse(!is.na(de.intens.tx.median) & !is.na(de.cont.tx.median), 
                                     NA, de.postdiag.median),
           dme.postdiag.total.median = ifelse(!is.na(dme.intens.tx.total.median) & 
                                                !is.na(dme.cont.tx.total.median), 
                                            NA, dme.postdiag.total.median), 
           dmne.postdiag.total.median = ifelse(!is.na(dmne.intens.tx.total.median) & 
                                                 !is.na(dmne.cont.tx.total.median), 
                                             NA, dmne.postdiag.total.median), 
           ie.postdiag.lostincome.total.median = ifelse(!is.na(ie.intens.tx.lostincome.total.median) & 
                                                          !is.na(ie.cont.tx.lostincome.total.median), 
                                                      NA, ie.postdiag.lostincome.total.median),
           ### Total TB Care 
           de.tbcare.median = ifelse((!is.na(de.prediag.median) & !is.na(de.postdiag.median)) | 
                                       (!is.na(de.prediag.median)  & !is.na(de.intens.tx.median) & !is.na(de.cont.tx.median)) | 
                                         (!is.na(dme.prediag.total.median) & !is.na(dme.postdiag.total.median) &
                                          !is.na(dmne.prediag.total.median) & !is.na(dmne.postdiag.total.median)), 
                                   NA, de.tbcare.median),
           dme.tbcare.total.median = ifelse((!is.na(dme.prediag.total.median) & 
                                               !is.na(dme.postdiag.total.median)) | 
                                              (!is.na(dme.prediag.total.median)  &
                                                   !is.na(dme.intens.tx.total.median) & 
                                                   !is.na(dme.cont.tx.total.median)), 
                                          NA, dme.tbcare.total.median), 
           dmne.tbcare.total.median = ifelse((!is.na(dmne.prediag.total.median) & !is.na(dmne.postdiag.total.median)) | 
                                               (!is.na(dmne.prediag.total.median)  &
                                                    !is.na(dmne.intens.tx.total.median) & !is.na(dmne.cont.tx.total.median)), 
                                           NA, dmne.tbcare.total.median), 
           ie.tbcare.lostincome.total.median = ifelse((!is.na(ie.prediag.lostincome.total.median) & 
                                                         !is.na(ie.postdiag.lostincome.total.median)) |
                                                        (!is.na(ie.prediag.lostincome.total.median) & 
                                                             !is.na(ie.intens.tx.lostincome.total.median) &
                                                             !is.na(ie.cont.tx.lostincome.total.median)), 
                                                    NA, ie.tbcare.lostincome.total.median),
           ### Only keep direct expenditures if we do not have dme and dmne
           de.prediag.median = ifelse(!is.na(dme.prediag.total.median) & !is.na(dmne.prediag.total.median), 
                                    NA, de.prediag.median),
           de.postdiag.median = ifelse((!is.na(dme.postdiag.total.median) & !is.na(dmne.postdiag.total.median)) |
                                        (!is.na(dme.intens.tx.total.median) & !is.na(dmne.intens.tx.total.median) &
                                         !is.na(dme.cont.tx.total.median) & !is.na(dmne.cont.tx.total.median)), 
                                     NA, de.postdiag.median),
           de.intens.tx.median = ifelse(!is.na(dme.intens.tx.total.median) & !is.na(dmne.intens.tx.total.median), 
                                      NA, de.intens.tx.median),
           de.cont.tx.median = ifelse(!is.na(dme.cont.tx.total.median) & !is.na(dmne.cont.tx.total.median), 
                                    NA, de.cont.tx.median),
           de.tbcare.median = ifelse(!is.na(dme.tbcare.total.median) & !is.na(dmne.tbcare.total.median), 
                                   NA, de.tbcare.median)
    ) %>% 
    pivot_longer(cols=de.tbcare.median:ie.postdiag.lostincome.total.median, 
                 names_to = "category", values_to = "cost", values_drop_na = TRUE) %>%
    ##########################################################################################
    ### Assign standard deviations ###########################################################
    ##########################################################################################
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
           ##########################################################################################
           ### Covariates from the data #############################################################
           ##########################################################################################
           cost = as.numeric(cost)) %>%
           ### Filter to only those that report DRTB or DSTB costs ##################################
           filter(xor(drtb.ind==1, dstb.ind==1)) %>%
    dplyr::select(covidence.id, pop.id, 
                  study.end.year,
                  n.participants, cost, se,
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median, 
                  #### Covariates ####
                  drtb.ind, dstb.ind,
                  drtb, dstb,
                  whohb, urban, public) %>% na.omit() %>% 
###############################################################################
### Remove any of the medians that have the mean estimates
###############################################################################
    filter(!covidence.id %in% modelDataMeans$covidence.id) 


###############################################################################
### Convert the extracted medians to mean estimates
###############################################################################
modelDataMedians$cost <- modelDataMedians$cost * (coeffVar^2+1)^0.5

###############################################################################
### Bind the two datasets together and ensure non-duplicative pop.ids       ###
###############################################################################
modelDataBind <- rbind(modelDataMeans, 
                       modelDataMedians) 

tmpBoth <- modelDataBind %>% 
            group_by(covidence.id) %>% 
            filter(length(unique(pop.id)) == 1 | # Keep those with only one pop.id
                       grepl("Total", pop.id, ignore.case = TRUE) | ### Keep pop.id=Total 
                       grepl("DSTB", pop.id, ignore.case = TRUE) |
                       grepl("DRTB", pop.id, ignore.case = TRUE) |
                       (length(unique(pop.id)) == 2 & 
                            ! "Total" %in% unique(pop.id) & pop.id != "LTBI"))

tmp2Both <- modelDataBind %>% 
            filter((length(unique(pop.id)) > 1) & (! covidence.id %in% tmpBoth$covidence.id)) 

modelData0 <- rbind(tmpBoth, 
                    tmp2Both) %>% 
              filter(covidence.id !="7866")

##############################################################################|
### Check data outliers                                                     ###
##############################################################################|
covIDs <- modelData0 %>% filter(cost > 25000) %>% select(covidence.id) %>%
    unique() %>% unlist() %>% as.vector(); covIDs
# 
# dataDesc %>% filter(covidence.id %in% covIDs)    
# View(modelData0 %>% filter(covidence.id %in% covIDs)) 

modelData <- modelData0 %>% filter(! covidence.id %in% c("9061", "9301", "7443"))
modelData %>% select(covidence.id) %>% unique() %>% nrow()

##############################################################################|
### Center the study.end.year variable                                ###
##############################################################################|
meanStudyYr <- floor(mean(modelData$study.end.year, na.rm=TRUE))
modelData <- modelData %>% 
              mutate(year_z = study.end.year - meanStudyYr)
##############################################################################|
### Add study country into the model data                                   ###
##############################################################################|
modelData <- modelData %>% left_join(dataList$descriptive %>% 
                           select(covidence.id, study.country, Country.Code)) %>% 
                           mutate(study.country=ifelse(study.country == "Vietnam", 
                                                       "Viet Nam", study.country)) %>%
                           ungroup() %>%
                           group_by(study.country) %>%
                           mutate(country.id = cur_group_id()) 

##############################################################################|
### Add country level covariates into the model data                        ###
##############################################################################|
modelData <- left_join(modelData, 
                        oopSpending %>% 
                        mutate(study.end.year = as.numeric(study.end.year)) %>% 
                        rename(oop.perc.che=`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`) %>%
                        select(Country.Code, study.end.year, oop.perc.che))

modelData <-left_join(modelData, 
                       medianIncome %>% 
                       mutate(study.end.year = as.numeric(study.end.year)) %>% 
                       select(study.country, study.end.year, median.income.calc.usd)) %>% 
                       rename(median.income = median.income.calc.usd) %>%
                       mutate(oop.perc.che = as.numeric(oop.perc.che))
                       
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
                        N_country = max(modelData$country.id),
                        country_id = modelData$country.id, 
                        ### Internal covariate
                        drtb_ind= modelData$drtb.ind,
                        survey_year = modelData$year_z,
                        ### External covariates
                        median_income = log(modelData$median.income) - mean(log(modelData$median.income)),
                        oop_perc_che = modelData$oop.perc.che - mean(modelData$oop.perc.che)
                    )

###############################################################################
##### Call the stan models                                                #####
###############################################################################

###############################################################################
### Covariates allowed to vary across  each cost category
###############################################################################

totalModel <- stan(file = "code/inst/tbPatientCosts.stan",
                   data = stanModelData,
                   iter = 1000, chains = 3,
                   warmup=500,
                   control=list(max_treedepth=14))


### Pooled patient costs as mean median income and oop perc and study year
exp(summary(totalModel)[[1]][1:9,"mean"])
# exp(summary(totalModel)[[1]][1:9,"50%"])
# exp(summary(totalModel)[[1]][1:9,"2.5%"])
# exp(summary(totalModel)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
costCategories <- paste(c(rep("dme",3), rep("dnme",3), rep("ie",3)), 
                        c("prediag", "intsvtx", "contntx"), sep="_")
yearCoeffs <- paste0("beta_year_", costCategories)
exp(summary(totalModel)[[1]][yearCoeffs,"mean"])

### Print the estimated impact of drtb
drtbCoeffs <- paste0("beta_drtb_", costCategories)
exp(summary(totalModel)[[1]][drtbCoeffs,"mean"])

###############################################################################
### Covariates have the same effect across cost category
###############################################################################

simpCovMod <- stan(file = "code/inst/tbPatientCosts-simpCov.stan",
                   data = stanModelData,
                   iter = 1000, chains = 3,
                   warmup=500,
                   control=list(max_treedepth=14))


### Pooled patient costs as mean median income and oop perc and study year
exp(summary(simpCovMod)[[1]][1:9,"mean"])
# exp(summary(totalModel)[[1]][1:9,"50%"])
# exp(summary(totalModel)[[1]][1:9,"2.5%"])
# exp(summary(totalModel)[[1]][1:9,"97.5%"])

### Print the estimated impact of study year 
exp(summary(simpCovMod)[[1]]["beta_year","mean"])

### Print the estimated impact of drtb
exp(summary(simpCovMod)[[1]]["beta_drtb","mean"])




