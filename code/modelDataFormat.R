### This script formats the data to go into the model. 
### Originally aimed to make this process algorithmic, but in the end, 
### a number of unique cases has created a mixture of algorithmic first 
### passes followed by some individual manual tidying. 

###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)

###############################################################################
##### Read in the model data                                              #####
###############################################################################
dataList <- readRDS(here("data/reviewData/finalData.rds"))
dataInds <- readRDS(here("data/dataIndicators.rds"))

fullData <- dataList$data %>% 
    left_join(dataInds) %>% 
    ### Add in study end year
    left_join(dataList$descriptive)

###############################################################################
##### Read in the covariate data                                          #####
###############################################################################

medianIncome <- readRDS(here("data/covariateData/medianIncome.rds")) %>% distinct()
oopSpending <- readRDS(here("data/covariateData/healthAccounts.rds")) %>% distinct()
uhcIndex <- readRDS(here("data/covariateData/uhcIndex.rds")) %>% distinct()

# fullData %>% 
#     filter(!is.na(n.participants.drtb) & !is.na(n.participants.dstb), 
#            !grepl("DRTB", pop.id), !grepl("DSTB", pop.id)) %>% 
#     select(pop.id)


tmpDRWgts <- fullData %>% 
    filter(!is.na(n.participants.drtb) & !is.na(n.participants.dstb)) %>% 
    
    mutate(n.participants.drtb = as.numeric(n.participants.drtb), 
           n.participants.dstb = as.numeric(n.participants.dstb),
           drtb.wt = n.participants.drtb / (n.participants.drtb + 
                                                n.participants.dstb)) %>% 
    distinct()

# length(unique(tmpDRWgts$covidence.id))

# fullData %>% filter(!covidence.id %in% tmpDRWgts$covidence.id, 
#                     !grepl("DRTB", pop.id), !grepl("DSTB", pop.id), 
#                     grepl("DSTB|DS-TB|sensitive", study.pop.desc)) %>%
#     select(covidence.id, pop.id, study.pop.desc) %>% View()

#### 7814 Total is DSTB 
#### 9516 PTB is DSTB

# fullData %>% filter(!covidence.id %in% tmpDRWgts$covidence.id, 
#                     !grepl("DRTB", pop.id), !grepl("DSTB", pop.id), 
#                     grepl("DRTB|DR-TB|resis", study.pop.desc)) %>%
#     select(covidence.id, pop.id, study.pop.desc) %>% View()

#### 8793 is DRTB 
#### 7760 Total is DRTB 

#### Add in the four above 
tmpDRWgts <- bind_rows(tmpDRWgts, 
                       fullData %>% filter(covidence.id %in% c(9612, 7516, 7420, 7322, 9061, 
                                                               9035, 8975, 9388)) %>% 
                                    mutate(n.participants.drtb = as.numeric(n.participants.drtb), 
                                           n.participants.dstb = as.numeric(n.participants.dstb),
                                            drtb.wt = case_when(covidence.id == 9612 ~ 1, 
                                                                covidence.id == 7516 ~ 0,
                                                                covidence.id == 7420 ~ 0, 
                                                                covidence.id == 7322 ~ 1,
                                                                covidence.id == 9061 ~ 0,
                                                                covidence.id == 9035 ~ 0,
                                                                covidence.id == 8975 ~ 0,
                                                                covidence.id == 9388 ~ 0,
                                                        )),
                       fullData %>% filter(covidence.id %in% c(7814, 9516, 8793, 7760)) %>% 
                           mutate(n.participants.drtb = as.numeric(n.participants.drtb), 
                                  n.participants.dstb = as.numeric(n.participants.dstb),
                                  drtb.wt = case_when(covidence.id == 7814 ~ 0, 
                                                      covidence.id == 9516 ~ 0,
                                                      covidence.id == 8793 ~ 1, 
                                                      covidence.id == 7760 ~ 1
                                ))) %>% 
    mutate(preferredSurvey = 1)

###############################################################################
##### For the remaining surveys that appear to be a mix of DSTB and DRTB  #####
##### we will use the national distribution of RR/MDRTB                   #####
###############################################################################

addSurvs <- read.csv("~/Desktop/addtnlStudies.csv") %>% 
    select(covidence.id) %>% unlist() %>% as.vector()

notifTmp <-
    read.csv("~/Downloads/TB_notifications_2026-04-23.csv") %>% 
          select(country, iso3, year, 
                 unconf_mdr_tx, conf_mdr_tx, conf_xdr_tx, 
                 conf_rrmdr_tx, unconf_rrmdr_tx, 
                 conf_rr_fqr_tx, conf_rr_nfqr_tx, unconf_rr_nfqr_tx, 
                 conf_rrmdr,
                 conf_rr_fqr, conf_rr_nfqr,
                 new_sn, new_sp, new_su, 
                 newrel_f014, newrel_f15plus, newrel_fu, 
                 newrel_m014, newrel_m15plus, newrel_mu, 
                 newrel_sexunk014, newrel_sexunk15plus, 
                 newrel_sexunkageunk
                 ) %>% 
            rowwise() %>% 
            mutate(total_pre2013 = sum(new_sn, new_sp, new_su, na.rm = TRUE),
                   total_2013p = sum(newrel_f014, newrel_f15plus, newrel_fu, 
                                 newrel_m014, newrel_m15plus, newrel_mu, 
                                 newrel_sexunk014, newrel_sexunk15plus, na.rm = TRUE), 
                   ### Here we exploit the behavior of the sum function which returns 0 if NA
                   ### So we know that either total_pre2013 or total_2013p have value 0 so 
                   ### we can just sum to get the denominator for below. 
                   denominator = sum(total_pre2013, total_2013p),
                   rrmdr_frac_pre2013 = sum(unconf_mdr_tx, conf_mdr_tx, conf_xdr_tx, na.rm = TRUE) / 
                                        denominator, 
                   rrmdr_frac_2013_2020  = sum(conf_rrmdr_tx, unconf_rrmdr_tx, na.rm=TRUE) / 
                                        denominator,
                   rrmdr_frac_2020p  = sum(conf_rr_fqr_tx, conf_rr_nfqr_tx, 
                                           unconf_rr_nfqr_tx, na.rm =TRUE) / 
                                        denominator, 

                   ### Similar exploit as above     
                   drtb.wt = sum(rrmdr_frac_pre2013, rrmdr_frac_2013_2020, rrmdr_frac_2020p, na.rm=TRUE), 
                   preferredSurvey = 0) %>%
    rename(study.country = country, 
           study.end.year = year, 
           Country.Code = iso3) %>% 
    right_join(fullData %>%
               filter(covidence.id %in% addSurvs),
               by = c("Country.Code", "study.end.year")) %>% 
    mutate(n.participants.drtb = as.numeric(n.participants.drtb), 
           n.participants.dstb = as.numeric(n.participants.dstb))

weightedDF <- bind_rows(tmpDRWgts, 
                        notifTmp)


###############################################################################
##### Format the model data to be fed into the stan model.                #####
###############################################################################
modelDataMeans<- weightedDF %>% 
    select(covidence.id, 
           study.end.year,
           pop.id, n.participants,
           drtb, dstb, drtb.desc, drtb.wt, 
           whohb, urban, public, 
           ### MEANS
           total.cost.mean, 
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
                       "Adult6H", "Adult3HP", "Child6H", "Child3HP", 
                       "LTBI"),
       ! (pop.id == "Total" & covidence.id == 9422),
       ! (pop.id == "DSTB" & covidence.id == 9061),
       ! (pop.id == "PCF" & covidence.id == 9430)) %>% 
    mutate(pop.id = ifelse(covidence.id %in% c(9516, 9422) & pop.id == "PTB", "Total", pop.id),
           # pop.id = ifelse(covidence.id %in% c(7814, 8092, 9430) & pop.id == "PCF", "Total", pop.id),
           pop.id = ifelse(covidence.id == 8887, "Total", pop.id)) %>%
    ##########################################################################################
### Create a DR-TB indicator  ############################################################
##########################################################################################
mutate(
#     drtb.ind = ifelse(pop.id == "DRTB" | drtb == 1, 1, 0),
#     dstb.ind = ifelse(pop.id == "DSTB" | 
#                           dstb == 1 | 
#                           grepl("all DS-TB|all DSTB|DSTB only|DS-TB only", drtb.desc), 1, 0),
    
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
                                     dmne.prediag.total.mean), 
    #### If we have direct expenditure total during a specific treatment phase 
    #### and either direct medical or direct non-medical
    dmne.intens.tx.total.mean = ifelse(!is.na(de.intens.tx.mean) & 
                                         !is.na(dme.intens.tx.total.mean) &
                                         is.na(dmne.intens.tx.total.mean),
                                     de.intens.tx.mean - dme.intens.tx.total.mean, 
                                     dmne.intens.tx.total.mean), 
    dme.intens.tx.total.mean = ifelse(!is.na(de.intens.tx.mean) & 
                                         !is.na(dmne.intens.tx.total.mean) &
                                         is.na(dme.intens.tx.total.mean),
                                     de.intens.tx.mean - dmne.intens.tx.total.mean, 
                                     dme.intens.tx.total.mean), 
    
    dmne.cont.tx.total.mean = ifelse(!is.na(de.cont.tx.mean) & 
                                         !is.na(dme.cont.tx.total.mean) &
                                         is.na(dmne.cont.tx.total.mean),
                                     de.cont.tx.mean - dme.cont.tx.total.mean, 
                                     dmne.cont.tx.total.mean), 
    dme.cont.tx.total.mean = ifelse(!is.na(de.cont.tx.mean) & 
                                        !is.na(dmne.cont.tx.total.mean) &
                                        is.na(dme.cont.tx.total.mean),
                                    de.cont.tx.mean - dmne.cont.tx.total.mean, 
                                    dme.cont.tx.total.mean), 
    
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
    
    pivot_longer(cols=total.cost.mean:ie.cont.tx.lostincome.total.mean, 
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
       dme.prediag = ifelse(category %in% c("total.cost.mean", "de.tbcare.mean", "dme.tbcare.total.mean", 
                                            "dme.tbcare.mean", "dme.prediag.total.mean"), 1,0), 
       dnme.prediag = ifelse(category %in% c("total.cost.mean","de.tbcare.mean", "dmne.tbcare.total.mean", 
                                             "de.prediag.mean", "dmne.prediag.total.mean"), 1,0), 
       ie.prediag = ifelse(category %in% c("total.cost.mean","ie.tbcare.lostincome.total.mean", "ie.prediag.lostincome.total.mean"), 1,0), 
       dme.intsvtx = ifelse(category %in% c("total.cost.mean","de.tbcare.mean", "dme.tbcare.total.mean", 
                                            "de.postdiag.mean", "dme.postdiag.total.mean",
                                            "de.intens.tx.mean", "dme.intens.tx.total.mean"), 1,0), 
       dnme.intsvtx = ifelse(category %in% c("total.cost.mean","de.tbcare.mean", "dmne.tbcare.total.mean", 
                                             "de.postdiag.mean", "dmne.postdiag.total.mean",
                                             "de.intens.tx.mean","dmne.intens.tx.total.mean"), 1,0), 
       ie.intsvtx = ifelse(category %in% c("total.cost.mean","ie.tbcare.lostincome.total.mean", "ie.postdiag.lostincome.total.mean",
                                           "ie.intens.tx.lostincome.total.mean"), 1,0), 
       dme.contntx = ifelse(category %in% c("total.cost.mean","de.tbcare.mean", "dme.tbcare.total.mean", 
                                            "de.postdiag.mean", "dme.postdiag.total.mean",
                                            "de.cont.tx.mean", "dme.cont.tx.total.mean"), 1,0), 
       dnme.contntx = ifelse(category %in% c("total.cost.mean","de.tbcare.mean", "dmne.tbcare.total.mean", 
                                             "de.postdiag.mean", "dmne.postdiag.total.mean",
                                             "de.cont.tx.mean", "dmne.cont.tx.total.mean"), 1,0), 
       ie.contntx = ifelse(category %in% c("total.cost.mean","ie.tbcare.lostincome.total.mean", "ie.postdiag.lostincome.total.mean",
                                           "ie.cont.tx.lostincome.total.mean"), 1,0), 
       mean = 1, 
       median = 0,
       ##########################################################################################
       ### Covariates from the data #############################################################
       ##########################################################################################
       cost = as.numeric(cost)) %>%
    ### Filter to only those that report DRTB or DSTB costs ##################################
# filter(xor(drtb.ind==1, dstb.ind==1)) %>%
    dplyr::select(covidence.id, pop.id,
                  study.end.year,
                  n.participants, cost, sd, se, 
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median, 
                  #### Covariates ####
                  drtb.wt, 
                  # drtb.ind, dstb.ind,
                  # drtb, dstb,
                  whohb, urban, public) 

##############################################################################|
### Estimate gamma shape parameter for the conversion from means to medians ###
##############################################################################|
coeffVar <- median(modelDataMeans$sd / modelDataMeans$cost, na.rm = TRUE)

modelDataMeans <- modelDataMeans %>% select (!sd) %>% mutate(se = 0) #%>% na.omit()
# modelDataMeans <- modelDataMeans %>% na.omit()

##############################################################################|
### Medians  ##################################################################
##############################################################################|
modelDataMedians <- weightedDF %>%  
    select(covidence.id, 
           study.end.year,
           pop.id, 
           drtb, dstb, drtb.desc, drtb.wt, 
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
                       "Adult6H", "Adult3HP", "Child6H", "Child3HP", 
                       "LTBI"),
       ! (pop.id == "Total" & covidence.id == 9422),
       ! (pop.id == "PCF" & covidence.id == 9430)) %>% 
    mutate(pop.id = ifelse(covidence.id %in% c(9516, 9422) & pop.id == "PTB", "Total", pop.id),
           # pop.id = ifelse(covidence.id %in% c(7814, 8092, 9430) & pop.id == "PCF", "Total", pop.id),
           pop.id = ifelse(covidence.id == 8887, "Total", pop.id) #HIVp 
    ) %>%
    ##########################################################################################
### Create a DR-TB indicator  ############################################################
##########################################################################################
mutate(
    # drtb.ind = ifelse(pop.id == "DRTB" | drtb == 1, 1, 0),
    # dstb.ind = ifelse(pop.id == "DSTB" | 
    #                       dstb == 1 | 
    #                       grepl("all DS-TB|all DSTB|DSTB only|DS-TB only", drtb.desc), 1, 0),
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
                                        dmne.postdiag.total.median), 
    #### If we have direct expenditure total during a specific treatment phase 
    #### and either direct medical or direct non-medical
    dmne.intens.tx.total.median = ifelse(!is.na(de.intens.tx.median) & 
                                           !is.na(dme.intens.tx.total.median) &
                                           is.na(dmne.intens.tx.total.median),
                                       de.intens.tx.median - dme.intens.tx.total.median, 
                                       dmne.intens.tx.total.median), 
    dme.intens.tx.total.median = ifelse(!is.na(de.intens.tx.median) & 
                                          !is.na(dmne.intens.tx.total.median) &
                                          is.na(dme.intens.tx.total.median),
                                      de.intens.tx.median - dmne.intens.tx.total.median, 
                                      dme.intens.tx.total.median), 
    
    dmne.cont.tx.total.median = ifelse(!is.na(de.cont.tx.median) & 
                                         !is.na(dme.cont.tx.total.median) &
                                         is.na(dmne.cont.tx.total.median),
                                     de.cont.tx.median - dme.cont.tx.total.median, 
                                     dmne.cont.tx.total.median), 
    dme.cont.tx.total.median = ifelse(!is.na(de.cont.tx.median) & 
                                        !is.na(dmne.cont.tx.total.median) &
                                        is.na(dme.cont.tx.total.median),
                                    de.cont.tx.median - dmne.cont.tx.total.median, 
                                    dme.cont.tx.total.median), 
) %>%
    ### Study 7866 has total direct costs, but they did not account for any 
    ### accomodation costs so we don't trust the implied DNME so we're going to 
    ### manually remove these totals (they should not have been extracted.)
    mutate(de.prediag.median = ifelse(covidence.id == 7866, 
                                      NA, de.prediag.median), 
           de.tbcare.median = ifelse(covidence.id == 7866, 
                                      NA, de.tbcare.median), 
           de.intens.tx.median = ifelse(covidence.id == 7866,
                                        NA, de.intens.tx.median),
           dmne.tbcare.total.median = ifelse(covidence.id == 7866, 
                                             NA, dmne.tbcare.total.median), 
    #### Study 9612 accidentally had costs removed so readd them 
            dme.postdiag.total.median = ifelse(covidence.id == 9612, 
                                                2696, dme.postdiag.total.median),
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
# filter(xor(drtb.ind==1, dstb.ind==1)) %>%
    dplyr::select(covidence.id, pop.id, 
                  study.end.year,
                  n.participants, cost, se,
                  dme.prediag, dnme.prediag, ie.prediag, 
                  dme.intsvtx, dnme.intsvtx, ie.intsvtx, 
                  dme.contntx, dnme.contntx, ie.contntx, 
                  mean, median, 
                  #### Covariates ####
                  drtb.wt,
                  # drtb.ind, dstb.ind,
                  # drtb, dstb,
                  whohb, urban, public) %>% #na.omit() %>% 
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

###############################################################################
### There are a number of subpopulations that needed to be replaced with    ###
### a weighted average. Due to time constrictions, these are written to a   ###
### CSV, then the columns wt.cost and drtb.wt.new were added. We then       ###
### reupload and replace with "wgtTotal" for pop.id.                        ###
###############################################################################

# modelDataBind %>% group_by(covidence.id) %>%
#                   filter(length(unique(pop.id)) > 1,
#                          ! grepl("^DRTB|DSTB|Total", pop.id)) %>%
                  # write.csv(here("data/reviewData/wgtAverage.csv"))
    

wgtAvPops <- modelDataBind %>% group_by(covidence.id) %>% 
                      filter(length(unique(pop.id)) > 1,
                             ! grepl("^DRTB|DSTB|Total", pop.id)) %>% 
                      select(covidence.id, n.participants) %>% distinct() %>% 
                      reframe(n.participants = sum(as.numeric(n.participants)))

updatedData0 <- read.csv(here("data/reviewData/wgtAverage.csv")) %>%
               select(! c(X, pop.id, cost, drtb.wt, n.participants)) %>%
               rename(cost = wt.cost, drtb.wt = drtb.wt.new) %>%
               distinct() %>% 
               mutate(pop.id = "wgtTotal", 
                      covidence.id = as.character(covidence.id)) %>% 
               left_join(wgtAvPops) %>% 
               filter(! (pop.id == "wgtTotal" & cost == 0))  ## we zeroed costs for all but one pop.id for each survey

#### Replace with these new weighted data 
modelDataBind <- bind_rows(modelDataBind %>% 
                                      filter(!covidence.id %in% updatedData0$covidence.id) %>% 
                                      mutate(n.participants = as.numeric(n.participants)), 
                                  updatedData0) %>% 
    
    ### remove some replicated rows 
                  distinct()

#### Add special survey 7919 which had DSTB weighted 
surv7919 <- read.csv(here("data/surv7919.csv")) %>% 
            select(! c(cost, drtb.wt)) %>% 
            rename(cost = wt.cost, drtb.wt = drtb.wt.new) %>% 
            mutate(covidence.id = as.character(covidence.id))
    

modelDataBind_z <- modelDataBind %>% filter(covidence.id != 7919) %>% 
                   bind_rows(surv7919)

tmpBoth <- modelDataBind_z %>% 
    group_by(covidence.id) %>% 
    filter( 
            length(unique(pop.id)) < 3 | 
            ("DSTB" %in% unique(pop.id) & "DRTB" %in% unique(pop.id) & !grepl("Total", pop.id)) |
            (!"DSTB" %in% unique(pop.id) & !"DRTB" %in% unique(pop.id) & !grepl("Male|Female|First", pop.id)) |
            ### Sepcial cases added in for expediency 
            covidence.id == 9612 & pop.id == "DRTB" |
            covidence.id == 7919 & grepl("DSTB|DRTB", pop.id))

# tmp2Both <- modelDataBind %>% 
#     filter((length(unique(pop.id)) > 1) & (! covidence.id %in% tmpBoth$covidence.id)) 

# modelData0 <- rbind(tmpBoth, 
#                     tmp2Both) %>% 
#     filter(covidence.id !="7866")

modelData <- tmpBoth

##############################################################################|
### Check data outliers and report total included                           ###
##############################################################################|
covIDs <- modelData %>% filter(cost > 25000) %>% select(covidence.id) %>%
    unique() %>% unlist() %>% as.vector()
print(paste("Survey number",covIDs,  "has a cost listed at higher than 25000 INTD:"))

## Remove for now
modelData <- modelData %>% filter(!covidence.id %in% covIDs)

print(paste("We are including data from",
modelData %>% select(covidence.id) %>% unique() %>% nrow(), 
"cost surveys."))

##############################################################################|
### Center the study.end.year variable                                ###
##############################################################################|
meanStudyYr <- floor(mean(modelData$study.end.year, na.rm=TRUE))
modelData <- modelData %>% mutate(year_z = study.end.year - meanStudyYr)

##############################################################################|
### Add study country into the model data                                   ###
##############################################################################|
modelData <- modelData %>% left_join(dataList$descriptive %>% 
                                         select(covidence.id, study.country, Country.Code)) %>% 
    rename(country.code = Country.Code) %>% 
    mutate(study.country=ifelse(study.country == "Vietnam", 
                                "Viet Nam", study.country)) %>%
    ungroup() %>%
    group_by(study.country)


##############################################################################|
### Add country level covariates into the model data                        ###
##############################################################################|
modelData <- left_join(modelData, 
                       oopSpending %>% 
                           mutate(study.end.year = as.numeric(study.end.year)) %>% 
                           rename(oop.perc.che=`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`) %>%
                           select(country.code, study.end.year, oop.perc.che))

modelData <-left_join(modelData, medianIncome %>%
                          rename(country.code = Country.Code) %>%
                          mutate(study.end.year = as.numeric(study.end.year)) %>%
                          select(country.code, study.end.year, median.income.calc.usd), 
                      by = c("study.end.year", "country.code")) %>%
            rename(median.income = median.income.calc.usd) %>%
            mutate(oop.perc.che = as.numeric(oop.perc.che))


modelData <-left_join(modelData, 
                      uhcIndex %>% 
                          mutate(study.end.year = as.numeric(study.end.year)) %>% 
                          select(country.code, study.end.year, UHCindex), 
                      by = c("study.end.year", "country.code"))

##############################################################################|
### Add WHO region into the dataset                                         ###
##############################################################################|

modelData <- left_join(modelData, 
                       read.csv(here("data/who-regions.csv")) %>%
             mutate(who.region = gsub(" \\(WHO\\)", "", World.regions.according.to.WHO)) %>% 
             select(Code, who.region) %>% 
             rename(country.code = Code),
    by="country.code") %>%
    mutate(region.code = as.factor(who.region))
    
    
    length(unique(modelData$covidence.id))
    
    modelData <- modelData %>% filter(grepl("DSTB|DRTB|Total", pop.id), 
                     !pop.id %in% c("UrbSlumTotal", "TeaGardHRTotal")) 

length(unique(modelData$covidence.id))

##############################################################################|
### Manual fixing                                                           ###
##############################################################################|

#### check the dme.prediag model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
           dme.intsvtx = sum(dme.intsvtx),
           dme.contntx = sum(dme.contntx),
           dnme.prediag = sum(dnme.prediag),
           dnme.intsvtx = sum(dnme.intsvtx),
           dnme.contntx = sum(dnme.contntx),
           ie.prediag = sum(ie.prediag),
           ie.intsvtx = sum(ie.intsvtx),
           ie.contntx = sum(ie.contntx),
           .by = c("covidence.id", "pop.id")) %>% 
    filter(dme.prediag > 1) %>% select(covidence.id)

### Fix 8748 which reports total direct costs, but also some but not all 
### of those costs individually. We will substract the known component costs
### and leave the remainder for the other components. 

# tmpModelData <- modelData %>% 
#                 filter(covidence.id == 8748); #View(tmpModelData)
# 
# tmpModelData[1,"cost"] <- tmpModelData[1,"cost"] - sum(tmpModelData[c(2,3,5),"cost"])
# 
# tmpModelData[1,"dme.prediag"] <- tmpModelData[1,"dnme.prediag"] <- 
# tmpModelData[1,"dnme.intsvtx"] <- tmpModelData[1,"dnme.contntx"] <- 0; #View(tmpModelData)
# 
# modelData[which(modelData$covidence.id == 8748),] <- tmpModelData

#View(modelData %>% filter(covidence.id == 8748))
# rm(tmpModelData)

##############################################################################|
#### check the dnme.prediag model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(dnme.prediag > 1) %>% select(covidence.id)

### Fix 9628 ##################################################################
tmpModelData <- modelData %>% 
    filter(covidence.id == 9628); #View(tmpModelData)

## DSTB
tmpModelData[2,"cost"] <- tmpModelData[2,"cost"] - tmpModelData[3,"cost"]
## DRTB 
tmpModelData[5,"cost"] <- tmpModelData[5,"cost"] - tmpModelData[6,"cost"]

# 
tmpModelData[2, "dnme.prediag"] <- 0
tmpModelData[5, "dnme.prediag"] <- 0; #View(tmpModelData)

modelData[which(modelData$covidence.id == 9628),] <- tmpModelData

# View(modelData %>% filter(covidence.id == 9628))
rm(tmpModelData)

##############################################################################|
#### check the dnme.intsvtx model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(dnme.intsvtx > 1) %>% select(covidence.id)

### Fix 8221 ##################################################################
# tmpModelData <- modelData %>% 
#     filter(covidence.id == 8221); #View(tmpModelData)
# 
# tmpModelData[4,"cost"] <- tmpModelData[4,"cost"] - sum(tmpModelData[5,"cost"])
# 
# tmpModelData[4,"dnme.intsvtx"] <- tmpModelData[4,"dnme.contntx"] <- 0; #View(tmpModelData)
# 
# modelData[which(modelData$covidence.id == 8221),] <- tmpModelData

# View(modelData %>% filter(covidence.id == 8221))
# rm(tmpModelData)

##############################################################################|
#### check the dnme.contntx model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(dnme.contntx > 1) %>% select(covidence.id)

### None to fix! 

##############################################################################|
#### check the dme.intsvtx model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(dme.intsvtx > 1) %>% select(covidence.id)

### Fix 7847
tmpModelData <- modelData %>% 
    filter(covidence.id == 7847); #View(tmpModelData)

tmpModelData[1,"cost"] <- tmpModelData[1,"cost"] - sum(tmpModelData[2,"cost"])

tmpModelData[1,"dme.intsvtx"] <- tmpModelData[1,"dme.contntx"] <- 0; #View(tmpModelData)

modelData[which(modelData$covidence.id == 7847),] <- tmpModelData

# View(modelData %>% filter(covidence.id == 7847))
rm(tmpModelData)

##############################################################################|
#### check the dme.contntx model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(dme.contntx > 1) %>% select(covidence.id)

### None to fix!

#### check the indirect costs model data 
modelData %>% ungroup() %>% 
    reframe(dme.prediag = sum(dme.prediag),
            dme.intsvtx = sum(dme.intsvtx),
            dme.contntx = sum(dme.contntx),
            dnme.prediag = sum(dnme.prediag),
            dnme.intsvtx = sum(dnme.intsvtx),
            dnme.contntx = sum(dnme.contntx),
            ie.prediag = sum(ie.prediag),
            ie.intsvtx = sum(ie.intsvtx),
            ie.contntx = sum(ie.contntx),
            .by = c("covidence.id", "pop.id")) %>% 
    filter(ie.prediag > 1 | ie.intsvtx > 1 | ie.contntx > 1) %>% 
    select(covidence.id)

### None to fix! 

### Create a variable for representative
modelData <- modelData %>% 
                mutate(rprsnt = case_when(covidence.id %in% c(9104, 8263) ~ "Inpatient",
                                          covidence.id %in% c(9198,8979,7775) ~ "PLWH", 
                                          .default = "All"))

modelDataRep <- modelData

modelData <- modelData %>% filter(rprsnt == "All")


length(unique(modelData$covidence.id))


##############################################################################|
### Add log median income into the model        ###
##############################################################################|

modelData <- modelData %>% 
             mutate(log.median.income = log(median.income))

##############################################################################|
### Add nat rep
##############################################################################|
modelData <- left_join(modelData, dataInds %>% select(covidence.id, natRep)) %>% 
              mutate(natRep = ifelse(natRep == "National", 1, 0), 
                     whoSurv = ifelse(covidence.id %in% 
                                           ### These are the WHO patient costs surveys 
                                           c(1:4, 8688, 7799, 8781, 7682, 9330,
                                             8270, 9620, 7443,7355, 7919, 9422,
                                             8310, 9585, 8634, 8677, 9301), 1, 0))
##############################################################################|

##############################################################################|
### Add missing participant numbers
##############################################################################|
##############################################################################|

modelData <- modelData %>% 
              mutate(n.participants = case_when(covidence.id == 9627 & pop.id == "DSTB" ~ 118,
                                                     covidence.id == 9627 & pop.id == "DRTB" ~ 143,
                                                     covidence.id == 9323 ~ 126,
                                                     covidence.id == 9035 ~ 425,
                                                     covidence.id == 7643 ~ 260,
                                                     covidence.id == 8634 & pop.id == "DSTB" ~ 1134,
                                                     covidence.id == 8634 & pop.id == "DRTB" ~ 44, 
                                                     covidence.id == 7959 ~ 175, ## could be different for pre-tx cost
                     .default = n.participants), 
##############################################################################|
### Add sample standard error relationship
##############################################################################|
                    stnd.err.wgt = 1 / sqrt (n.participants))

##############################################################################|
### Remove those without pop size 
##############################################################################|

modelData <- modelData %>% filter(!is.na(n.participants))

##############################################################################|
### Add study and country-level identifier into the model data              ###
##############################################################################|
modelData <- modelData %>%
    ungroup() %>%
    group_by(covidence.id) %>%
    mutate(survey.id = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(country.code) %>% 
    mutate(country.id = cur_group_id()) %>% ungroup() %>% 
    ### Force these weights to be 1 or 0
    mutate(drtb.wt = ifelse(pop.id == "DRTB", 1, 
                            ifelse(pop.id == "DSTB", 0, drtb.wt)))
