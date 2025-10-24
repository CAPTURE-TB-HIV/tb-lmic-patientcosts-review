###############################################################################
### Creates indicator variables in the data that will be used in the model. 
###############################################################################

###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)

###############################################################################
##### Read in the model data                                              #####
###############################################################################
dataList <- readRDS(here("data/tempCleanData.rds"))

dataDesc <- dataList$descriptive

###############################################################################
##### Create indicators                                                   #####
###############################################################################

dataIndicator <- dataDesc %>% 
           ### Whether the study was conducted in an urban community or 
           ### health facility. 
           mutate(urban = ifelse(study.rurality == "Urban", 
                                 1, 0),
           ### Whether the study was conducted in an rural community or 
           ### health facility. 
                  rural = ifelse(study.rurality == "Rural", 
                                 1, 0), 
           ### Whether the study was conducted in only public 
           ### health facility (ies). 
                  public = ifelse(pub.or.private.hc == "Public", 
                                  1, 0), 
           ### Whether the study used WHO handbook
                  whohb = ifelse(who.handbook == "Yes", 
                                  1, 0), 
           ### Exclusively drug resistant population
                  drtb = ifelse(grepl("DSTB|DS-TB|sensitive", study.exclusion), 
                                  1, 0)
           ) %>%
    
    select(covidence.id, 
           urban, rural, 
           public, whohb, 
           drtb)

saveRDS(dataIndicator, 
        here("data/dataIndicators.rds"), version=2)

                   