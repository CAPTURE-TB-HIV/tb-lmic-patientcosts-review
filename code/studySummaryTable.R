###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)

###############################################################################
##### Read in the model data 
###############################################################################
dataList <- readRDS(here("data/reviewData/finaldata.rds"))

dataDesc0 <- dataList[["descriptive"]]

dataInds <- readRDS(here("data/dataIndicators.rds"))

fullData <- dataList$data %>% 
    left_join(dataInds) %>% 
    ### Add in study end year
    left_join(dataList$descriptive)

###############################################################################
##### Add any additional rows of data
###############################################################################
### Fill in study id for any missing rows 

dataDesc <- dataDesc0 %>% 
            mutate(study.id.table = paste(study.country, 
                                          study.years), 
                   author.pub.year = paste(first.author.surname, 
                                           publication.year)) %>% 
                    left_join(dataInds)

dataList <- readRDS(here("data/reviewData/finalData.rds"))
dataInds <- readRDS(here("data/dataIndicators.rds"))

fullData <- dataList$data %>% 
    left_join(dataInds) %>% 
    ### Add in study end year
    left_join(dataList$descriptive)dme.components <- c("consult", "diagtest", "meds", 
                    "admin", "outptnt", "hospital")

dnme.components <- c("transport", "accomod", "food", "caregiving", 
                     "nutritionsup", "childcare")

# ie.components <- c("Patient", "Guardian", "Caregiver", "ouseold")

costComponents <- dataList$data %>%
                  select(covidence.id, contains(dme.components))
                  select_if(colSums(!is.na(.)) > 0)

dataList$data %>% mutate(report.consult = ifelse(!is.na(grepl(dme.components[1])), 
                                                  "Yes", "No"))

###############################################################################
##### Add any additional rows of data
###############################################################################
modelCovIDs <- modelData %>% select(covidence.id) %>% distinct()

studyDF <- dataDesc %>%
           select(covidence.id, 
                  study.id.table, 
                  study.setting, 
                  study.rurality,
                  n.sites, 
                  recruit.sites, 
                  who.handbook, 
                  study.pop.desc, 
                  study.exclusion,
                  author.pub.year) %>% 
          mutate("Included in model" = ifelse(covidence.id %in% modelCovIDs, 
                                       "Yes", "No")) %>% 
        distinct() #%>% 


write.csv(studyDF, "~/Desktop/tempPtCstTable.csv")


modelData %>% select(covidence.id, pop.id, cost, 
                    contains("dme"), contains("dnme"), contains("ie"), 
                    drtb.wt, 
                    study.end.year, 
                    median.income, 
                    oop.perc.che, 
                    UHCindex) %>% left_join(studyDF %>% select(covidence.id, study.id.table)) %>%
                                                               write.csv("~/Desktop/model-data.csv")



