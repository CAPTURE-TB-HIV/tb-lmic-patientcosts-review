###############################################################################
### Creates indicator variables in the data that will be used in the model. 
###############################################################################

###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)
library(reshape2)
library(forcats)

###############################################################################
##### Read in the model data                                              #####
###############################################################################
dataList <- readRDS(here("data/reviewData/finalData.rds"))

dataDesc <- left_join(dataList$descriptive, 
                  dataList$data %>% select(covidence.id, drtb.desc) %>% distinct())

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
           ### Whether the study was conducted in only private 
           ### health facility (ies). 
                  private = ifelse(pub.or.private.hc == "Private", 
                           1, 0), 
           ### Whether the study used WHO handbook
                  whohb = ifelse(who.handbook == "Yes", 
                                  1, 0), 
           ### Exclusively drug resistant population
                  drtb = ifelse(grepl("DSTB|DS-TB|sensitive", study.exclusion), 
                                  1, 0), 
           ### Exclusively drug sensitive population
                  dstb = ifelse(grepl("resistant", study.exclusion), 
                         1, 0), 
           ### Free TB treatment available
           
           ### Cost components 
                  report.consult  = ifelse(grepl("Consult", dme.desc), 
                                          1, 0),   
                  report.diagtest = ifelse(grepl("Diag", dme.desc), 
                                          1, 0),   
                  report.meds = ifelse(grepl("Med", dme.desc), 
                                           1, 0),   
                  report.admin  = ifelse(grepl("Admin", dme.desc), 
                                            1, 0),   
                  report.outpt = ifelse(grepl("Outpatient", dme.desc), 
                                            1, 0),   
                  report.hospital = ifelse(grepl("Hospital", dme.desc), 
                                        1, 0),   
                  report.transport  = ifelse(grepl("Transport", dmne.desc), 
                                            1, 0),   
                  report.accomod = ifelse(grepl("Accommodation", dmne.desc), 
                                            1, 0),   
                  report.food = ifelse(grepl("Food", dmne.desc), 
                                        1, 0),   
                  report.nutrition = ifelse(grepl("Nutrition", dmne.desc), 
                                              1, 0),   
                  report.caregiving = ifelse(grepl("Caregiving", dmne.desc), 
                                           1, 0),   
                  report.relocation = ifelse(grepl("Relocat", dmne.desc, ignore.case = TRUE), 
                                        1, 0),   
                  report.hygiene = ifelse(grepl("hygiene", dmne.desc, ignore.case = TRUE) |
                                          grepl("extra health products", dmne.desc, ignore.case = TRUE), 
                                          1, 0),  
           natRep = ifelse(grepl("National", study.setting), 
                           "National", "Subnational")
           ) %>% 
    
    select(covidence.id, study.country,
           dme.desc, 
           report.consult, report.diagtest, report.meds, 
           report.admin, report.outpt, report.hospital, 
           dmne.desc, 
           report.transport, report.accomod, report.food, 
           report.nutrition, report.caregiving, report.relocation, 
           report.hygiene,
           urban, rural, 
           public, private, whohb, 
           drtb, dstb, natRep,
           publication.year, study.end.year)

saveRDS(dataIndicator, 
        here("data/dataIndicators.rds"), version=2)


#### Component cost heat map 

## Need to scale by the number of studies published that year
nCompYear <- dataIndicator %>% ungroup() %>%
             select(! c(dme.desc, dmne.desc, urban, rural, 
                         public, whohb, drtb, publication.year)) %>%
             rename("Hygiene products" = report.hygiene, 
                    "Relocation" = report.relocation, 
                    "Administration" = report.admin, 
                    "Outpatient care" = report.outpt, 
                    "Caregiving" = report.caregiving, 
                    "Nutritional supplements" = report.nutrition, 
                    "Hospitalization" = report.hospital, 
                    "Consulation" = report.consult, 
                    "Accomodation" = report.accomod, 
                    "Diagnostic tests" = report.diagtest, 
                    "Medication" = report.meds, 
                    "Food" = report.food, 
                    "Transportation" = report.transport) %>%
             melt(id.vars = c("covidence.id", "study.end.year")) %>%
             reframe(N = sum(value), .by = c(study.end.year, 
                                             variable)) %>% 
             mutate(variable = fct_reorder(variable, N)) 

nTotalYear <-  dataIndicator %>% ungroup() %>%
              reframe(TotalSurveys = n(), .by = c(study.end.year))

nCompYear <- left_join(nCompYear, nTotalYear) %>% 
             mutate(percStudies = N/TotalSurveys * 100) %>% 
             mutate(variable = fct_reorder(variable, -percStudies))

ggplot(nCompYear, aes(x = study.end.year, y = variable,  
                      fill = percStudies)) + 
    geom_tile() + ylab("")+ xlab("Year") + 
    theme_minimal() 

##### Compare by WHO handbook and not WHO handbook 
nCompYear_WHB <- dataIndicator %>% ungroup() %>%
    filter(whohb == 1) %>%
     select(! c(dme.desc, dmne.desc, urban, rural, 
               public, whohb, drtb, publication.year)) %>%
    rename("Hygiene products" = report.hygiene, 
           "Relocation" = report.relocation, 
           "Administration" = report.admin, 
           "Outpatient care" = report.outpt, 
           "Caregiving" = report.caregiving, 
           "Nutritional supplements" = report.nutrition, 
           "Hospitalization" = report.hospital, 
           "Consulation" = report.consult, 
           "Accomodation" = report.accomod, 
           "Diagnostic tests" = report.diagtest, 
           "Medication" = report.meds, 
           "Food" = report.food, 
           "Transportation" = report.transport) %>%
    melt(id.vars = c("covidence.id", "study.end.year")) %>%
    reframe(N = sum(value), .by = c(study.end.year, 
                                    variable)) %>% 
    mutate(variable = fct_reorder(variable, N)) 


nTotalYear_WHB <-  dataIndicator %>% 
    filter(whohb == 1) %>% ungroup() %>%
    reframe(TotalSurveys = n(), .by = c(study.end.year))

nCompYear_WHB <- left_join(nCompYear_WHB, nTotalYear_WHB) %>% 
    mutate(`Fraction of\nsurveys` = N/TotalSurveys) %>% 
    mutate(variable = fct_reorder(variable, -`Fraction of\nsurveys`))

ggplot(nCompYear_WHB, aes(x = study.end.year, y = variable,  
                          fill = `Fraction of\nsurveys`)) + 
    geom_tile() + ylab("")+ xlab("Survey end year") + 
    theme_minimal(base_size = 16) + 
    ggtitle("Used WHO Handbook") +
    scale_x_continuous(breaks = seq(2010, 2023, 1)) + 
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "line"))  + 
    scale_fill_gradient(high="lightblue",low= "dodgerblue4")


###############################################################################

nCompYear_noWHB <- dataIndicator %>% ungroup() %>%
    filter(whohb == 0) %>%
    select(! c(dme.desc, dmne.desc, urban, rural, 
               public, whohb, drtb, publication.year)) %>%
    rename("Hygiene products" = report.hygiene, 
           "Relocation" = report.relocation, 
           "Administration" = report.admin, 
           "Outpatient care" = report.outpt, 
           "Caregiving" = report.caregiving, 
           "Nutritional supplements" = report.nutrition, 
           "Hospitalization" = report.hospital, 
           "Consulation" = report.consult, 
           "Accomodation" = report.accomod, 
           "Diagnostic tests" = report.diagtest, 
           "Medication" = report.meds, 
           "Food" = report.food, 
           "Transportation" = report.transport) %>%
    melt(id.vars = c("covidence.id", "study.end.year")) %>%
    reframe(N = sum(value), .by = c(study.end.year, 
                                    variable)) %>% 
    mutate(variable = fct_reorder(variable, N)) 


nTotalYear_noWHB <-  dataIndicator %>% 
    filter(whohb == 0) %>% ungroup() %>%
    reframe(TotalSurveys = n(), .by = c(study.end.year))

nCompYear_noWHB <- left_join(nCompYear_noWHB, nTotalYear_noWHB) %>% 
    mutate(`Fraction of\nsurveys` = N/TotalSurveys) %>% 
    mutate(variable = factor(variable, 
                             levels = levels(nCompYear_WHB$variable)))

ggplot(nCompYear_noWHB, aes(x = study.end.year, y = variable,  
                          fill = `Fraction of\nsurveys`)) + 
    geom_tile() + ylab("")+ xlab("Survey end year") + 
    theme_minimal(base_size = 16) + 
    ggtitle("Did not use WHO Handbook") +
    scale_x_continuous(breaks = seq(2010, 2020, 1)) + 
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "line")) + 
    scale_fill_gradient(high="lightblue",low= "dodgerblue4")

###############################################################################

countryYearWHOHandbook<- dataIndicator %>% 
    group_by(study.country, study.end.year, whohb) %>% 
    count() %>% 
    rename(`Number of surveys` = n) %>% 
    mutate(`Used WHO handbook` = ifelse(whohb==1, "Yes", "No"))


ggplot(countryYearWHOHandbook) + geom_point(aes(x=study.end.year, 
                                        y=study.country, 
                                        color =`Used WHO handbook`, 
                                        size=`Number of surveys`)) + 
    theme_minimal(base_size = 16) + 
    scale_y_discrete(limits = rev) + 
    scale_size_continuous(range = c(3,7)) +
    xlab("Survey end year") + 
    ylab("") + 
    scale_x_continuous(breaks = seq(2010, 2023, 1)) + 
    theme(legend.position = "bottom",
          legend.box="vertical") +
    scale_color_discrete(type = myPal[c(9,4)])
                   