medianIncomePrediction <- readRDS("~/Library/CloudStorage/OneDrive-HarvardUniversity/Desktop/nis100/tbPatientCosts/tb-lmic-patientcosts-review/data/covariateData/medianIncomePrediction.rds")

medianIncomePrediction2 <- medianIncomePrediction  %>% 
    group_by(who.region, study.end.year) %>% 
    ### set to NA so not included in the median calculation
    mutate(
           median.income.calc.intd.2025.rev = ifelse(GNI.desc != "Regional", 
                                                     median.income.calc.intd.2025, 
                                                     median(median.income.calc.intd.2025, na.rm=TRUE))) %>%
    ungroup() 

medianIncomePrediction3 <- medianIncomePrediction  %>% 
    group_by(who.region, study.end.year) %>% 
    ### set to NA so not included in the median calculation
    mutate(median.income.calc.intd.2025.rev0 = ifelse(GNI.desc != "Regional", 
                                                 median.income.calc.intd.2025, 
                                                 NA),
           median.income.calc.intd.2025.rev = ifelse(GNI.desc != "Regional", 
                                                     median.income.calc.intd.2025, 
            median(median.income.calc.intd.2025.rev0, na.rm=TRUE))) %>%
    ungroup() 



medianIncomePrediction4 <- medianIncomePrediction  %>% 
    ### set to NA so not included in the median calculation
    mutate(median.income.calc.intd.2025.rev0 = ifelse(GNI.desc != "Regional", 
                                                      median.income.calc.intd.2025, 
                                                      NA)) %>% 
    group_by(who.region, study.end.year) %>% 
    mutate(
           median.income.calc.intd.2025.regional = median(median.income.calc.intd.2025.rev0, na.rm=TRUE),
           median.income.calc.intd.2025.rev = ifelse(GNI.desc != "Regional", 
                                                    median.income.calc.intd.2025, 
                                                    median.income.calc.intd.2025.regional)) %>%
    ungroup()  

range(medianIncomePrediction2$median.income.calc.intd.2025.rev, na.rm=T)

range(medianIncomePrediction3$median.income.calc.intd.2025.rev, na.rm=T)
range(medianIncomePrediction4$median.income.calc.intd.2025.rev, na.rm=T)
