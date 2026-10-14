### This script looks up and stores all conversion factors to
### converts all costs from local 2024 to USD 2024 
### Data will have previously been converted to local currency for study year
### Then inflated to 2024 local currency. 

### Last edited 27 March 2026.

##############################################################################|
### Setup                                             #########################
##############################################################################|
### Load in needed libraries
library(here)

### Read in LMIC list 
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
    rename(study.country = country.name) %>%
    select(! class25)

### Load in the information about what the local currencies are that we are
### converting from. 
currencyInfo <- readRDS(file = here("data/monetaryConversion/localCurrencyInfo.rds")) 

### Load in the lcu to international dollar conversion factors 
convertToINTDFactors0 <- read.csv(here("data/monetaryConversion/LCU-INTD/API_PA.NUS.PPP_DS2_en_csv_v2_33039.csv")) %>%
                rename(study.country = Country.Name, 
                       country.code = Country.Code) %>%  
                filter(country.code %in% LMIC$country.code) %>% 
                mutate(conv.fact.25 = case_when(!is.na(`X2025`) ~ `X2025`,
                                                !is.na(`X2024`) ~ `X2024`,
                                                !is.na(`X2022`) ~ `X2022`,
                                                !is.na(`X2021`) ~ `X2021`,
                                                !is.na(`X2020`) ~ `X2020`,
                                                !is.na(`X2019`) ~ `X2019`,
                                                !is.na(`X2013`) ~ `X2013`, 
                                                !is.na(`X2011`) ~ `X2011`)) %>% 
                select(study.country, country.code, conv.fact.25)

convertToINTDFactors <- left_join(currencyInfo, convertToINTDFactors0) %>% 
                        select(conv.fact.25) %>% unlist() %>% as.vector()

saveRDS(convertToINTDFactors, file=here("data/monetaryConversion/toINTDconversionFactors.rds"), version=2)


# intdFact<- data.frame(dataList[["descriptive"]], "ScalarTo2025INTD" = infFactor) %>% 
#     mutate(label = paste(study.country, study.end.year))
# 
# ggplot(data = intdFact) + geom_point(aes(x=label, y=ScalarTo2025INTD)) + theme_minimal() + 
#     coord_flip()
# 
# ggplot(data = intdFact %>% filter(ScalarTo2025INTD > 10)) + geom_point(aes(x=label, y=ScalarTo2025INTD)) + theme_minimal() + 
#     coord_flip() + scale_y_log10()
# 
# ggplot(data = intdFact %>% filter(ScalarTo2025INTD > 100)) + geom_point(aes(x=label, y=ScalarTo2025INTD)) + theme_minimal() + 
#     coord_flip() + scale_y_log10()
