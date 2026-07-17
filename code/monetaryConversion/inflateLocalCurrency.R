

### Apply the function

### Read in the review data (clean data!)
cleanDF <- readRDS(here("data/reviewData/fullDataClean.rds"))[["clean data"]]
### Read in the local currency informaiton 
currencyInfo <- readRDS(here("data/monetaryConversion/localCurrencyInfo.rds"))

### Read in the GDP deflators and format
deflator0 <- read.csv(here("data/monetaryConversion/GDP-deflator/API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_38103.csv"))

deflator <- deflator0 %>% 
    mutate(gdp.def.25 = ifelse(!is.na(X2025),
                               ### Yemen's nearest gdp deflator year is 2018
                               X2025, X2018)) %>% 
    pivot_longer(X1960:X2025, names_to = "Year", values_to = "gdp.def") %>% 
    mutate(end.currency.year.calc = gsub("X", "", Year)) %>% 
    filter(end.currency.year.calc > 2008) %>% 
    rename(study.country = Country.Name,
           country.code = Country.Code) %>% 
    right_join(currencyInfo) %>% 
    select(! (c(Indicator.Name, Indicator.Code, Year))) %>% 
    mutate(inf.ratio = gdp.def.25 / gdp.def)

inflationFactors <- as.vector(unlist(deflator$inf.ratio))
# inflateCurrency <-function(currencyYear,
#                            study.country, 
#                            summaryMeasure = "median"){
#     if (is.na(currencyYear)){
#         infltnFct <- NA
#     } else {
#     ### Setup ###
#     library(priceR)
#     ### Package needs an API key 
#     ### Guidance to setup is here: 
#     ### https://cran.r-project.org/web/packages/priceR/readme/README.html
# 
#     if (nchar(currencyYear) == 4){
#         ### Generate inflation factor ### 
#         if(summaryMeasure == "mean"){
#             print("Option 1")
#             infltnFct <- mean(adjust_for_inflation(1, from_date = as.numeric(currencyYear),
#                                                    country = study.country,
#                                                    to_date = 2024, 
#                                                    # extrapolate_future = TRUE, 
#                                                    extrapolate_future_method = "average", 
#                                                    future_averaging_period = 5), 
#                              na.rm=TRUE) 
#         }
#         if(summaryMeasure == "median"){
#             print("Option 2")
#             infltnFct <- median(adjust_for_inflation(1, from_date = as.numeric(currencyYear),
#                                                      country = study.country, to_date = 2024,
#                                                      # extrapolate_future = TRUE,
#                                                      extrapolate_future_method = 'average', 
#                                                      future_averaging_period = 5), 
#                                na.rm=TRUE)
#         }
#     } else if(nchar(currencyYear) > 4){
#         ### Generate inflation factor ### 
#         if(summaryMeasure == "mean"){
#             print("Option 3")
#             infltnFct <- mean(adjust_for_inflation(1, from_date = as.numeric(paste0(substr(currencyYear, 
#                                                                                start = (nchar(currencyYear)-3), 
#                                                                                stop = nchar(currencyYear)))),
#                                                    country = study.country, to_date = 2024,
#                                                    # extrapolate_future = TRUE, 
#                                                    extrapolate_future_method = "average", 
#                                                    future_averaging_period = 5),
#                              na.rm=TRUE) 
#         }
#         if(summaryMeasure == "median"){
#             print("Option 4")
#             infltnFct <- median(adjust_for_inflation(1, from_date = as.numeric(paste0(substr(currencyYear, 
#                                                                                              start = (nchar(currencyYear)-3), 
#                                                                                              stop = nchar(currencyYear)))),
#                                                      country = study.country, to_date = 2024,
#                                                      # extrapolate_future = TRUE, 
#                                                      extrapolate_future_method = "average", 
#                                                      future_averaging_period = 5),
#                                na.rm=TRUE)
#         }
#     }
# }
# return(infltnFct)
# }
### Empty vector to hold the inflation factors
# inflationFactors <- rep(0, nrow(cleanDF))

### Store the countries available 
# allCountriesAvail <- show_countries()

### Look up and store inflation factors
# for(i in 1:length(inflationFactors)){
#     print(i)
#     print(currencyInfo[i, "currency.code"])
#     print(currencyInfo[i, "currency.year"])
#     if (cleanDF$study.country[i] == "Pakistan"){ 
#         index <- 5
#     } else if (cleanDF$study.country[i] == "South Africa"){ 
#         index<-3
#     } else{
#         index<-1
#     }
#     if(cleanDF$study.country[i] %in% c("Democratic Republic of Congo", "DR Congo")){
#         cntry <- "Congo, Dem. Rep."
#     } else if (cleanDF$study.country[i] == "Vietnam"){
#         cntry <- "Viet Nam"
#     } else if (cleanDF$study.country[i] %in% c("The Gambia", "Gambia")){
#         cntry <- "Gambia, The"
#     } else if (cleanDF$study.country[i] == "Yemen"){
#         cntry <- "Yemen, Rep."
#     } else {
#         cntry <- allCountriesAvail[grep(cleanDF$study.country[i], allCountriesAvail$country_name)[index], "country_name"]
#     }
#     
#     inflationFactors[i] <- inflateCurrency(study.country = cntry,
#                                            currencyYear = currencyInfo[i, "currency.year"])
#     ### Add sleep because API needs it 
#     Sys.sleep(1)
# }

### Save inflation factors 
saveRDS(inflationFactors, here("data/monetaryConversion/inflationLocalFactors.rds"), version=2)
