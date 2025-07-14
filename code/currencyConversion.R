convertCurrency <-function(currencyCode, 
                           currencyYear,
                           summaryMeasure = "median", 
                           USD = "from") {
    if (USD=="to"){
    if (currencyCode %in% c("USD", "INTD")){
        cnvsnFct <- 1
    } else if (is.na(currencyYear)){
        cnvsnFct <- NA
    } else {
        if (currencyCode == "RMB") currencyCode <- "CNY"
        if (currencyCode == "SLR") currencyCode <- "LKR"
        if (nchar(currencyYear) == 4){
            ### Setup ###
            library(priceR)
            #### REPLACE WITH ACCESS KEY
            Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="")
            ### Generate conversion factor ### 
            if(summaryMeasure == "mean"){
                cnvsnFct <- mean(historical_exchange_rates(currencyCode, to = "USD",
                                          start_date = paste0(currencyYear, "-01-01"), 
                                          end_date = paste0(currencyYear, "-12-31"))[,2], 
                                 na.rm=TRUE) 
            }
            if(summaryMeasure == "median"){
                cnvsnFct <- median(historical_exchange_rates(currencyCode, to = "USD",
                                               start_date = paste0(currencyYear, "-01-01"), 
                                               end_date = paste0(currencyYear, "-12-31"))[,2], 
                            na.rm=TRUE)
            }
        } else if(nchar(currencyYear) > 4){
            ### Generate conversion factor ### 
            if(summaryMeasure == "mean"){
                cnvsnFct <- mean(historical_exchange_rates(currencyCode, to = "USD",
                                                           start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                           end_date = paste0(substr(currencyYear, 
                                                                                    start = (nchar(currencyYear)-3), 
                                                                                    stop = nchar(currencyYear)), "-12-31"))[,2], 
                                 na.rm=TRUE) 
            }
            if(summaryMeasure == "median"){
                cnvsnFct <- median(historical_exchange_rates(currencyCode, to = "USD",
                                                             start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                             end_date = paste0(substr(currencyYear, 
                                                                                      start = (nchar(currencyYear)-3), 
                                                                                      stop = nchar(currencyYear)), "-12-31"))[,2], 
                                   na.rm=TRUE)
            }
        }
    }
    } else if (USD == "from"){
        if (currencyCode == "USD"){
            cnvsnFct <- 1
        } else if (is.na(currencyYear)){
            cnvsnFct <- NA
        } else {
            if (currencyCode == "RMB") currencyCode <- "CNY"
            if (currencyCode == "SLR") currencyCode <- "LKR"
            if (nchar(currencyYear) == 4){
                ### Setup ###
                library(priceR)
                ### replace with access key to api
                Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="")
                ### Generate conversion factor ### 
                if(summaryMeasure == "mean"){
                    cnvsnFct <- mean(historical_exchange_rates(from="USD", to = currencyCode,
                                                               start_date = paste0(currencyYear, "-01-01"), 
                                                               end_date = paste0(currencyYear, "-12-31"))[,2], 
                                     na.rm=TRUE) 
                }
                if(summaryMeasure == "median"){
                    cnvsnFct <- median(historical_exchange_rates(from="USD", to = currencyCode,
                                                                 start_date = paste0(currencyYear, "-01-01"), 
                                                                 end_date = paste0(currencyYear, "-12-31"))[,2], 
                                       na.rm=TRUE)
                }
            } else if(nchar(currencyYear) > 4){
                ### Generate conversion factor ### 
                if(summaryMeasure == "mean"){
                    cnvsnFct <- mean(historical_exchange_rates(from="USD", to = currencyCode,
                                                               start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                               end_date = paste0(substr(currencyYear, 
                                                                                        start = (nchar(currencyYear)-3), 
                                                                                        stop = nchar(currencyYear)), "-12-31"))[,2], 
                                     na.rm=TRUE) 
                }
                if(summaryMeasure == "median"){
                    cnvsnFct <- median(historical_exchange_rates(from="USD", to = currencyCode,
                                                                 start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                                 end_date = paste0(substr(currencyYear, 
                                                                                          start = (nchar(currencyYear)-3), 
                                                                                          stop = nchar(currencyYear)), "-12-31"))[,2], 
                                       na.rm=TRUE)
                }
            }
        }
    }
    
    return(cnvsnFct)
}

# ### Calculate the conversion factor and add it to the 
# conversionFactors <- rep(0,119)
# # 25, 56, 73, 88, 89, 105, 118
# index <- 1:119
# # index <- index[-c(5, 25, 56, 73,88, 89, 105, 118)]
# ### Apply and save conversion factors 
# 
# ### Calculate the mean difference between publication year and currency year
# endCurrencyYear <- rep(NA,119)
# for (i in 1:119){
#     if (!is.na(dataDesc$currency.year[i])){
#     endCurrencyYear[i] <- as.numeric(substr(currencyYear, 
#                                          start = (nchar(dataDesc$currency.year[i])-3), 
#                                          stop = nchar(dataDesc$currency.year[i])))
#     }  else if (!is.na(dataDesc$study.years[i])){
#         endCurrencyYear[i] <- as.numeric(substr(currencyYear, 
#                                                 start = (nchar(dataDesc$study.years[i])-3), 
#                                                 stop = nchar(dataDesc$study.years[i])))
#     }
# }
# meanPubTime <- mean(as.numeric(dataDesc$publication.year) - as.numeric(endCurrencyYear), na.rm=TRUE)
# 
# for(i in index){
#     print(i)
#     print(dataDesc$currency[i])
#     if (!is.na(dataDesc$currency.year[i])){
#         print(paste("currency year(s) = ", dataDesc$currency.year[i]))
#         conversionFactors[i] <- convertCurrency(currencyCode = dataDesc$currency[i],
#                                                   currencyYear = endCurrencyYear[i])
#     } else if (!is.na(dataDesc$study.years[i])){
#         print(paste("study year(s) = ", dataDesc$study.years[i]))
#         conversionFactors[i] <- convertCurrency(currencyCode = dataDesc$currency[i],
#                                                 currencyYear = endCurrencyYear[i])
#     } else {
#         print(paste("publication year = ", dataDesc$publication.year[i]))
#         conversionFactors[i] <- convertCurrency(currencyCode = dataDesc$currency[i],
#                                                 currencyYear = as.numeric(dataDesc$publication.year[i]) - round(meanPubTime))
#     }
#     ### Add sleep because API needs it 
#     Sys.sleep(2)
# }
# 
# saveRDS(conversionFactors, here("data/conversionFactors.rds"), version=2)
# 
# 
# ### supported currencies 
# currencies <- readRDS(here("data/currencyNames.rds"))
# 
# for (i in 1:119){
#     print(i)
#     print(dataDesc$study.country[i])
#     if (dataDesc$currency[i] == "USD"){
#         print(currencies[grep(dataDesc$study.country[i], currencies[,1]),1])
#     } else {
#         print(dataDesc$currency[i])
#     }
# }
# ##############################################################################|
# ### Replace missing currencies based on study country #########################
# ##############################################################################|
replaceCurrency <- function(study.country){
    if (study.country == "Cameroon") currency <- "XAF"
    if (study.country == "China") currency <- "CNY"
    if (study.country == "Thailand") currency <- "THB"
    if (study.country == "Philippines") currency <- "PHP"
    if (study.country == "Viet Nam") currency <- "VND"
    if (study.country == "Timor-Leste") currency <- "USD"
    if (study.country == "Mali") currency <- "MLF"
    if (study.country == "Eswatini") currency <- "SZL"
    if (study.country == "Democratic Republic of Congo") currency <- "CDF"
    if (study.country == "Burkina Faso") currency <- "XOF"
    if (study.country == "The Gambia") currency <- "GMD"
    if (study.country == "Lao PDR") currency <- "LAK"
    if (study.country == "Burkina Faso") currency <- "XOF"
    return(currency)
}

