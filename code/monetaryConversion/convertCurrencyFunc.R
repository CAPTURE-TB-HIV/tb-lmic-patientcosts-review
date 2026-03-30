### This script includes a function to convert to or from USD
### It should be sourced into two scripts: 
### 1. convertFromUSD.R
### 2. convertToUSD.R

### Convert currencies to or from USD
convertCurrency <-function(currencyCode, 
                           currencyYear,
                           summaryMeasure = "median", 
                           USD = "from") {
    library(priceR)
    ### Package needs an API key 
    ### Guidance to setup is here: 
    ### https://cran.r-project.org/web/packages/priceR/readme/README.html
    
    if (USD=="to"){
    if (currencyCode %in% c("USD", "INTD")){
        cnvsnFct <- 1
    } else if (is.na(currencyYear)){
        cnvsnFct <- NA
    } else {
        if (currencyCode == "RMB") currencyCode <- "CNY"
        if (currencyCode == "SLR") currencyCode <- "LKR"
        if (nchar(currencyYear) == 4){
            ### Generate conversion factor ### 
            if(summaryMeasure == "mean"){
                cnvsnFct <- mean(historical_exchange_rates(currencyCode, to = "USD",
                                          start_date = paste0(currencyYear, "-01-01"), 
                                          end_date = paste0(currencyYear, "-11-30"))[,2], 
                                 na.rm=TRUE) 
            }
            if(summaryMeasure == "median"){
                if (currencyCode != "MNT"){
                cnvsnFct <- median(historical_exchange_rates(currencyCode, to = "USD",
                                               start_date = paste0(currencyYear, "-01-01"), 
                                               end_date = paste0(currencyYear, "-11-30"))[,2], 
                            na.rm=TRUE)
                } else {
                ### Mongolia has slightly less 2024 data
                cnvsnFct <- median(historical_exchange_rates(currencyCode, to = "USD",
                                                             start_date = paste0(currencyYear, "-01-01"), 
                                                             end_date = paste0(currencyYear, "-11-11"))[,2], 
                                   na.rm=TRUE)
                }
                
            }
        } else if(nchar(currencyYear) > 4){
            ### Generate conversion factor ### 
            if(summaryMeasure == "mean"){
                cnvsnFct <- mean(historical_exchange_rates(currencyCode, to = "USD",
                                                           start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                           end_date = paste0(substr(currencyYear, 
                                                                                    start = (nchar(currencyYear)-3), 
                                                                                    stop = nchar(currencyYear)), "-11-30"))[,2], 
                                 na.rm=TRUE) 
            }
            if(summaryMeasure == "median"){
                cnvsnFct <- median(historical_exchange_rates(currencyCode, to = "USD",
                                                             start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                             end_date = paste0(substr(currencyYear, 
                                                                                      start = (nchar(currencyYear)-3), 
                                                                                      stop = nchar(currencyYear)), "-11-30"))[,2], 
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
                ### Generate conversion factor ### 
                if(summaryMeasure == "mean"){
                    cnvsnFct <- mean(historical_exchange_rates(from="USD", to = currencyCode,
                                                               start_date = paste0(currencyYear, "-01-01"), 
                                                               end_date = paste0(currencyYear, "-11-30"))[,2], 
                                     na.rm=TRUE) 
                }
                if(summaryMeasure == "median"){
                    cnvsnFct <- median(historical_exchange_rates(from="USD", to = currencyCode,
                                                                 start_date = paste0(currencyYear, "-01-01"), 
                                                                 end_date = paste0(currencyYear, "-11-30"))[,2], 
                                       na.rm=TRUE)
                }
            } else if(nchar(currencyYear) > 4){
                ### Generate conversion factor ### 
                if(summaryMeasure == "mean"){
                    cnvsnFct <- mean(historical_exchange_rates(from="USD", to = currencyCode,
                                                               start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                               end_date = paste0(substr(currencyYear, 
                                                                                        start = (nchar(currencyYear)-3), 
                                                                                        stop = nchar(currencyYear)), "-11-30"))[,2], 
                                     na.rm=TRUE) 
                }
                if(summaryMeasure == "median"){
                    cnvsnFct <- median(historical_exchange_rates(from="USD", to = currencyCode,
                                                                 start_date = paste0(substr(currencyYear, start = 1, stop = 4), "-01-01"), 
                                                                 end_date = paste0(substr(currencyYear, 
                                                                                          start = (nchar(currencyYear)-3), 
                                                                                          stop = nchar(currencyYear)), "-11-30"))[,2], 
                                       na.rm=TRUE)
                }
            }
        }
    }
    
    return(cnvsnFct)
}


