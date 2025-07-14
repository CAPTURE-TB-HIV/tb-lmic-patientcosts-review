### Setup ###
library(priceR)
Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="")

dataList <- readRDS(here("data/tempCleanData.rds"))
 
### supported currencies 
currencies <- readRDS(here("data/currencyNames.rds"))

### Calculate the mean difference between publication year and currency year
meanPubTime <- mean(as.numeric(dataDesc$publication.year) - as.numeric(endCurrencyYear), na.rm=TRUE)

##############################################################################|
### convert from usd to local currency years          #########################
##############################################################################|
# conversionFactors <- rep(1, 119)
for(i in 107:119){
    print(i)
    if(dataDesc$currency[i] == "USD" & conversionFactors[i]==1){
        if(!is.na(dataDesc$currency.year[i])){
            ## Calculate the index of the currencies dataframe
            index <- grep(dataDesc$study.country[i], currencies[,1])[1]
            ## Calculate the currencies end year 
            # endCurrencyYear[i] <- as.numeric(substr(dataDesc$currency.year[i], 
            #                                         start = (nchar(dataDesc$currency.year[i])-3), 
            #                                         stop = nchar(dataDesc$currency.year[i])))
            if(length(index > 0) & !is.na(index)){
                print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
                            currencies[index, 1]))
                conversionFactors[i] <- convertCurrency(currencyCode = currencies[index, 2],
                                                        currencyYear = dataDesc$currency.year[i],
                                                        summaryMeasure = "median", USD = "from")
            } else if (is.na(index)){
                newCurrency <- replaceCurrency(dataDesc$study.country[i])  
                print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
                                                             newCurrency))
                conversionFactors[i] <- convertCurrency(currencyCode = newCurrency,
                                                        currencyYear = dataDesc$currency.year[i],
                                                        summaryMeasure = "median", USD = "from")
            } else{    
                print(paste("No currency found for", dataDesc$study.country[i]))
            }
        } else if(!is.na(dataDesc$currency.year[i]) & conversionFactors[i]==1){
            index <- grep(dataDesc$study.country[i], currencies[,1])[1]
            ## Calculate the currencies end year 
            # endCurrencyYear[i] <- as.numeric(substr(dataDesc$study.years[i], 
            #                                         start = (nchar(dataDesc$study.years[i])-3), 
            #                                         stop = nchar(dataDesc$study.years[i])))
            if(length(index > 0) & !is.na(index)){
                print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
                            currencies[index, 1]))
                conversionFactors[i] <- convertCurrency(currencyCode = currencies[index, 2],
                                                        currencyYear = dataDesc$study.years[i],
                                                        summaryMeasure = "median", USD = "from")
            } else if (is.na(index)){
                newCurrency <- replaceCurrency(dataDesc$study.country[i])  
                print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
                            newCurrency))
                conversionFactors[i] <- convertCurrency(currencyCode = newCurrency,
                                                        currencyYear = dataDesc$study.years[i],
                                                        summaryMeasure = "median", USD = "from")
            } else {
                print(paste("No currency found for", dataDesc$study.country[i]))
            }
        } else if(conversionFactors[i]==1) {
            index <- grep(dataDesc$study.country[i], currencies[,1])[1]
            ## Calculate the currencies end year 
            # endCurrencyYear[i] <- as.numeric(substr(dataDesc$publication.year[i], 
            #                                         start = (nchar(dataDesc$publication.year[i])-3), 
            #                                         stop = nchar(dataDesc$publication.year[i])))
            if(length(index > 0) & !is.na(index)){
                print(paste("Saving conversion factor from", as.numeric(dataDesc$publication.year[i]) + 2, "USD to", 
                            currencies[index, 1]))
                conversionFactors[i] <- convertCurrency(currencyCode = currencies[index, 2],
                                                        currencyYear = as.numeric(dataDesc$publication.year[i])+2,
                                                        summaryMeasure = "median", USD = "from")
            } else if (is.na(index)){
                newCurrency <- replaceCurrency(dataDesc$study.country[i])  
                print(paste("Saving conversion factor from", as.numeric(dataDesc$publication.year[i])-2, "USD to", 
                            newCurrency))
                conversionFactors[i] <- convertCurrency(currencyCode = newCurrency,
                                                        currencyYear = as.numeric(dataDesc$publication.year[i])-2,
                                                        summaryMeasure = "median", USD = "from")
            } else{    
                print(paste("No currency found for", dataDesc$study.country[i]))
            }
        }
    }
    Sys.sleep(2)
}

saveRDS(conversionFactors, here("data/fromUSDconversionFactors.rds"), version=2)

###problems with 12, 24, 30, 45, 66, 92 (Vietnam), 95, 96, 102, 112
badIndex <- c(12, 24, 30, 45, 66, 92, 95,96,102,112)
dataDesc$currency.year[badIndex]
dataDesc$study.country[badIndex]

###problems 
badIndex2 <- c(13,16,18,20,21,22,24,29,33,35, 53,59, 60,93,103, 106)
dataDesc$currency.year[badIndex2]
dataDesc$study.country[badIndex2]

