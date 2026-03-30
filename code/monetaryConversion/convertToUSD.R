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

### Load in the conversion function
source(here("code/monetaryConversion/convertCurrencyFunc.R"))

### Load in the information about what the local currencies are that we are
### converting from. 
currencyInfo <- readRDS(file = here("data/monetaryConversion/localCurrencyInfo.rds"))

### Empty vector to store conversion factors
convertToUSDFactors <- rep(0, nrow(currencyInfo))

for(i in 1:nrow(currencyInfo)){
    print(i)
    convertToUSDFactors[i] <- convertCurrency(currencyCode = currencyInfo[i,"currency.code"], 
                                            currencyYear = 2024, 
                                            summaryMeasure = "median", USD = "to")
    Sys.sleep(2)
}

saveRDS(convertToUSDFactors, file=here("data/monetaryConversion/toUSDconversionFactors.rds"), version=2)
