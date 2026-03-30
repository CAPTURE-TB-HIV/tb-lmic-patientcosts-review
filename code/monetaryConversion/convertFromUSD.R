### This script looks up and stores all conversion factors to
### converts all costs from USD to local currency in either the 
### reported or assumed currency year. 

### Last edited 30 March 2026.

##############################################################################|
### Setup                                             #########################
##############################################################################|
### Load in needed libraries
library(here)

### Load in the conversion function
source(here("code/monetaryConversion/convertCurrencyFunc.R"))

### Load in the clean dataset 
### Can't be final dataset because that has costs converted 
### (i.e. happens after this step)
cleanDF <- readRDS(here("data/reviewData/fullDataClean.rds"))[["clean data"]]

### Read in the local currency information 
currencyInfo <- readRDS(here("data/monetaryConversion/localCurrencyInfo.rds"))

if (sum(currencyInfo$covidence.id != cleanDF$covidence.id) > 0){
    print("Covidence IDs misaligned. Need to check currencyInfo creation or join.")
    break()
}

##############################################################################|
### convert from usd to local currency years          #########################
##############################################################################|
conversionFactors <- rep(1, nrow(cleanDF))

for(i in 1:nrow(cleanDF)){
    print(i)
    ### We only need conversion factors for those costs reported in USD or INTD
    ### We use the new variable end.currency.year.calc to access the years
    ### replaced by study year.
    if(cleanDF$currency[i] == "USD" & !is.na(currencyInfo$end.currency.year.calc[i])){

                print(paste("Saving conversion factor from", currencyInfo$end.currency.year.calc[i], "USD to",
                            currencyInfo[i, "currency.code"]))
                conversionFactors[i] <- convertCurrency(currencyCode = currencyInfo[i, "currency.code"],
                                                        currencyYear = currencyInfo[i, "end.currency.year.calc"],
                                                        summaryMeasure = "median", USD = "from")
                ## API works better with a small pause between calls
                Sys.sleep(2)

    } else{
        print("Study costs reported in local currency.")
    }
}

saveRDS(conversionFactors, here("data/monetaryConversion/fromUSDconversionFactors.rds"), version=2)


