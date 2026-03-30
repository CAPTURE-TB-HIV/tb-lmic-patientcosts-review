### Setup data for monetary conversion 

### Load in the clean dataset 
### Can't be final dataset because that has costs converted 
### (i.e. happens after this step)
cleanDF <- readRDS(here("data/reviewData/fullDataClean.rds"))[["clean data"]] 

##############################################################################|
### Helper function to replace missing currencies based on study country ######
##############################################################################|
replaceCurrency <- function(study.country){
    if (study.country == "Cameroon") currency <- "XAF"
    if (study.country == "China") currency <- "CNY"
    if (study.country == "Thailand") currency <- "THB"
    if (study.country == "Philippines") currency <- "PHP"
    if (study.country == "Viet Nam") currency <- "VND"
    if (study.country == "Timor-Leste") currency <- "USD"
    if (study.country == "Mali") currency <- "XOF"
    if (study.country == "Eswatini") currency <- "SZL"
    if (study.country %in% c("Democratic Republic of Congo", "DR Congo")) currency <- "CDF"
    if (study.country == "Burkina Faso") currency <- "XOF"
    if (study.country %in% c("The Gambia", "Gambia")) currency <- "GMD"
    if (study.country == "Lao PDR") currency <- "LAK"
    if (study.country == "Burkina Faso") currency <- "XOF"
    if (study.country == "Fiji") currency <- "FJD"
    if (study.country == "Indonesia") currency <- "IDR"
    if (study.country == "Mongolia") currency <- "MNT"
    if (study.country == "Nigeria") currency <- "NGN"
    return(currency)
}

### Create a datafile that represents local currencies by study country. 
### These aren't necessarily the currency reported in the study, but if
### we need to convert from USD to local currency, this is what we will use. 
### Comment out its generation unless it needs to be updated. 

### Read in the supported currencies in priceR 
currencies <- readRDS(here("data/monetaryConversion/currencyNames.rds"))

### Empty vector to hold currency codes
currencyCode <- rep(NA, nrow(cleanDF))

### Store currency codes for each study. Indexing will be used in other
### scripts. 

for(i in 1:nrow(cleanDF)){
    print(i)
    if(cleanDF$currency[i] %in% c("USD", "INTD")){
        index <- grep(cleanDF$study.country[i], currencies[,1])[1]
        if(length(index > 0) & !is.na(index)){
            currencyCode[i] <- currencies[index, 2]
        } else if (is.na(index)){
            currencyCode[i] <- replaceCurrency(cleanDF$study.country[i])  
        } else {
            currencyCode[i] <- "USD"
        }
    } else {
        currencyCode[i] <- cleanDF$currency[i]
    }
    
    ### Replace all RMB with CNY 
    if(currencyCode[i] == "RMB") currencyCode[i] <- "CNY"
}

### Currency year 
cleanDF <- cleanDF %>%
    rowwise() %>%
    ### Create a variable for the end of the currency year range 
    mutate(end.currency.year = as.numeric(substr(currency.year, 
                                                 start = (nchar(currency.year)-3),
                                                 stop = nchar(currency.year))),
           end.study.year = as.numeric(substr(study.years, 
                                              start = (nchar(study.years)-3),
                                              stop = nchar(study.years))),
           ### Calculate the differences in the reported years to understand the 
           ### relationship between currency year and others. Use this info to 
           ### determine which assumption to make 
           diffPubCurrency = as.numeric(publication.year) - as.numeric(end.currency.year), 
           diffStudyCurrency = as.numeric(end.study.year) - as.numeric(end.currency.year))

### Check the relationship of differences 
plot(cleanDF$diffPubCurrency)
points(cleanDF$diffStudyCurrency, col="red")

### The overwhelming majority have the end currency year as end study year. 
### Some may use all study years as currency years, but we will keep it 
### simple and replace missing currency years with end study year. 

cleanDF <- cleanDF %>% mutate(end.currency.year.calc = ifelse(is.na(end.currency.year), 
                                                              end.study.year, 
                                                              end.currency.year))



currencyInfo <- cbind(cleanDF$covidence.id, cleanDF$study.country, 
                      currencyCode,
                      cleanDF$currency.year, cleanDF$end.currency.year.calc)

colnames(currencyInfo) <- c("covidence.id", "study.country",
                            "currency.code", "currency.year", "end.currency.year.calc")

currencyInfo <- as.data.frame(currencyInfo)
rm(cleanDF)

saveRDS(currencyInfo, here("data/monetaryConversion/localCurrencyInfo.rds"), version=2)
