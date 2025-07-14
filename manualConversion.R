###############################################################################
i<- badIndex[1]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> kenya2020

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> kenya2021


median(c(kenya2020[,2],
         kenya2021[,2])) -> conversionFactors[i]

###############################################################################

i<- badIndex[2]
index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2012-01-01", 
                          end_date =  "2012-12-01") -> ethiopia2012
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2013-01-01", 
                          end_date =  "2013-12-01") -> ethiopia2013
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2014-01-01", 
                          end_date =  "2014-12-01") -> ethiopia2014
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2015-01-01", 
                          end_date =  "2015-12-01") -> ethiopia2015

median(c(ethiopia2012[,2],
       ethiopia2013[,2],
       ethiopia2014[,2], 
       ethiopia2015[,2])) -> conversionFactors[24]


###############################################################################

i<- badIndex[3]
index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2017-01-01", 
                          end_date =  "2017-12-31") -> solomon2017
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> solomon2018
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> solomon2019

median(c(solomon2017[,2],
         solomon2018[,2],
         solomon2019[,2])) -> conversionFactors[i]

###############################################################################

i<- badIndex[4]
index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> india2019
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> india2020
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> india2021

median(c(india2019[,2],
         india2020[,2],
         india2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[5]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2017-01-01", 
                          end_date =  "2017-12-31") -> indonesia2017
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> indonesia2018
median(c(indonesia2017[,2],
         indonesia2018[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[6]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> vietnam2020

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> vietnam2021

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2022-01-01", 
                          end_date =  "2022-12-31") -> vietnam2022

median(c(vietnam2020[,2],
         vietnam2021[,2], 
         vietnam2022[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[7]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01",
                          end_date =  "2020-11-30") -> ethiopia2020
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01",
                          end_date =  "2021-12-31") -> ethiopia2021
median(c(ethiopia2020[,2],
         ethiopia2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[8]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(ethiopia2020[,2],
         ethiopia2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[9]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2016-01-01",
                          end_date =  "2016-11-30") -> moldova2016
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2017-01-01",
                          end_date =  "2017-12-31") -> moldova2017
median(c(moldova2016[,2],
         moldova2017[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex[10]

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> nepal2018
historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> nepal2019
median(c(nepal2018[,2],
         nepal2019[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[1]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "XAF"))

currencyCode <- "XAF"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> cameroon2020

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> cameroon2021
median(c(cameroon2020[,2],
         cameroon2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[2]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "CNY"))

currencyCode <- "CNY"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2012-01-01", 
                          end_date =  "2012-12-31") -> china2012

median(c(china2012[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[3]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "THB"))

currencyCode <- "THB"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> thai2019

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> thai2020

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> thai2021

median(c(thai2019[,2], 
         thai2020[,2], 
         thai2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[4]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "PHP"))

currencyCode <- "PHP"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> phillip2018

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> phillip2019

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> phillip2020

median(c(phillip2018[,2], 
         phillip2019[,2], 
         phillip2020[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[5]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "PHP"))

median(c(phillip2018[,2], 
         phillip2019[,2], 
         phillip2020[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[6]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "CNY"))

currencyCode <- "CNY"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> china2020

median(c(china2020[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[7]
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

median(c(ethiopia2012[,2],
         ethiopia2013[,2],
         ethiopia2014[,2],
         ethiopia2015[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[8]
print(dataDesc$study.country[i])

index <- grep("Viet", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> vietnam2018

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> vietnam2019

median(c(vietnam2018[,2],
         vietnam2019[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[9]
print(dataDesc$study.country[i])
currencyCode = "XOF"

print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencyCode))

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2021-01-01", 
                          end_date =  "2021-12-31") -> mali2021

median(c(mali2021[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[10]
print(dataDesc$study.country[i])

print(paste("Saving conversion factor from", dataDesc$currency.year[i], "USD to", 
            "PHP"))

currencyCode = "PHP"

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2016-01-01", 
                          end_date =  "2016-11-30") -> phillip2016

median(c(phillip2016[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[11]
print(dataDesc$study.country[i])

index <- grep("Viet", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(vietnam2020[,2],
         vietnam2021[,2],
         vietnam2022[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[12]
print(dataDesc$study.country[i])

index <- grep("Viet", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2016-01-01", 
                          end_date =  "2016-11-30") -> vietnam2016

median(c(vietnam2016[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[13]
print(dataDesc$study.country[i])

index <- grep("Viet", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(vietnam2020[,2],
         vietnam2021[,2],
         vietnam2022[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[14]
print(dataDesc$study.country[i])
currencyCode = "XOF"

print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencyCode))

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2020-01-01", 
                          end_date =  "2020-11-30") -> burkinaFaso2020

median(c(burkinaFaso2020[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[15]
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2016-01-01", 
                          end_date =  "2016-11-30") -> bangla2016

median(c(bangla2016[,2])) -> conversionFactors[i]

###############################################################################
i<- badIndex2[16]
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(india2019[,2], 
         india2020[,2], 
         india2021[,2])) -> conversionFactors[i]


###############################################################################
### International Dollars #####################################################

i <- 37
print(dataDesc$study.country[i])

index <- grep("Thai", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2014-01-01", 
                          end_date =  "2014-12-31") -> thai2014

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2015-01-01", 
                          end_date =  "2015-12-31") -> thai2015

median(c(thai2014[,2], 
         thai2015[,2])) -> conversionFactors[i]

###############################################################################
i<-38
print(dataDesc$study.country[i])
index <- grep("Thai", currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

median(c(thai2015[,2])) -> conversionFactors[i]

#####
### Do a check
check <- cbind(dataDesc$currency, conversionFactors)
check[19,]

###############################################################################
i<-19 
print(dataDesc$study.country[i])
index <- grep("Chin", currencies[,1])[2]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))
currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> china2018

median(c(china2018[,2])) -> conversionFactors[i]

###############################################################################
i<-54 
print(dataDesc$study.country[i])

currencyCode <- "SZL"

print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencyCode))

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2017-01-01", 
                          end_date =  "2017-12-31") -> eswatini2017

median(c(eswatini2017[,2])) -> conversionFactors[i]

###############################################################################
i<-104 
print(dataDesc$study.country[i])

currencyCode <- "LAK"

print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencyCode))

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-12-31") -> lao2018

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2019-01-01", 
                          end_date =  "2019-12-31") -> lao2019

median(c(lao2018[,2],
         lao2019[,2])) -> conversionFactors[i]

###############################################################################
i<-107
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(india2019[,2])) -> conversionFactors[i]

###############################################################################
i<-108
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(india2019[,2])) -> conversionFactors[i]

###############################################################################
i<-109
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

currencyCode = currencies[index, 2]

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2017-01-01", 
                          end_date =  "2017-12-31") -> india2017

historical_exchange_rates(from="USD", to = currencyCode,
                          start_date =  "2018-01-01", 
                          end_date =  "2018-11-30") -> india2018

median(c(india2017[,2], 
         india2018[,2])) -> conversionFactors[i]

###############################################################################
i<-110
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(india2020[,2])) -> conversionFactors[i]

###############################################################################
i<-111
print(dataDesc$study.country[i])

index <- grep(dataDesc$study.country[i], currencies[,1])[1]
print(paste("Saving conversion factor from", dataDesc$study.years[i], "USD to", 
            currencies[index, 1]))

median(c(ethiopia2013[,2])) -> conversionFactors[i]
