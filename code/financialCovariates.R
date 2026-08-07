##############################################################################|
##### Load in packages ########################################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)
##############################################################################|

##############################################################################|
##### Read in supporting data #################################################
### Read in the TB patient costs review country year pairs 
reviewData <- readRDS(here("data/reviewData/finalData.rds"))[["descriptive"]] %>% 
              select(study.country,Country.Code, study.end.year)

### Read in the country codes 
countryCodes <- read.csv(here("data/iso3.csv")) %>%
                transmute(study.country = name, 
                          country.code = alpha.3)

### Read in LMIC list 
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
    rename(study.country = country.name) %>%
    select(! class25)

###############################################################################
##### Create the individual median income estimate ############################
##### using Gross National Income and GINI coefficient
##### Note: these estimates are used for prediction of patient costs for all 
##### LMICs by year and for a sister analysis of HIV patient costs. 
##### patient costs -- as such, we do not filter by year or country. 
###############################################################################

##### Read in the GNI data 
### USD (meta data in the directory)
gniPcUSD <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-USD/API_NY.GNP.PCAP.KD_DS2_en_csv_v2_36454.csv")) %>%
    select(Country.Name,
           Country.Code,
           X2000:X2025) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GNI (USD per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) 

#### Interpolate missing GNI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(gniPcUSD$study.country)){
    print(country)
    tmpDF <- gniPcUSD %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`GNI (USD per-capita)`)) > 1){
        gniPcUSD[which(gniPcUSD$study.country == country), "GNI.USD.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`GNI (USD per-capita)`)],
                                                                                             y=tmpDF$`GNI (USD per-capita)`[!is.na(tmpDF$`GNI (USD per-capita)`)], xout=tmpDF$study.end.year,
                                                                                             rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`GNI (USD per-capita)`))== 1){
        gniPcUSD[which(gniPcUSD$study.country == country), "GNI.USD.interpolated"] <- na.omit(tmpDF$`GNI (USD per-capita)`)[1]
    }
}

### Local Currency Units (meta data in the directory)
### These are in constant currency year which varies by country
gniPcLCU <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-LCU/API_NY.GNP.PCAP.KN_DS2_en_csv_v2_35342.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2025) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GNI (LCU per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>%
    ## filter to only the countries of interest 
    # filter(Country.Code %in% reviewData$Country.Code)

#### Interpolate missing GNI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(gniPcLCU$study.country)){
    print(country)
    tmpDF <- gniPcLCU %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`GNI (LCU per-capita)`)) > 1){
        gniPcLCU[which(gniPcLCU$study.country == country), "GNI.LCU.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`GNI (LCU per-capita)`)],
                                                                                      y=tmpDF$`GNI (LCU per-capita)`[!is.na(tmpDF$`GNI (LCU per-capita)`)], xout=tmpDF$study.end.year,
                                                                                      rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`GNI (LCU per-capita)`))== 1){
        gniPcLCU[which(gniPcLCU$study.country == country), "GNI.LCU.interpolated"] <- na.omit(tmpDF$`GNI (LCU per-capita)`)[1]
    }
}

gniPcLCUnom <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-LCU-NOMINAL/API_NY.GNP.PCAP.CN_DS2_en_csv_v2_33196.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2025) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GNI (LCU nominal per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>%
## filter to only the countries of interest 
# filter(Country.Code %in% reviewData$Country.Code)

#### Interpolate missing GNI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(gniPcLCUnom$study.country)){
    print(country)
    tmpDF <- gniPcLCUnom %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`GNI (LCU nominal per-capita)`)) > 1){
        gniPcLCU[which(gniPcLCU$study.country == country), "GNI.LCU.nom.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`GNI (LCU nominal per-capita)`)],
                                                                                             y=tmpDF$`GNI (LCU nominal per-capita)`[!is.na(tmpDF$`GNI (LCU nominal per-capita)`)], xout=tmpDF$study.end.year,
                                                                                             rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`GNI (LCU nominal per-capita)`))== 1){
        gniPcLCU[which(gniPcLCU$study.country == country), "GNI.LCU.nom.interpolated"] <- na.omit(tmpDF$`GNI (LCU nominal per-capita)`)[1]
    }
}

### International dollars (meta data in the directory)
### metadata in directory
gniPcINTD <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-INTD/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_41471.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2025) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GNI (INTD per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE)))

#### Interpolate missing GNI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(gniPcINTD$study.country)){
    print(country)
    tmpDF <- gniPcINTD %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`GNI (INTD per-capita)`)) > 1){
        gniPcINTD[which(gniPcINTD$study.country == country), "GNI.INTD.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`GNI (INTD per-capita)`)],
                                                                                                y=tmpDF$`GNI (INTD per-capita)`[!is.na(tmpDF$`GNI (INTD per-capita)`)], xout=tmpDF$study.end.year,
                                                                                                rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`GNI (INTD per-capita)`))== 1){
        gniPcINTD[which(gniPcINTD$study.country == country), "GNI.INTD.interpolated"] <- na.omit(tmpDF$`GNI (INTD per-capita)`)[1]
    }
}


### Join the three datasets together 
gniPcDF00<- full_join(gniPcUSD, gniPcLCU)
gniPcDF0 <- full_join(gniPcDF00, gniPcLCUnom)
gniPcDF <-  full_join(gniPcDF0, gniPcINTD)


###### Read in the GINI data
giniDF <- read.csv(here("data/covariateData/calc-median-income/GINI.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2025) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    rename(study.country = Country.Name) %>%
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GINI index") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>% 

#### Interpolate missing GINI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(giniDF$study.country)){
    print(country)
    tmpDF <- giniDF %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`GINI index`)) > 1){
    giniDF[which(giniDF$study.country == country), "GINI.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`GINI index`)],
                                                                                  y=tmpDF$`GINI index`[!is.na(tmpDF$`GINI index`)], xout=tmpDF$study.end.year,
                                                                                  rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`GINI index`))== 1){
        giniDF[which(giniDF$study.country == country), "GINI.interpolated"] <- na.omit(tmpDF$`GINI index`)[1]
    }
}

### Join into a single dataframe
medianIndividualIncomeDF0 <- left_join(gniPcDF, giniDF %>% select(!study.country), by=c("Country.Code", "study.end.year")) %>%
    select(study.country, Country.Code, 
           study.end.year,
           `GNI (LCU per-capita)`, GNI.LCU.interpolated, 
           `GNI (LCU nominal per-capita)`, GNI.LCU.nom.interpolated, 
           `GNI (USD per-capita)`, GNI.USD.interpolated, 
           `GNI (INTD per-capita)`, GNI.INTD.interpolated, 
           `GINI index`, GINI.interpolated)


### Add in WHO region and calculate regional median values 
### we will use these when the country specific values are missing 
medianIndividualIncomeDF0 <- left_join(medianIndividualIncomeDF0, read.csv(here("data/who-regions.csv")) %>%
                                           mutate(who.region = gsub(" \\(WHO\\)", "", World.regions.according.to.WHO)) %>% 
                                           select(Code, who.region) %>% 
                                           rename(Country.Code = Code),
                                       by="Country.Code")  %>% 
    mutate(who.region = ifelse(Country.Code == "VCT", 
                               "Americas", who.region)) %>% 
    group_by(who.region, study.end.year) %>% 
    mutate(GNI.LCU.interpolated.region = median(GNI.LCU.interpolated, na.rm=TRUE),
           GNI.LCU.nom.interpolated.region = median(GNI.LCU.nom.interpolated, na.rm=TRUE),
           GNI.USD.interpolated.region = median(GNI.USD.interpolated, na.rm=TRUE),
           GNI.INTD.interpolated.region = median(GNI.INTD.interpolated, na.rm=TRUE),
           GINI.interpolated.region = median(GINI.interpolated, na.rm=TRUE)) %>%
    ungroup() 

### We also developed an imputation model in `giniImputation.R` that imputed missing 
### GINI via GDP, HDI, and other covariates. Stored in data/.../GINI-imputed.csv
### ###### This dataset from Stephanie on August 4, 2026.
giniImpDF <- read.csv(here("data/covariateData/calc-median-income/GINI-imputed-INTD.csv")) %>% 
    select(iso3, year, gni_final) %>% 
    rename(Country.Code = iso3, 
           study.end.year = year,
           GINI.imputed = gni_final)



medianIndividualIncomeDF0 <- left_join(medianIndividualIncomeDF0, giniImpDF) %>%
    mutate(GNI.LCU.calc = ifelse(is.na(GNI.LCU.interpolated),
                                 GNI.LCU.interpolated.region,
                                 GNI.LCU.interpolated),
           GNI.LCU.nom.calc = ifelse(is.na(GNI.LCU.nom.interpolated),
                                 GNI.LCU.nom.interpolated.region,
                                 GNI.LCU.nom.interpolated),
           GNI.USD.calc = ifelse(is.na(GNI.USD.interpolated),
                                 GNI.USD.interpolated.region,
                                 GNI.USD.interpolated),
           GNI.INTD.calc = ifelse(is.na(GNI.INTD.interpolated),
                                  GNI.INTD.interpolated.region,
                                  GNI.INTD.interpolated),
           GINI.calc = ifelse(is.na(GINI.interpolated),
                              GINI.imputed,
                              GINI.interpolated),
           GINI.calc = ifelse(is.na(GINI.calc),
                              GINI.interpolated.region,
                              GINI.calc),
           GINI.desc = case_when(GINI.calc == `GINI index` ~ "Country/Year matched",
                                 GINI.calc ==  GINI.interpolated ~ "Country interpolated",
                                 GINI.calc ==  GINI.imputed ~ "Imputed",
                                 GINI.calc ==  GINI.interpolated.region ~ "Regional"
           ),
           GNI.desc = case_when(GNI.INTD.calc == `GNI (INTD per-capita)` ~ "Country/Year matched",
                                GNI.INTD.calc ==  GNI.INTD.interpolated ~ "Country interpolated",
                                GNI.INTD.calc ==  GNI.INTD.interpolated.region ~ "Regional"
           )
    )

# medianIndividualIncomeDF0M <- medianIndividualIncomeDF01 %>% filter(study.end.year == 2023 & 
                                                # (is.na(GNI.USD.interpolated) | is.na(GINI.interpolated)))
# View(medianIndividualIncomeDF0M)
###############################################################################
###### Calculate median income using the dataset above

### Define a function to estimate the median income from GNI per capita and GINI
calc_median_income <- function(gini, #GNI
                               gniPC){
    
    gini_from_alpha2 <- function(alpha) { exp(lgamma(alpha+0.5) - log(pi)/2 - lgamma(alpha+1)) }
    
    ### Define a function for optimization
    alpha_from_gini <- function(gini){
        xopt <- function(alpha_try,
                         gini){ 
            (gini_from_alpha2(alpha_try)-gini)^2 
        }
        
        optimize(xopt,c(0.001,100000), gini=gini)$minimum  
    }
    
    # For gini = 0.45 and mean PC GNI = 500...
    # gam_shape = alpha_from_gini(0.45)
    # gam_rate  = gam_shape/500
    
    gam_shape <- alpha_from_gini(gini)
    gam_rate  <- gam_shape/gniPC
    
    return(qgamma(.5,
                  gam_shape,
                  gam_rate)) # This is median income (379.4672)
}

###############################################################################
##### Apply the function 
for(i in 1:nrow(medianIndividualIncomeDF0)){
    
    if(! is.na(medianIndividualIncomeDF0[i, "GINI.calc"])){
        ### USD 
        medianIndividualIncomeDF0[i,"median.income.calc.usd"] <- calc_median_income(medianIndividualIncomeDF0$GINI.calc[i]/100, 
                                                                                    medianIndividualIncomeDF0$GNI.USD.calc[i])
        ### Local currency units
        medianIndividualIncomeDF0[i,"median.income.calc.lcu"] <- calc_median_income(medianIndividualIncomeDF0$GINI.calc[i]/100, 
                                                                                    medianIndividualIncomeDF0$GNI.LCU.calc[i])
        
        ### Local currency units
        medianIndividualIncomeDF0[i,"median.income.calc.lcu.nom"] <- calc_median_income(medianIndividualIncomeDF0$GINI.calc[i]/100, 
                                                                                    medianIndividualIncomeDF0$GNI.LCU.nom.calc[i])
        
        ### USD 
        medianIndividualIncomeDF0[i,"median.income.calc.intd"] <- calc_median_income(medianIndividualIncomeDF0$GINI.calc[i]/100, 
                                                                                     medianIndividualIncomeDF0$GNI.INTD.calc[i])
    }}

### Join with the full data to check for missingness 
medianIndividualIncomeDF <- left_join(reviewData,
                                      medianIndividualIncomeDF0 %>% select(!study.country),
                                      by=c("Country.Code", "study.end.year")) %>%
    rename(country.code = Country.Code) %>% 
    select(study.country, country.code, 
           study.end.year,
           `GNI (LCU per-capita)`, GNI.LCU.interpolated, GNI.LCU.calc,
           `GNI (USD per-capita)`, GNI.USD.interpolated, GNI.USD.calc,
           `GNI (INTD per-capita)`, GNI.INTD.interpolated, GNI.INTD.calc,
           `GINI index`, GINI.interpolated, GINI.imputed, GINI.calc, 
           median.income.calc.usd, median.income.calc.lcu, median.income.calc.intd)

##### Missingness is better; let's print the missing countries 
medianIndividualIncomeDF[rowSums(is.na(medianIndividualIncomeDF)) > 1,]

sum(medianIndividualIncomeDF$median.income.calc.lcu < 1, na.rm = TRUE) / nrow(medianIndividualIncomeDF)*100
sum(medianIndividualIncomeDF$median.income.calc.usd < 1, na.rm = TRUE) / nrow(medianIndividualIncomeDF)*100

##############################################################################|
##### Move median income to 2025 currency year 
##############################################################################|

##### Read in the deflator data 
deflator0 <- read.csv(here("data/monetaryConversion/GDP-deflator/API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_38103.csv"))

##### Set the USD conversion factor between 2021 and 2025 
us25_21 <- (deflator0 %>% filter(Country.Name == "United States"))[,"X2025"] / 
           (deflator0 %>% filter(Country.Name == "United States"))[,"X2021"]
us25_15 <- (deflator0 %>% filter(Country.Name == "United States"))[,"X2025"] / 
           (deflator0 %>% filter(Country.Name == "United States"))[,"X2015"]

##### Format the deflator data with a ratio of the every year to 2025 data 
deflator <- deflator0 %>% 
    mutate(gdp.def.21 = case_when(
                               Country.Code == "ERI" ~ X2011,
                               # Country.Code == "VEN" ~ X2011,
                               Country.Code == "YEM" ~ X2013, 
                               Country.Code == "SSD" ~ X2015,
                               .default = X2021),
           gdp.def.25 = case_when(Country.Code == "AFG" ~ X2024,
                                  Country.Code == "CUB" ~ X2024,
                               Country.Code == "LBN" ~ X2024,
                               Country.Code == "SSD" ~ X2015,
                               Country.Code == "ERI" ~ X2011,
                               Country.Code == "SYR" ~ X2022,
                               # Country.Code == "VEN" ~ X2011,
                               Country.Code == "YEM" ~ X2018, 
                        .default = X2025),
           gdp.def25_21 = gdp.def.25/gdp.def.21) %>% 
    pivot_longer(X1960:X2025, names_to = "Year", values_to = "gdp.def") %>% 
    mutate(study.end.year = as.numeric(gsub("X", "", Year))) %>% 
    filter(study.end.year > 1999, 
           Country.Code %in% LMIC$country.code) %>% 
    rename(country.code = Country.Code) %>% 
    select(! (c(Indicator.Name, Indicator.Code, Year, Country.Name))) %>% 
    mutate(inf.ratio = gdp.def.25 / gdp.def)

##### Join to the median income dataset and inflate the lcu data to 2025 and
##### do the simple conversion of 2021 international dollars using USD deflator

medianIndividualIncomeDF2 <- left_join(medianIndividualIncomeDF0 %>% 
                                       rename(country.code = Country.Code), deflator, 
                                       by=c("country.code", "study.end.year")) %>% 
                             mutate(median.income.calc.lcu.nom.2025 = 
                                    median.income.calc.lcu.nom * inf.ratio,
                                    median.income.calc.intd.2025.usd =
                                    median.income.calc.intd * us25_21)

##### Read in the LCU to INTD conversion factors data
lcu.intd0 <- read.csv(here("data/monetaryConversion/LCU-INTD/API_PA.NUS.PPP_DS2_en_csv_v2_33039.csv"))

##### Format the conversion factors, saving only 2025 and 2021 which we need 
lcu.intd <- lcu.intd0 %>% 
    mutate(
           conv.21 = case_when(Country.Code == "VEN" ~ X2011,
                               Country.Code == "CUB" ~ X2011,
                               Country.Code == "YEM" ~ X2013, 
                               .default = X2021),
           conv.25 = case_when(Country.Code == "AFG" ~ X2024, 
                               Country.Code == "LBN" ~ X2024,
                               Country.Code == "SSD" ~ X2021,
                               Country.Code == "ERI" ~ X2021,
                               Country.Code == "SYR" ~ X2022,
                               Country.Code == "CUB" ~ X2011,
                               Country.Code == "VEN" ~ X2011,
                               Country.Code == "YEM" ~ X2013, 
                               .default = X2025)) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    rename(country.code = Country.Code) %>% 
    select((c(country.code,conv.25, conv.21))) 

##### Afghanistan 2024, Lebanon 2024, South Sudan 2021, Syrian Republic 2022 
##### Venezuela 2011 , Yemen 2013
##### Convert the LCU 2025 to international dollars and do the other conversions
##### Below we use an approximation to bring the 2021 INTD to 2025 InTD 
medianIndividualIncomeDF3 <- left_join(medianIndividualIncomeDF2, lcu.intd) %>% 
                             mutate(median.income.calc.intd.2025.lcu = 
                                    median.income.calc.lcu.nom.2025 / conv.25, 
                                    median.income.calc.intd.2025.approx = 
                                    median.income.calc.intd * gdp.def25_21 * (conv.21/conv.25),
                                    approx_scalar = gdp.def25_21 * (conv.21/conv.25))

##### After comparison and discussion, we decided the stabilization of the 2021
##### INTD data is too valuable to lose so we have decided to push forward with 
##### the approximation method. We will clean up the datafile before moving forward

medianIndividualIncomeDF0 <- medianIndividualIncomeDF3 %>% 
                             select(!c(
                                      median.income.calc.lcu.nom.2025,
                                       median.income.calc.intd.2025.usd,
                                       median.income.calc.intd.2025.lcu,
                                       `GNI (LCU per-capita)`,
                                       GNI.LCU.interpolated,
                                       `GNI (LCU nominal per-capita)`,
                                       GNI.LCU.nom.interpolated,
                                      GNI.LCU.interpolated.region, 
                                      GNI.LCU.nom.interpolated.region, 
                                       GNI.LCU.nom.calc,
                                       median.income.calc.lcu.nom,
                                       GNI.LCU.calc,
                                       median.income.calc.lcu,
                                       gdp.def.25,
                                       gdp.def.21,
                                       gdp.def25_21,
                                       gdp.def,
                                       inf.ratio,
                                       conv.25, conv.21)
                                    ) %>% 
                             rename(median.income.calc.usd.2015 = median.income.calc.usd,
                                    median.income.calc.intd.2021 = median.income.calc.intd,
                                    median.income.calc.intd.2025 = median.income.calc.intd.2025.approx,
                                    intd21_25 = approx_scalar) %>%

##### Because of the new approach with conversion to 2025 INTD some of the GDP
##### deflator estimates created a much too high estimate. We now assign the regional
##### averages here after median income is calculated instead for these countries. 

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
    ungroup()  %>% 
    rename(median.income.calc.intd.2025.OLD = median.income.calc.intd.2025, 
           median.income.calc.intd.2025 = median.income.calc.intd.2025.rev) %>% 
    select(! c(median.income.calc.intd.2025.rev0,
               median.income.calc.intd.2025.regional))


# View(medianIndividualIncomeDF0 %>% filter(is.na(median.income.calc.intd.2025)))
# rm(medianIndividualIncomeDF2)
# rm(medianIndividualIncomeDF3)

##############################################################################|
##### Convert to household income  ############################################
##############################################################################|

hh_comp0 <- read.csv(here("data/covariateData/calc-median-income/unPop_hhComp.csv"),na.strings = c("..")) %>% 
            ### There are a bunch of extraneous tabs in the csv -- remove them 
            ### and exploit as numeric to set the rest of the missings to NA
            mutate(unkHH = as.numeric(gsub("\\t", "", unkHH))); str(hh_comp0)


hh_income <- left_join(medianIndividualIncomeDF0, hh_comp0)

# ggplot(hh_income %>% 
#            filter(Country.Code %in% modelData$country.code)) + #%>% 
#     # mutate(study.country = ifelse(study.country == "Congo, Dem. Rep.", "DR Congo", study.country))) + 
#     geom_point(aes(x=study.end.year, y=avgSizeHH)) + theme_minimal() +
#     facet_wrap(facets=vars(study.country))


#### Interpolate missing data 
for (country in unique(hh_income$study.country)){
    print(country)
    tmpDF <- hh_income %>% filter(study.country == country)
    
    nYears <- tmpDF %>% filter(!is.na(avgSizeHH)) %>% select(study.end.year) %>% 
              unique() %>% unlist() %>% length()
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$avgSizeHH)) > 1 & nYears > 1){
        hh_income[which(hh_income$study.country == country), "hh.size.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$avgSizeHH)],
                                                                                      y=tmpDF$avgSizeHH[!is.na(tmpDF$avgSizeHH)], xout=tmpDF$study.end.year,
                                                                                      rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$avgSizeHH))== 1 | nYears == 1){
        hh_income[which(hh_income$study.country == country), "hh.size.interpolated"] <- na.omit(tmpDF$avgSizeHH)[1]
    }
}

hh_income <- hh_income %>% 
    group_by(who.region, study.end.year) %>% 
    mutate(hh.size.interpolated.region = median(hh.size.interpolated, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(    hh.size.calc = ifelse(is.na(hh.size.interpolated),
                                 hh.size.interpolated.region, 
                                 hh.size.interpolated),
               hh.size.desc = case_when( hh.size.calc == avgSizeHH ~ "Country/Year matched",
                                         hh.size.calc ==  hh.size.interpolated ~ "Country interpolated",
                                         hh.size.calc ==  hh.size.interpolated.region ~ "Region interpolated"
              ))


# ggplot(hh_income %>%
#            filter(Country.Code %in% LMIC$country.code)) + #%>%
#     geom_point(aes(x=study.end.year, y=avgSizeHH)) + theme_minimal() +
#     geom_line(aes(x=study.end.year, y=hh.size.calc), color="dodgerblue2") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
#     expand_limits(y=0)
# 
# 
# ggplot(hh_income %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[1:66]))) + #%>%
#     geom_point(aes(x=study.end.year, y=avgSizeHH)) + theme_minimal() +
#     geom_line(aes(x=study.end.year, y=hh.size.calc), color="dodgerblue2") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)
# 
# ggplot(hh_income %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + #%>%
#     geom_point(aes(x=study.end.year, y=avgSizeHH)) + theme_minimal() +
#     geom_line(aes(x=study.end.year, y=hh.size.calc), color="dodgerblue2") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)


#### Create per household income estimates 

medianAllIncomes <- hh_income %>%
                    mutate(median.income.calc.usd.2015.hh  = median.income.calc.usd.2015  * hh.size.calc,
                           median.income.calc.intd.2021.hh = median.income.calc.intd.2021 * hh.size.calc,
                           median.income.calc.intd.2025.hh = median.income.calc.intd.2025 * hh.size.calc)
# 
# ggplot(medianAllIncomes %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[1:66]))) + #%>%
#     geom_line(aes(x=study.end.year, y=median.income.calc.intd.hh), color="salmon") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)
# 
# ggplot(medianAllIncomes %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + #%>%
#     geom_line(aes(x=study.end.year, y=median.income.calc.intd.hh), color="salmon") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)
# 
# ggplot(medianAllIncomes %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[1:66]))) + #%>%
#     geom_line(aes(x=study.end.year, y=median.income.calc.intd), color="purple") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)
# 
# ggplot(medianAllIncomes %>% 
#            filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + #%>%
#     geom_line(aes(x=study.end.year, y=median.income.calc.intd), color="purple") + theme_minimal() +
#     facet_wrap(facets=vars(study.country)) + 
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#     expand_limits(y=0)

#####



##############################################################################|
##### SAVE THE FULL DATASET  ##################################################
# write.csv(medianIndividualIncomeDF, "data/medianIncome.csv")
##### UNCOMMENT ME!!!!! #####
saveRDS(medianAllIncomes, here("data/covariateData/medianIncome.rds"), version =2)
saveRDS(medianAllIncomes, here("data/covariateData/medianIncomePrediction.rds"), version =2)
##############################################################################|

##############################################################################|
##### OOP Spending on TB as percentage of CHE #################################
### Read in spending data from national health accounts collated by WHO
### and accessed through the GHEH. All estimates are in current USD to match
### the Gross National Income Data. 
### link to data repository https://apps.who.int/nha/database/
##############################################################################|

### these data represent spending for TB in current USD per capita
tbSpendData0 <- read.csv(here("data/covariateData/health-accounts/NHA indicators-tb-formatted.csv")) %>% 
    rename(study.country = Countries) %>%
    ### Make data longer format for years
    pivot_longer(cols = starts_with("X"), 
                 names_to = "study.end.year", 
                 names_prefix = "X") %>% 
    ### Make data wider for the Indicators 
    pivot_wider(id_cols = c(study.country, study.end.year), names_from = Indicators)

### Join so we can see the availability of the data 
tbSpendData <- left_join(reviewData %>%
                             mutate(study.end.year = as.character(study.end.year)), 
                         tbSpendData0)

#### Check how much missing data
sum(is.na(tbSpendData$`Tuberculosis`)) / nrow(tbSpendData) *100

#### Poor availability; let's check alternative data from GHEH

##############################################################################|
##### OOP total health as percentage of CHE ###################################
##############################################################################|

### These data are the current health expenditure (in current USD per capita 
### and % GDP) and OOP (in current USD per capita and % CHE)
healthSpendData0 <- read.csv(here("data/covariateData/health-accounts/NHA indicators-general-formatted.csv"), na.strings = c("")) %>% 
    rename(study.country = Countries) %>%
    ### Make data longer format for years
    pivot_longer(cols = starts_with("X"), 
                 names_to = "study.end.year", 
                 names_prefix = "X") %>% 
    ### Make data wider for the Indicators 
    pivot_wider(id_cols = c(study.country, study.end.year), names_from = Indicators) %>% 
    left_join(countryCodes) %>%
    ### Manually add some country codes 
    mutate(country.code = case_when(grepl("Iran", study.country) ~ countryCodes[grep("Iran", countryCodes$study.country), "country.code"],
                                    grepl("Tanzania", study.country) ~ countryCodes[grep("Tanzania", countryCodes$study.country), "country.code"],
                                    grepl("of the Congo", study.country) ~ "COD",
                                    grepl("Bolivia", study.country) ~ countryCodes[grep("Bolivia", countryCodes$study.country), "country.code"],
                                    grepl("Venezuela", study.country) ~ countryCodes[grep("Venezuela", countryCodes$study.country), "country.code"],
                                    grepl("Netherlands", study.country) ~ countryCodes[grep("Netherlands", countryCodes$study.country), "country.code"],
                                    grepl("Moldova", study.country) ~ countryCodes[grep("Moldova", countryCodes$study.country), "country.code"],
                                    grepl("Micronesia", study.country) ~ countryCodes[grep("Micronesia", countryCodes$study.country), "country.code"],
                                    grepl("Korea, Republic of", study.country) ~ "KOR",
                                    .default = country.code)) %>% 
    right_join(LMIC %>% crossing(study.end.year = 2000:2024) %>%
                   select(!study.country) %>% 
                   mutate(study.end.year = as.character(study.end.year)), by=c("country.code", "study.end.year"))

### Those with OOP missing -- check for alternative years 
# dim(healthSpendData0 %>% filter(is.na(`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`),
#                                  study.end.year < 2024))

#### Interpolate missing GNI 
#### Using a loop because dplyr had odd behavior and deadlines 
for (country in unique(healthSpendData0$study.country)){
    print(country)
    tmpDF <- healthSpendData0 %>% filter(study.country == country)
    ### Interpolate through the points we have 
    if(sum(!is.na(tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)) > 1){
        healthSpendData0[which(healthSpendData0$study.country == country), "oop.perc.che.interpolated"] <- approx(x=tmpDF$study.end.year[!is.na(tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)],
                                                                                                                  y=tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`[!is.na(tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)], xout=tmpDF$study.end.year,
                                                                                                                  rule = 2, method = "linear")$y
    } else if(sum(!is.na(tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`))== 1){
        healthSpendData0[which(healthSpendData0$study.country == country), "oop.perc.che.interpolated"] <- na.omit(tmpDF$`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)[1]
    }
}
### Those with OOP missing -- check for alternative years 
# dim(healthSpendData0 %>% filter(is.na(`oop.perc.che.interpolated`),
#                                  study.end.year < 2024))

### Those with OOP missing -- check for alternative years 
# View(healthSpendData0 %>% filter(is.na(`oop.perc.che.interpolated`),
#                                  study.end.year < 2024))


### Join so we can see the availability of the data 
healthSpendData <- left_join(reviewData %>%
                                 rename(country.code = Country.Code) %>%
                                 mutate(study.end.year = as.character(study.end.year)), 
                             healthSpendData0, by = c("study.end.year", "country.code")) 

##### Missingness is better; let's print the missing countries 
healthSpendData[rowSums(is.na(healthSpendData)) > 0,]

### Save the dataset 
# write.csv(healthSpendData, "data/healthAccounts.csv")
saveRDS(healthSpendData0, here("data/covariateData/healthAccounts.rds"), version =2)



##### Check for correlation #### 
###############################################################################
##### If doing a recheck you can just read in the files saved above: 
# medianIncomeDF <- readRDS(here("data/covariateData/medianIncome.rds"))
# healthSpendData<- readRDS(here("data/covariateData/healthAccounts.rds"))

allFinCovDF <- full_join(medianIncomeDF %>% 
                             mutate(study.end.year = as.character(study.end.year)) %>% distinct(), 
                         healthSpendData %>% distinct()) %>% 
               mutate_at(vars(median.income.calc.usd, median.income.calc.lcu, 
                              `Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)`,
                              `Current Health Expenditure (CHE) per Capita in US$`,
                              `Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`,
                              `Out-of-Pocket Expenditure (OOPS) per Capita in US$`),
                              as.numeric)
                       

ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=median.income.calc.usd, y=`Current Health Expenditure (CHE) per Capita in US$`)) + 
    theme_minimal() + xlab("Median Income per Capita in US$")

ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=median.income.calc.usd, y=`Out-of-Pocket Expenditure (OOPS) per Capita in US$`)) + 
    theme_minimal() + xlab("Median Income per Capita in US$")


ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=`Current Health Expenditure (CHE) per Capita in US$`,
                          y=`Out-of-Pocket Expenditure (OOPS) per Capita in US$`)) + 
    theme_minimal()

##### Check percentages #####
ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=median.income.calc.usd, y=`Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)`)) + 
    theme_minimal() + xlab("Median Income per Capita in US$")

ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=median.income.calc.usd, y=`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)) + 
    theme_minimal() + xlab("Median Income per Capita in US$")


ggplot() + geom_point(data=allFinCovDF, 
                      aes(x=`Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)`,
                          y=`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)) + 
    theme_minimal()


##### We are also considering a health quality index covariate so let's check for
##### correlation with that. 

uhcDF0 <- read.csv(here("data/covariateData/WHO-UHC-index.csv")) %>% 
          select(Location, SpatialDimValueCode, Period, Value) %>%
          rename(study.country = Location, 
                 country.code = SpatialDimValueCode,
                 study.end.year = Period, 
                 UHCindex = Value) %>% 
          mutate(study.end.year = as.character(study.end.year))

### Join with other data to check for missingness
uhcDF <- left_join(allFinCovDF, uhcDF0 %>% select(!study.country))

saveRDS(uhcDF0 %>% select(study.country, country.code, study.end.year, UHCindex), 
        here("data/covariateData/uhcIndex.rds"),version=2)

ggplot() + geom_point(data=uhcDF, 
                      aes(x=UHCindex,
                          y=`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`)) + 
    theme_minimal() + xlab("WHO UHC index")

ggplot() + geom_point(data=uhcDF, 
                      aes(x=UHCindex,
                          y=median.income.calc.usd)) + 
    theme_minimal()  + xlab("WHO UHC index") + 
    ylab("Median Income per Capita in US$")
