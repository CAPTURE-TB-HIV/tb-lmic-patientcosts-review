### Load in packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)


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

### Read in spending data from national health accounts collated by WHO
### and accessed through the GHEH. All estimates are in current USD to match
### the Gross National Income Data. 
### link to data repository https://apps.who.int/nha/database/

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

###############################################################################
##### Create the median income estimate using Gross National Income and GINI
###############################################################################

##### Read in the GNI data 
### USD (meta data in the directory)
gniPcUSD <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-USD/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_763.csv")) %>%
    select(Country.Name,
           Country.Code,
           X2000:X2023) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2023, names_to = "study.end.year", values_to = "GNI (USD per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>%
    ## filter to only the countries of interest
    # filter(Country.Code %in% reviewData$Country.Code)

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
gniPcLCU <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-LCU/API_NY.GNP.PCAP.CN_DS2_en_csv_v2_9957.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2023) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2023, names_to = "study.end.year", values_to = "GNI (LCU per-capita)") %>%
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

### Join the two together 
gniPcDF <- full_join(gniPcUSD, gniPcLCU)

###### Read in the GINI data
giniDF <- read.csv(here("data/covariateData/calc-median-income/GINI.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2023) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    rename(study.country = Country.Name) %>%
    pivot_longer(cols = X2000:X2023, names_to = "study.end.year", values_to = "GINI index") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>% 
    ## filter to only the countries of interest 
    # filter(Country.Code %in% reviewData$Country.Code)

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
medianIncomeDF0 <- left_join(gniPcDF, giniDF %>% select(!study.country), by=c("Country.Code", "study.end.year")) %>%
                    select(study.country, Country.Code, 
                           study.end.year,
                           `GNI (LCU per-capita)`, GNI.LCU.interpolated, 
                           `GNI (USD per-capita)`, GNI.USD.interpolated, 
                           `GINI index`, GINI.interpolated)


### Add in WHO region and calculate regional median values 
### we will use these when the country specific values are missing 
medianIncomeDF0 <- left_join(medianIncomeDF0, read.csv(here("data/who-regions.csv")) %>%
              mutate(who.region = gsub(" \\(WHO\\)", "", World.regions.according.to.WHO)) %>% 
              select(Code, who.region) %>% 
              rename(Country.Code = Code),
          by="Country.Code")  %>% 
              mutate(who.region = ifelse(Country.Code == "VCT", 
                                         "Americas", who.region)) %>% 
              group_by(who.region, study.end.year) %>% 
              mutate(GNI.LCU.interpolated.region = median(GNI.LCU.interpolated, na.rm=TRUE),
                     GNI.USD.interpolated.region = median(GNI.USD.interpolated, na.rm=TRUE),
                     GINI.interpolated.region = median(GINI.interpolated, na.rm=TRUE)) %>%
              ungroup() 

### We also developed an imputation model in `giniImputation.R` that imputed missing 
### GINI via GDP, HDI, and other covariates. Stored in data/.../GINI-imputed.csv
giniImpDF <- read.csv(here("data/covariateData/calc-median-income/GINI-imputed.csv")) %>% 
             select(iso3, year, gni_pred) %>% 
             rename(Country.Code = iso3, 
                    study.end.year = year,
                    GINI.imputed = gni_pred)



medianIncomeDF0 <- left_join(medianIncomeDF0, giniImpDF) %>%
              mutate(GNI.LCU.calc = ifelse(is.na(GNI.LCU.interpolated),
                                           GNI.LCU.interpolated.region,
                                           GNI.LCU.interpolated),
                     GNI.USD.calc = ifelse(is.na(GNI.USD.interpolated),
                                           GNI.USD.interpolated.region,
                                           GNI.USD.interpolated),
                     GINI.calc = ifelse(is.na(GINI.interpolated),
                                        GINI.imputed,
                                        GINI.interpolated),
                     GINI.desc = case_when(GINI.calc == `GINI index` ~ "Country/Year matched",
                                           GINI.calc ==  GINI.interpolated ~ "Country interpolated",
                                           GINI.calc ==  GINI.imputed ~ "Imputed"
                                           ),
                     GNI.desc = case_when(GNI.USD.calc == `GNI (USD per-capita)` ~ "Country/Year matched",
                                          GNI.USD.calc ==  GNI.USD.interpolated ~ "Country interpolated",
                                          GNI.USD.calc ==  GNI.USD.interpolated.region ~ "Regional"
                                          )
                     );

# medianIncomeDF0M <- medianIncomeDF01 %>% filter(study.end.year == 2023 & 
                                                # (is.na(GNI.USD.interpolated) | is.na(GINI.interpolated)))
# View(medianIncomeDF0M)
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
for(i in 1:nrow(medianIncomeDF0)){
    
    if(! is.na(medianIncomeDF0[i, "GINI.calc"])){
    ### USD 
    medianIncomeDF0[i,"median.income.calc.usd"] <- calc_median_income(medianIncomeDF0$GINI.calc[i]/100, 
                                                                     medianIncomeDF0$GNI.USD.calc[i])
    ### Local currency units
    medianIncomeDF0[i,"median.income.calc.lcu"] <- calc_median_income(medianIncomeDF0$GINI.calc[i]/100, 
                                                                     medianIncomeDF0$GNI.LCU.calc[i])
    }}

### Join with the full data to check for missingness 
medianIncomeDF <- left_join(reviewData,
                            medianIncomeDF0 %>% select(!study.country),
                            by=c("Country.Code", "study.end.year")) %>%
    rename(country.code = Country.Code) %>% 
    select(study.country, country.code, 
           study.end.year,
           `GNI (LCU per-capita)`, GNI.LCU.interpolated, GNI.LCU.calc,
           `GNI (USD per-capita)`, GNI.USD.interpolated, GNI.USD.calc,
           `GINI index`, GINI.interpolated, GINI.imputed, GINI.calc, median.income.calc.usd, median.income.calc.lcu)

##### Missingness is better; let's print the missing countries 
medianIncomeDF[rowSums(is.na(medianIncomeDF)) > 1,]

sum(medianIncomeDF$median.income.calc.lcu < 1, na.rm = TRUE) / nrow(medianIncomeDF)*100
sum(medianIncomeDF$median.income.calc.usd < 1, na.rm = TRUE) / nrow(medianIncomeDF)*100

### Also Nigeria does not have GNI per capita data for any year. 

### Save the dataset
# write.csv(medianIncomeDF, "data/medianIncome.csv")
saveRDS(medianIncomeDF, here("data/covariateData/medianIncome.rds"), version =2)
saveRDS(medianIncomeDF0, here("data/covariateData/medianIncomePrediction.rds"), version =2)


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
