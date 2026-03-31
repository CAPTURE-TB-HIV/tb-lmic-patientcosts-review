### Load in packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)


### Read in the TB patient costs review country year pairs 
reviewData <- readRDS(here("data/reviewData/finalData.rds"))[["descriptive"]] %>% 
              select(study.country,Country.Code, study.end.year)


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

healthSpendData0 <- read.csv(here("data/covariateData/health-accounts/NHA indicators-general-formatted.csv")) %>% 
    rename(study.country = Countries) %>%
    ### Make data longer format for years
    pivot_longer(cols = starts_with("X"), 
                 names_to = "study.end.year", 
                 names_prefix = "X") %>% 
    ### Make data wider for the Indicators 
    pivot_wider(id_cols = c(study.country, study.end.year), names_from = Indicators) %>% 
    ### These data did not have country codes so we have to manually rename some counties
    mutate(study.country = case_when(grepl("Iran", study.country) ~ "Iran",
                                     grepl("Tanzania", study.country) ~ "Tanzania",
                                     grepl("of the Congo", study.country) ~ "DR Congo",
                                     grepl("Moldova", study.country) ~ "Moldova", 
                                     grepl("Lao", study.country) ~ "Lao PDR", 
                                     .default = study.country))

### Join so we can see the availability of the data 
healthSpendData <- left_join(reviewData %>%
                                 mutate(study.end.year = as.character(study.end.year)), 
                             healthSpendData0)

##### Missingness is better; let's print the missing countries 
healthSpendData[rowSums(is.na(healthSpendData)) > 0,]

### Save the dataset 
# write.csv(healthSpendData, "data/healthAccounts.csv")
saveRDS(healthSpendData, here("data/covariateData/healthAccounts.rds"), version =2)

###############################################################################
##### Create the median income estimate using Gross National Income and GINI
###############################################################################

##### Read in the GNI data 
### USD (meta data in the directory)
gniPcUSD <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-USD/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_763.csv")) %>%
    select(Country.Name,
           Country.Code,
           X2009:X2023) %>%
    rename(study.country = Country.Name) %>%
    
    pivot_longer(cols = X2009:X2023, names_to = "study.end.year", values_to = "GNI (USD per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) %>%
    ## filter to only the countries of interest
    filter(Country.Code %in% reviewData$Country.Code)

### Local Currency Units (meta data in the directory)
gniPcLCU <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-LCU/API_NY.GNP.PCAP.CN_DS2_en_csv_v2_9957.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2009:X2023) %>%
    rename(study.country = Country.Name) %>%
    
    pivot_longer(cols = X2009:X2023, names_to = "study.end.year", values_to = "GNI (LCU per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) %>%
    ## filter to only the countries of interest 
    filter(Country.Code %in% reviewData$Country.Code)

### Join the two together 
gniPcDF <- full_join(gniPcUSD, gniPcLCU)


###### Read in the GINI data
giniDF <- read.csv(here("data/covariateData/calc-median-income/GINI.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2009:X2023) %>%
    rename(study.country = Country.Name) %>%
    pivot_longer(cols = X2009:X2023, names_to = "study.end.year", values_to = "GINI index") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) %>% 
    ## filter to only the countries of interest 
    filter(Country.Code %in% reviewData$Country.Code)

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
medianIncomeDF0 <- left_join(gniPcDF, giniDF, by=c("Country.Code", "study.end.year"))

### Join with the full data 
medianIncomeDF <- left_join(reviewData, 
                            medianIncomeDF0, by=c("Country.Code", "study.end.year")) %>%
    select(study.country, 
           study.end.year,
           `GNI (LCU per-capita)`, `GNI (USD per-capita)`,
           `GINI index`, GINI.interpolated)

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
for(i in 1:nrow(medianIncomeDF)){
    
    if(! is.na(medianIncomeDF[i, "GINI.interpolated"])){
    ### USD 
    medianIncomeDF[i,"median.income.calc.usd"] <- calc_median_income(medianIncomeDF$`GINI index`[i]/100, 
                                                                     medianIncomeDF$`GNI (USD per-capita)`[i])
    ### Local currency units
    medianIncomeDF[i,"median.income.calc.lcu"] <- calc_median_income(medianIncomeDF$`GINI index`[i]/100, 
                                                                     medianIncomeDF$`GNI (LCU per-capita)`[i])
}}


##### Missingness is better; let's print the missing countries 
medianIncomeDF[rowSums(is.na(medianIncomeDF)) > 1,]

sum(medianIncomeDF$median.income.calc.lcu < 1, na.rm = TRUE) / nrow(medianIncomeDF)*100
sum(medianIncomeDF$median.income.calc.usd < 1, na.rm = TRUE) / nrow(medianIncomeDF)*100

### Also Nigeria does not have GNI per capita data for any year. 

### Save the dataset
# write.csv(medianIncomeDF, "data/medianIncome.csv")
saveRDS(medianIncomeDF, here("data/covariateData/medianIncome.rds"), version =2)


##### Check for correlation #### 

allFinCovDF <- full_join(medianIncomeDF %>% mutate(study.end.year = as.character(study.end.year)), healthSpendData) %>% 
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
