### Local Currency Units (meta data in the directory)
gniPcLCU <- read.csv(here("data/covariateData/calc-median-income/GNI-PC-LCU/API_NY.GNP.PCAP.CN_DS2_en_csv_v2_9957.csv")) %>% 
    select(Country.Name, 
           Country.Code,
           X2000:X2025) %>%
    rename(study.country = Country.Name) %>%
    filter(Country.Code %in% LMIC$country.code) %>% 
    pivot_longer(cols = X2000:X2025, names_to = "study.end.year", values_to = "GNI (LCU per-capita)") %>%
    mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) #%>%


### Join the two together 
gniPcDF <- full_join(gniPcDF, gniPcINTD)

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
medianIndividualIncomeDF0 <- left_join(gniPcDF, giniDF %>% select(!study.country), by=c("Country.Code", "study.end.year")) %>%
    select(study.country, Country.Code, 
           study.end.year,
           `GNI (LCU per-capita)`, GNI.LCU.interpolated, 
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
           GNI.USD.interpolated.region = median(GNI.USD.interpolated, na.rm=TRUE),
           GNI.INTD.interpolated.region = median(GNI.INTD.interpolated, na.rm=TRUE),
           GINI.interpolated.region = median(GINI.interpolated, na.rm=TRUE)) %>%
    ungroup() 

### We also developed an imputation model in `giniImputation.R` that imputed missing 
### GINI via GDP, HDI, and other covariates. Stored in data/.../GINI-imputed.csv
giniImpDF <- read.csv(here("data/covariateData/calc-median-income/GINI-imputed.csv")) %>% 
    select(iso3, year, gni_pred) %>% 
    rename(Country.Code = iso3, 
           study.end.year = year,
           GINI.imputed = gni_pred)



medianIndividualIncomeDF0 <- left_join(medianIndividualIncomeDF0, giniImpDF) %>%
    mutate(GNI.LCU.calc = ifelse(is.na(GNI.LCU.interpolated),
                                 GNI.LCU.interpolated.region,
                                 GNI.LCU.interpolated),
           GNI.USD.calc = ifelse(is.na(GNI.USD.interpolated),
                                 GNI.USD.interpolated.region,
                                 GNI.USD.interpolated),
           GNI.INTD.calc = ifelse(is.na(GNI.INTD.interpolated),
                                 GNI.INTD.interpolated.region,
                                 GNI.INTD.interpolated),
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


quantile(medianIndividualIncomeDF$median.income.calc.intd, na.rm = TRUE)

ggplot(data=medianIndividualIncomeDF3 %>% 
           filter(country.code %in% unique(LMIC$country.code[1:66]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd), color = "green") + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd.2021), color = "orchid3") + 
    
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data=medianIndividualIncomeDF3 %>% 
           filter(country.code %in% unique(LMIC$country.code[67:132]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd), color = "green") + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd.2021), color = "orchid3") + 
    
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data=medianIndividualIncomeDF0 %>% 
    filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd), color = "green") + 
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=medianIndividualIncomeDF0 %>% 
           filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.usd), color = "orange") + 
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=medianIndividualIncomeDF0 %>% 
           filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.lcu), color = "blue") + 
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data=medianIndividualIncomeDF0 %>% 
           filter(Country.Code %in% unique(LMIC$country.code[67:132]))) + 
    geom_line(aes(x=study.end.year, y = median.income.calc.usd), color = "orange") + 
    geom_line(aes(x=study.end.year, y = median.income.calc.intd), color = "green") + 
    geom_line(data=medianIndividualIncomeDF3 %>% 
                  filter(country.code %in% unique(LMIC$country.code[67:132])), 
              aes(x=study.end.year, y = median.income.calc.intd.2021), color = "orchid3") + 
    theme_minimal() + 
    facet_wrap(facets=vars(study.country), scales = "free_y") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


