### This script calls the related data cleaning functions in 
### series and returns a list containing:
### 1. a dataframe of missings
### 2. a dataframe of errors 
### 3. a dataframe of cleaned data for analysis. 

clean_data <- function(rawData, saveFile = FALSE){
    library(here)

    ### Create an error dataframe to populate throughout the steps
    errorDF <- data.frame("Study title" = NULL,
                          "Study ID" = NULL,
                          "Column name" = NULL,
                          "Error message" = NULL)

    ### Read in the rawData
    # rawData <- readRDS(here("data/fullDataRaw.rds"))

    ### Call in the standardizeData function
    source(here("code/standardizeData.R"))
    stndList <- standardData(rawData)
    stndData <- stndList[["clean data"]]
    ### Create line listed data 
    
    
    ### Create new variables 
    ### Separate convidence intervals, etc. 
    stndData$study.start.year <- NA
    stndData$study.end.year <- NA
    
    ### Split the string based on the "-" character 
    yearSplit <- strsplit(stndData$study.years, "-")
    
    ### If length of the split is 1, set the start and end year to 
    ### the first sub-element.
    stndData[which(sapply(yearSplit,length)==1), "study.start.year"] <-
        stndData[which(sapply(yearSplit,length)==1), "study.end.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==1)],"[[",1))
    
    ### If length of the split is 2, set the start year to the first 
    ### sub-element and end year to second. 
    stndData[which(sapply(yearSplit,length)==2), "study.start.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==2)],"[[",1))
    
    stndData[which(sapply(yearSplit,length)==2), "study.end.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==2)],"[[",2))
    
    
    ### Fix study country names and add country codes 
    ### Read in countryCodes
    codes <- read.csv(here("data/iso3.csv")) %>% 
        select(name, alpha.3) %>% 
        rename(study.country = name, 
               Country.Code = alpha.3) 
    
    stndData <- stndData %>% 
                mutate(study.country =  case_when( study.country == "Iran" ~ 
                                                       "Iran, Islamic Republic of", 
                                                   study.country == "Vietnam" ~ 
                                                       "Viet Nam", 
                                                   study.country == "Tanzania" ~
                                                       "Tanzania, United Republic of",
                                                   study.country == "Democratic Republic of Congo" ~
                                                       "Congo, Democratic Republic of the", 
                                                   study.country == "The Gambia" ~
                                                       "Gambia", 
                                                   study.country == "Moldova" ~ 
                                                       "Moldova, Republic of", 
                                                   study.country == "Lao PDR" ~ 
                                                       "Lao People's Democratic Republic", 
                                         .default = study.country))
    
    stndData <- left_join(stndData, codes) %>% 
                mutate(study.country =  case_when( study.country == "Iran, Islamic Republic of" ~ 
                                                       "Iran", 
                                                   study.country == "Tanzania, United Republic of" ~
                                                       "Tanzania",
                                                   study.country == "Congo, Democratic Republic of the" ~
                                                       "DR Congo", 
                                                   study.country == "Moldova, Republic of" ~ 
                                                       "Moldova", 
                                                   study.country == "Lao People's Democratic Republic" ~ 
                                                       "Lao PDR", 
                                                   .default = study.country))
                
    
    stndList[["clean data"]] <- stndData
    ### Check for valid data
    
    ### Check for logical data 
    
    ### Create a summary of all the cleaning and the "cleanest dataset".
    # cleanDataSummary <- list("missings" = stndList[["missings"]], 
    #                          "errors" = lgcList[["errors"]], 
    #                          "clean data" = cleanData)
    
    ### Save errors dataframe as a csv
    # write.csv(lgcList[["errors"]], here("data/errorDataToCheck.csv"), row.names = FALSE)
    
    ### Save clean data to RDS file 
    if (saveFile == TRUE){
        saveRDS(stndList, file = here("data/fullDataClean.rds"))
    }
    
    # return(cleanDataSummary)
    return(stndList)
}
