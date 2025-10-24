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
