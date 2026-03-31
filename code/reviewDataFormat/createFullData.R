##############################################################################|
#############################   DESCRIPTION   #################################
##### This script brings together various steps of data cleaning, reformating, 
##### currency conversion and inflation to convert from the raw dataset from 
##### the raw data from Covidence into the data set that we need from analysis.

##############################################################################|
############################   EDIT HISTORY   #################################
##### Last edited October 24, 2025 by N. Swartwood. 

##############################################################################|
#############################  LOAD PACKAGES  #################################
##############################################################################|
library(here)
library(dplyr)
library(reshape2)

##############################################################################|
#########################    SOURCE OTHER FILES    ############################
##############################################################################|
##### Data cleaning function 
source(here("code/reviewDataFormat/cleanData.R")) 
##### Reformat the multiple populations into separate lines of data
source(here("code/reviewDataFormat/createLineData.R")) 

##############################################################################|
#############################    LOAD DATA    #################################
##############################################################################|
rawData <- readRDS(here("data/reviewData/fullDataRaw.rds"))

##### Economic data ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
toUSDcf   <- readRDS(here("data/monetaryConversion/toUSDconversionFactors.rds"))
fromUSDcf <- readRDS(here("data/monetaryConversion/fromUSDconversionFactors.rds"))
inflation <- readRDS(here("data/monetaryConversion/inflationLocalFactors.rds"))

##############################################################################|
########################      CLEAN THE DATA      #############################
##############################################################################|

cleanDataList <- clean_data(rawData = rawData, 
                            saveFile = FALSE)

### Keep only the clean dataset itself 
cleanData <- cleanDataList[["clean data"]]

##############################################################################|
########################      LINE LIST DATA      #############################
##############################################################################|

lineDataList <- line_data(cleanData = cleanData, 
                          saveFile = FALSE)

##############################################################################|
########################   CONVERT TO 2024 USD    #############################
##############################################################################|
econData <- data.frame(covidence.id = lineDataList$descriptive$covidence.id, 
                       fromUSD = as.numeric(fromUSDcf),
                       toUSD = as.numeric(toUSDcf),
                       inflate = as.numeric(inflation))

costData <- lineDataList$data %>% 
            select(covidence.id, pop.id, 
                   contains("de."), 
                   contains("dme."), 
                   contains("dmne."), 
                   contains("ie."),
                   contains("total.cost")) %>%
            left_join(econData) %>% 
            select(! contains("report"))
# sum(is.na(costData))

### Expand the econData to match the rows 
econData <- costData %>% select(covidence.id) %>% left_join(econData)

updatedCosts <- costData[,c(-(which(colnames(costData) %in% c("pop.id", colnames(econData)))))]
updatedCosts <- as.data.frame(sapply( updatedCosts, as.numeric)) *
                econData[,"fromUSD"] * econData[,"inflate"] * econData[,"toUSD"]

# sum(is.na(updatedCosts))

updatedCosts <- cbind(lineDataList$data %>% select(!colnames(updatedCosts)),
                updatedCosts)

lineDataUpdated           <- lineDataList
lineDataUpdated[["data"]] <- updatedCosts

#### Check for induced NAs
setdiff(
colnames(cleanData[1,] %>% select(colnames(lineDataUpdated[["data"]])[-2])) [which(!is.na(cleanData[1,] %>% select(colnames(lineDataUpdated[["data"]])[-2])))],
colnames(lineDataUpdated[["data"]][1,-2]) [which(!is.na(lineDataUpdated[["data"]][1,-2]))]
)
#### These are all IQRs which are not being used at the moment so we will 
#### leave as a future to-do. 

saveRDS(lineDataUpdated, here("data/reviewData/finalData.rds"), version=2)
