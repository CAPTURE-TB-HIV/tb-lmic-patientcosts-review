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
source(here("code/cleanData.R")) 
##### Reformat the multiple populations into separate lines of data
source(here("code/createLineData.R")) 

##############################################################################|
#############################    LOAD DATA    #################################
##############################################################################|
rawData <- readRDS(here("data/fullDataRaw.rds"))

##### Economic data ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
toUSDcf   <- readRDS(here("data/toUSDconversionFactors.rds"))
fromUSDcf <- readRDS(here("data/fromUSDconversionFactors.rds"))
inflation <- readRDS(here("data/inflationLocalFactors.rds"))

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
                   contains("ie.")) %>%
            left_join(econData) %>% 
            select(! contains("report"))
# sum(is.na(costData))

### Expand the econData to match the rows 
econData <- costData %>% select(covidence.id) %>% left_join(econData)

updatedCosts <- costData[,c(-(which(colnames(costData) %in% c("pop.id", colnames(econData)))))]
updatedCosts <- as.data.frame(sapply( updatedCosts, as.numeric )) *
                econData[,"fromUSD"] * econData[,"inflate"] * econData[,"toUSD"]

# sum(is.na(updatedCosts))

updatedCosts <- cbind(lineDataList$data %>% select(!colnames(updatedCosts)),
                updatedCosts)

lineDataUpdated           <- lineDataList
lineDataUpdated[["data"]] <- updatedCosts

saveRDS(lineDataUpdated, here("data/finalData.rds"), version=2)
