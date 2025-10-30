### This function separates the data extraction strings into
### line specific data using regex. 

line_data <- function(cleanData, saveFile=FALSE){
##############################################################################|
#############################  LOAD PACKAGES  #################################
##############################################################################|
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gt)
library(maps)
library(ggpubr)

##############################################################################|
################ SEPARATE INTO DESCRIPTIVE AND DATA ###########################
##############################################################################|
# cleanData <- readRDS(here("data/fullDataClean.rds"))[["clean data"]]

dataDesc <- cleanData[,1:(which(colnames(cleanData)=="n.participants")-1)]
dataAll <- cleanData[,(which(colnames(cleanData)=="n.participants")):which(colnames(cleanData)=="additional.comments")] ### Need to fix 
dataStudyQuality <- cleanData[,(which(colnames(cleanData)=="additional.comments")+1):ncol(cleanData)] ### Need to fix 

# rm(cleanData)
##############################################################################|
################        EXTRACT THE DATA FROM FIELDS       ####################
##############################################################################|

### Loop through the dataframe rows to find the columns that have entries that 
### need to be disaggregated by population. These are indicated with "=" so we 
### can filter based on that character. All should be formatted as such: 
### pop.id.1=N.1; pop.id.2=N.1; pop.id.3=N.3 so we can leverage the structure
### to create a row for each population. We will then join these non-missing 
### columns back to the full dataframe to fill the missings with NAs. 


### Create an empty list of which each element will hold one study worth of 
### data. 
allData <- list()

### Create a dataframe to hold the stat data: 
rowStatData <- data.frame(pop.id = NA,
                          participant.age.mean=NA, 
                          participant.age.sd=NA, 
                          participant.age.median=NA, 
                          participant.age.iqr=NA,
                          participant.age.range=NA,
                          participant.hhs.mean=NA, 
                          participant.hhs.sd=NA, 
                          participant.hhs.median=NA, 
                          participant.hhs.iqr=NA, 
                          participant.hhs.range=NA,
                          participant.delay.mean=NA, 
                          participant.delay.sd=NA, 
                          participant.delay.median=NA, 
                          participant.delay.iqr=NA,
                          participant.delay.range=NA)

### Loop through the dataframe rows. 
for (index in 1:nrow(dataAll)){
    ### Find the non-missings
    tmpData0 <- dataAll[index, grep("=", dataAll[index,])] %>% 
                ### These stat columns need to be treated specially so 
                ### remove for now. 
                select(!contains(c(".stat", "comments"))) 
    # tmpDataStats <- dataAll[index, grep("=", dataAll[index,])] %>% 
    #     select(contains(c(".stat")))
    ## Create a dataframe to hold the study specific disaggregated data
    rowData <- data.frame()
    nameRow <- dataDesc[index, "covidence.id"]
    print(nameRow)

    
    for (col in 1:ncol(tmpData0)){
        nameColumn <- colnames(tmpData0)[col]
        print(nameColumn)
        info <- unlist(strsplit(x = tmpData0[,col], split = "[;=]+"))
        ### Remove the spaces
        info <-  sapply(info, function(x) gsub("^\\s+|\\s+$", "", x))

        ### Calculate the number of unique populations
        numbData <- length(info) / 2

        ### dataframe to hold that specific column's data by population
        tmpDF <- data.frame()

        for (dt in 1:numbData){
            tmpDF[dt,1] <- info[((dt*2)-1)]
            tmpDF[dt,2] <- as.numeric(info[(dt*2)])
        }

        colnames(tmpDF) <- c("pop.id",
                             nameColumn)

        ### Join to the study specific dataframe.
        if (col == 1){
            rowData <- tmpDF
        } else {
            rowData <- full_join(rowData, tmpDF, by=c("pop.id"))
        }

        #### if we have "Mean or Median" without an identifier, it should
        #### always map to the first identifier, so let's consolidate

        # # CHECK IF MEAN ROW EXISTS
        # if(length(which(rowData$pop.id == "Mean")) > 0){
        #     which(!is.na(rowData["Mean",]))
        # }


    }
        ### Add the covidence id for later join and save to the list.
        rowData$covidence.id <- nameRow
        allData[[index]] <- rowData
        
    #/////////////////////////////////////////////////////////////////////////#
    #/////////////////////////////////////////////////////////////////////////#
    # for (col2 in 1:ncol(tmpDataStats)){
    #     nameColumn2 <- gsub(".stat", "", colnames(tmpDataStats)[col2])
    #     print(nameColumn2)
    # 
    #     info2 <- unlist(strsplit(x = tmpDataStats[,col2], split = "[;\\s:\\s=\\s(]+"))
    #     ### Remove the spaces
    #     info2 <-  unlist(as.vector(strsplit(sapply(info2, function(x) gsub("^\\s+|\\s+$|^\\(|\\)$", "", x)), split = "\\(")))
    #     names(info2) <- NULL
    #     # tmpDF2 <- data.frame()
    #     
    #     numbData2 <- length(grep("\\:", tmpDataStats[,col2]))
    #     ### If there is a colon, then they've listed a population label, if not, we know 
    #     ### it's the pop.id used above. 
    #     if(!grepl(":", tmpDataStats[,col2])){
    #         rowStatData[,"pop.id"] = "Total"
    #         rowStatData[,grep(paste0(nameColumn2,".", info2[1]), colnames(rowStatData), ignore.case = TRUE)] <- info2[2]
    #         rowStatData[,grep(paste0(nameColumn2,".", info2[3]), colnames(rowStatData), ignore.case = TRUE)] <- info2[4]
    #     } else {
    #         rowStatData[,"pop.id"] = info[1]
    #         rowStatData[,grep(paste0(nameColumn2,".", info2[2]), colnames(rowStatData), ignore.case = TRUE)] <- info2[3]
    #         rowStatData[,grep(paste0(nameColumn2,".", info2[4]), colnames(rowStatData), ignore.case = TRUE)] <- info2[5]
    #         }
    #     } 
        
        # colnames(tmpDF) <- c("pop.id", 
        #                      nameColumn)
    # }
}





### make empty dataframe 
newDF <- dataAll %>% select(!contains("stat")) 
newDF$covidence.id <- dataDesc$covidence.id

### Create a new dataframe that joins all the data together to fill in the 
### missing fields. 

newAllData <- data.frame()
for(i in 1:length(allData)){
    newDF2 <- newDF[i,] %>% select(!colnames(allData[[i]])[-c(1, ncol(allData[[i]]))])
    newDF3 <- full_join(newDF2, allData[[i]], by=c("covidence.id"))
    ### bind the data all together
    newAllData <- rbind(newAllData, newDF3)
}

### Extract the pop.id so we can move it to the front of the dataframe 
pop.id <- newAllData$pop.id
newAllData <- subset(newAllData, select= -c(pop.id)) 

### Bind the new dataframe with the identifiers at the beginning
popData <-   cbind(newAllData$covidence.id, 
             pop.id,
             (newAllData[,match(colnames(newDF), colnames(newAllData))])) 

### Covidence ID is now in the dataframe twice so remove one            
popData <- subset(popData, select= -c(covidence.id)) 
### Rename the columns at the front
colnames(popData)[1:2] <- c("covidence.id", "pop.id")

rm(newDF); rm(newDF2); rm(newDF3)

lineData <- list("descriptive" = dataDesc, 
                 "data" = popData, 
                 "quality" = dataStudyQuality) 

### Save line data to RDS file 
if (saveFile == TRUE){
    saveRDS(lineData, here("data/fullLineData.rds"), version=2)
}

return(lineData)

}