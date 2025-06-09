### This function separates the data extraction strings into
### line specific data using regex. 


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

dataDesc <- stndData[,1:(which(colnames(stndData)=="n.participants")-1)]
dataAll <- stndData[,(which(colnames(stndData)=="n.participants")):which(colnames(stndData)=="additional.comments")] ### Need to fix 
dataStudyQuality <- stndData[,(which(colnames(stndData)=="additional.comments")+1):ncol(stndData)] ### Need to fix 

# rm(stndData)
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

### Loop through the dataframe rows. 
for (index in 1:nrow(dataAll)){
    ### Find the non-missings
    tmpData0 <- dataAll[index, grep("=", dataAll[index,])] %>% 
                ### These stat columns need to be treated specially so 
                ### remove for now. 
                select(!contains(c(".stat", "comments"))) 
    ### Create a dataframe to hold the study specific disaggregated data
    rowData <- data.frame()
    nameRow <- dataDesc[index, "covidence.id"]
    print(nameRow)
    
    for (col in 1:ncol(tmpData0)){
        nameColumn <- colnames(tmpData0)[col]
        print(nameColumn)
        info <- unlist(strsplit(as.vector(unlist(strsplit(x = tmpData0[,col], split = ";"))), "=")) 
        ### Remove the spaces
        info <-  sapply(info, function(x) gsub("^\\s+|\\s+$", "", x))

        ## Check if info has any colons
        # if(sum(grepl(":", info)) > 0){
        #     subInfo <- unlist(strsplit(unlist(strsplit(x = info, split = ":")), " "))
        # }
        
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
        
        ##need to remove tmpData because each study is different
        # rm(tmpDF)
        # tmpDF
        
    }
        ### Add the covidence id for later join and save to the list.
        rowData$covidence.id <- nameRow
        allData[[index]] <- rowData
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
