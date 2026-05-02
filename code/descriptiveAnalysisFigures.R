### This function takes clean data (stratified or not) and a string
### describing the data ("AllSurveys", "RuralityStudies", etc.)
### It uses that clean data to plot descriptive analysis and risk
### of bias summary figures. These are saved to PDFs with the inputted
### string suffix in the file name. 
### See example below. 

##############################################################################|
### SAMPLE SCRIPT FOR RUNNING FOR RURALITY SURVEYS ###########################|
### source(here("code/cleanData.R"))
### cleanDF0 <- cleanData()[["clean data"]] %>% filter(report.rurality == "Yes")
### 
### source(here("code/descriptiveAnalysisFigures.R"))
### descriptPlots(cleanDF0 = cleanDF0, fileSuffix = "RuralitySurveys")
##############################################################################|

descriptPlots <- function(){
### Load packages 
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gt)
library(maps)
library(ggpubr)
    library(geosphere)


### Setup palette 
myPal <- c( "#44AA99", "#882255", "#332288", "#117733", "#6699CC", "#CC6677",
            "#AA4499", "#999933", "#A41034","#88CCEE", "#DDCC77", "#888888")

##############################################################################|
##### Create the clean dataset ################################################
##############################################################################|
# source(here("code/cleanData.R"))
# cleanDF0 <- cleanData()[["clean data"]]

##############################################################################|
##### Read in the naming schema for figures ###################################
##############################################################################|

# figID <- read.csv(here("data/titlesForFigures.csv"))
# colnames(figID)[1] <- "covidence.id"
# cleanDF0 <- cleanDF0 %>% left_join(figID[,c("covidence.id", "figure.id")], by = "covidence.id") %>% 
#             mutate(figure.id.yr = paste(figure.id, study.start.year))

cleanDF <- dataList[["descriptive"]]
# readRDS("data/fullDataRaw.rds")


##############################################################################|
##### Read in the World Bank Region data ######################################
##############################################################################|
regionWHO <- read.csv("data/who-regions.csv")[,c(1,4)] %>% 
             rename(study.country = Entity)

### Rename some countries to match the WHO CSV.
### For labeling these will remain as extracted. 
cleanDF[which(cleanDF$Study.country == "Viet Nam"), "Study.country"] <- "Vietnam"
cleanDF[which(cleanDF$Study.country == "The Gambia"), "Study.country"] <- "Gambia"
cleanDF[which(cleanDF$Study.country == "Timor-Leste"), "Study.country"] <- "East Timor"
cleanDF[which(cleanDF$Study.country == "Lao PDR"), "Study.country"] <- "Laos"
cleanDF[which(cleanDF$Study.country == " Philippines"), "Study.country"] <- "Philippines"
cleanDF[which(cleanDF$Study.country == " Ethiopia"), "Study.country"] <- "Ethiopia"
cleanDF[which(cleanDF$Study.country == "Democratic People's Republic of Korea"), "Study.country"] <- "North Korea"

### Join the WHO data 
cleanDF <- cleanDF %>% left_join(regionWHO)

##############################################################################|
##### DESCRIPTIVE PLOTS #######################################################
##############################################################################|
### Open PDF 
filename <- here(paste0("output/descriptive/descriptiveAnalysis.pdf"))
pdf(file = filename, width = 11.5, height = 8.5)

##### PLOTS BY GEOGRAPHY ######################################################

### COUNTRY 
### Count the county instances
nCountry0 <- cleanDF %>% 
            count(Study.country) %>% 
            left_join(regionWHO %>% rename(Study.country=study.country)) 

###
WHOSurveys <- data.frame(Study.country = c("Burkina Faso",
                                           "Democratic Republic of the Congo",
                                           "Fiji",
                                           "Ghana", 
                                           "Indonesia", 
                                           "Mali", 
                                           "Mongolia",
                                           "Myanmar", 
                                           "Nigeria", 
                                           "Papua New Guinea", 
                                           "Philippines", 
                                           "Solomon Islands", 
                                           "Uganda", 
                                           "Vietnam"
                                           ), 
                         n1 = c(1,2,rep(1, 12))) %>% 
    left_join(regionWHO %>% rename(Study.country=study.country)) 

nCountry <- full_join(nCountry0, WHOSurveys) %>% 
            filter(Study.country != "Democratic Republic of Congo") %>%
            group_by(Study.country) %>%
            mutate(n = sum(n, n1, na.rm = TRUE)) %>%
            select(!n1)

### Setup a world map 
world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

centroids <- world_map %>% 
    group_by(region) %>% 
    group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat)))) %>% 
    rename(Study.country = region)

nCountriesGeo <- left_join(nCountry, centroids)
### Make a heat map of prevalence studies by country 
# filename <- here(paste0("~/Desktop/countriesPatientCosts.pdf"))
# pdf(file = filename, width = 13, height = 8.5)
print(
ggplot(nCountry) +
    geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "grey95", color = "black", size = 0.25) +
    geom_map(map = world_map, aes(map_id = Study.country, fill = n)) +
    scale_fill_gradient(low = "darkolivegreen1", high = "darkolivegreen",
                        name = "Number of patient costs surveys") +
    expand_limits(x = world_map$long, y = world_map$lat) + 
    theme_void(base_size = 18) + theme(legend.position = "inside",
                            legend.position.inside = c(.15,.2)) + 
    with(nCountriesGeo, annotate(geom="text", x = lon, y=lat, label = n, size = 5)) +
    ggtitle ("Number of patient cost surveys by country")
)
# dev.off()
### Bar plot of countries by total survey count
nCountry <- nCountry %>% arrange(n) %>% 
            mutate(Study.country = as.factor(Study.country), levels = Study.country)
nCountry$Study.country <- factor(nCountry$Study.country, 
                                 levels = c(sort(unique(nCountry$Study.country), 
                                                 decreasing = TRUE)))
print(
ggplot(nCountry) + geom_col(aes(x=Study.country, y=n), color = "black") + 
                   theme_minimal() + coord_flip() + 
                   theme(legend.position = "inside",
                         legend.position.inside = c(.85,.15), 
                         legend.background = element_rect(color = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=Study.country, y=n, label = n), hjust = 1.5, color = "white") + 
    labs(y = "Survey country", "Number of prevalence surveys") + 
    ggtitle("Number of prevalence surveys by country")
)
### Also make a bar plot colored by WHO Region
# print(
ggplot(nCountry) +
    geom_col(aes(x=Study.country, y=n, fill = World.regions.according.to.WHO), color = "black") +
    theme_minimal() +
    coord_flip() + theme(legend.position = "inside",
                         legend.position.inside = c(.85,.15),
                         legend.background = element_rect(color = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=Study.country, y=n, label = n), hjust = 1.5, color = "white") +
    scale_fill_manual(values=myPal, name = "WHO Region") +
    labs(x = "Survey country", y="Number of prevalence surveys") +
    ggtitle("Number of prevalence surveys by country")
# )

##### WHO REGION
nRegion <- cleanDF %>% 
           count(World.regions.according.to.WHO)
nRegion$World.regions.according.to.WHO <- factor(nRegion$World.regions.according.to.WHO, 
                                                 levels = c(sort(unique(nRegion$World.regions.according.to.WHO), 
                                                                 decreasing = TRUE)))
print(
ggplot(nRegion) + 
    geom_col(aes(x=World.regions.according.to.WHO, y=n), color = "black") + 
    theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=World.regions.according.to.WHO, y=n, label = n), hjust = 2, color ="white") + 
    labs(x = "Survey region", y="Number of prevalence surveys") + 
    ggtitle("Number of prevalence surveys by world region")
)

##### PLOTS BY TIME ###########################################################
### Surveys by publication year
print(
ggplot(cleanDF, aes(x = Publication.year)) + 
    geom_bar(color = "black") + theme_minimal() +
    scale_x_continuous(breaks = seq(2013, 2025, 1)) +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Publication year") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("Number of prevalence surveys by publication year")
)


##### PLOTS BY TIME AND GEOGRAPHY ###########################################################
# print(
nCountryWHO <- cleanDF %>% 
    left_join(regionWHO) %>%  
    group_by(publication.year) %>%
    count(World.regions.according.to.WHO)  

ggplot(nCountryWHO, aes(x = publication.year, y = World.regions.according.to.WHO,  
                    fill = n)) + 
    geom_tile() +
    theme_minimal()
# )



## some summary statistics 
table(cleanDF$Does.the.study.report.that.it.followed.the.World.Health.Organization..WHO..protocol.and.patient.cost.survey.instrument.)
sum(grepl("National", cleanDF$Study.name.of.sub.national.geography, ignore.case = TRUE))

### Report at least one direct medical cost 
sum(rowSums(cleanDF[,colnames(cleanDF)[grep("report.DME", colnames(cleanDF))]] =="Yes") > 0)

### Report at least one direct non-medical cost 
sum(rowSums(cleanDF[,colnames(cleanDF)[grep("report.DNME", colnames(cleanDF))]] =="Yes") > 0)

### Report at least indirect cost 
sum(rowSums(cleanDF[,colnames(cleanDF)[grep("report.IE", colnames(cleanDF))]] =="Yes") > 0)

### Report catastrophic costs 
sum(cleanDF$Does.the.study.report.the.population.with.catastrophic.costs.=="Yes")
sum(grepl("Mtb infection treatment", cleanDF$For.which.steps.of.the.care.cascade.did.the.study.collect.costs.))

###publications of national cost surveys
cleanDF$Title[which(grepl("national", cleanDF$Title, ignore.case = TRUE))]

dev.off()
}