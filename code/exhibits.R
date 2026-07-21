
##############################################################################|
#### Load in packages #### 
##############################################################################|
library(reshape2)
library(geosphere)
library(forcats)
library(here)
library(dplyr)
library(patchwork)
library(tidybayes)
library(scales)
library(forcats)
library(ggplot2)

incRate <- read.csv("~/Downloads/TB_burden_countries_2026-04-13.csv") %>% 
    filter(year == 2024) %>% 
    select(country, iso3, e_inc_100k) %>% 
    rename(study.country = country, 
           country.code = iso3, 
           inc100K = e_inc_100k)

lmic <- read.csv(here("data/LMIC2025.csv")) %>% 
    rename(study.country = country.name)

### Data used to fit the model 
source(here("code/modelDataFormat.R"))
regionWHO <- read.csv(here("data/who-regions.csv")) %>% 
    rename(study.country=Entity)

### Add who region to descriptive data 
dataDesc <- cbind(dplyr::left_join(dataList$descriptive, regionWHO) %>% 
                      mutate(who.region = gsub(" \\(WHO\\)", replacement="",
                                               World.regions.according.to.WHO)) %>% 
                      select(! World.regions.according.to.WHO), 
                  dataList$quality) %>% 
    mutate(risk.of.bias=gsub(" risk of bias", replacement="", 
                             study.quality.risk.of.bias)) %>% 
    select(! study.quality.risk.of.bias) 

### Add in the covariates for the prediction 
covariates <- readRDS(here("data/covariates.rds"))
covariatesTime <- readRDS(here("data/covariatesTime.rds"))

### Load in the samples 
lmicSampsAll <- readRDS(here("output/allYears_06-09.rds"))
lmicSamps <- lmicSampsAll[[16]]

### Create a vector of the Top 30 TB burden countries 
top30 <- c("Brazil", "Central African Republic", "Congo", "Ethiopia", 
           "Gabon", "Kenya", "Lesotho", "Liberia", "Namibia", "Thailand", 
           "Uganda", "United Republic of Tanzania", "China", "India", 
           "Indonesia", "Mozambique", "Myanmar", "Nigeria", "Philippines", 
           "South Africa", "Zambia", "Angola", "Bangladesh", "DPR Korea", 
           "Mongolia", "Pakistan", "Papua New Guinea", "Viet Nam", 
           "Sierra Leone", "Democratic Republic of the Congo")


tbPtntCstSeed <- 513

##############################################################################|
##### Total surveys (included not modeled)       ##############################
##############################################################################|

dataList$data %>% select(covidence.id, pop.id, n.participants) %>% na.omit() %>% 
    group_by(covidence.id) %>% 
    mutate(n.participants = as.numeric(n.participants), 
            total.participants = case_when(length(unique(pop.id)) == 1 ~ n.participants, 
                                          length(unique(pop.id)) > 1 & grepl("Total", pop.id) ~ n.participants, 
                                          length(unique(pop.id)) > 1 & "Total" %in% unique(pop.id) & 
                                              !grepl("Total", pop.id) ~ NA, 
                                          length(unique(pop.id)) > 1 & !("Total" %in% unique(pop.id)) & 
                                              !grepl("Total", pop.id) ~ sum(as.numeric(n.participants)),
                                          .default = 0)) %>%
select(covidence.id, total.participants) %>% 
    ### manually fix a couple of studies 
    mutate(total.participants = case_when(covidence.id == 7682 ~ 725, 
                                          covidence.id == 9594 ~ 100, 
                                          .default=total.participants)) %>% 
    na.omit() %>% distinct() %>% 
    ungroup() %>% summarize(total.participants = sum(total.participants))


##############################################################################|
##### HIV surveys (not modeled)       ########################################
##############################################################################|

hivCovID <- c(9198, 8979, 8263)

tmpHIV <- (fullData %>% filter(covidence.id %in% hivCovID))

tmpHIV <- tmpHIV[,colSums(is.na(tmpHIV))<nrow(tmpHIV)]




##############################################################################|
##### Descriptive characteristics of modeled surveys      #####################
##############################################################################|

dataList$data %>% select(covidence.id, pop.id, n.participants) %>% na.omit() %>% 
    group_by(covidence.id) %>% 
    mutate(n.participants = as.numeric(n.participants), 
           total.participants = case_when(length(unique(pop.id)) == 1 ~ n.participants, 
                                          length(unique(pop.id)) > 1 & grepl("Total", pop.id) ~ n.participants, 
                                          length(unique(pop.id)) > 1 & "Total" %in% unique(pop.id) & 
                                              !grepl("Total", pop.id) ~ NA, 
                                          length(unique(pop.id)) > 1 & !("Total" %in% unique(pop.id)) & 
                                              !grepl("Total", pop.id) ~ sum(as.numeric(n.participants)),
                                          .default = 0)) %>%
    select(covidence.id, total.participants) %>% 
    ### manually fix a couple of studies 
    mutate(total.participants = case_when(covidence.id == 7682 ~ 725, 
                                          covidence.id == 9594 ~ 100, 
                                          .default=total.participants)) %>% 
    na.omit() %>% distinct() %>% 
    ungroup() %>% 
    filter(covidence.id %in% modelData$covidence.id) %>% 
    summarize(total.participants = sum(total.participants))

dataDesc %>% 
    filter(covidence.id %in% modelData$covidence.id) %>% 
    select(study.country) %>% distinct() %>% dim()

### All cost categories unmodeled pooled median
### Optional for the margins for the proportions of these costs
modelData %>% 
    ungroup() %>%
    mutate(pop.id = ifelse(pop.id == "wgtTotal", "Total", pop.id)) %>%
    reframe(numbCosts = sum(dme.prediag, dnme.prediag, ie.prediag, 
                            dme.intsvtx, dnme.intsvtx, ie.intsvtx,
                            dme.contntx, dnme.contntx, ie.contntx), .by=c(covidence.id, pop.id), 
            cost=cost) %>% 
    filter(numbCosts == 9) %>% 
    group_by(pop.id) %>%
    reframe(medianCosts = median(cost))

modelData %>% 
    ungroup() %>%
    mutate(pop.id = ifelse(pop.id == "wgtTotal", "Total", pop.id)) %>%
    reframe(numbCosts = sum(dme.prediag, dnme.prediag, ie.prediag, 
                            dme.intsvtx, dnme.intsvtx, ie.intsvtx,
                            dme.contntx, dnme.contntx, ie.contntx), .by=c(covidence.id, pop.id), 
            cost=sum(cost)) %>% 
    filter(numbCosts == 9) -> allCosts

### How many surveys 
length(unique(allCosts$covidence.id))

allCosts %>% filter(pop.id %in% c("DSTB", "DRTB")) %>% select(covidence.id) %>% 
    distinct() %>% unlist() %>% length()

    allCosts %>% 
    group_by(pop.id) %>%
    reframe(medianCosts = median(cost),
            lowerIQ = quantile(cost, probs=0.25),
            higherIQ = quantile(cost, probs=0.75))
    
    allCosts %>% 
    reframe(medianCosts = median(cost))
    
    ggplot(data=allCosts %>% mutate(whoSurv = ifelse(covidence.id %in% 
                                                         ### These are the WHO patient costs surveys 
                                                         c(1:4, 8688, 7799, 8781, 7682, 9330,
                                                           8270, 9620, 7443,7355, 7919, 9422,
                                                           8310, 9585, 8634, 8677, 9301), "WHO", "Other"))) + 
        theme_minimal(base_size=14) + 
        geom_point(aes(x=covidence.id, y=cost, color=pop.id,shape=whoSurv), size=3) + 
        coord_flip() + facet_wrap(facets=vars(whoSurv), ncol=1) + 
        ggtitle("Costs among studies reporting all costs")
    
    
    ggplot(data=allCosts %>% mutate(whoSurv = ifelse(covidence.id %in% 
                                                         ### These are the WHO patient costs surveys 
                                                         c(1:4, 8688, 7799, 8781, 7682, 9330,
                                                           8270, 9620, 7443,7355, 7919, 9422,
                                                           8310, 9585, 8634, 8677, 9301), "WHO", "Other")) %>% 
               filter(pop.id == "Total")) + 
        theme_minimal(base_size=14) + 
        geom_point(aes(x=covidence.id, y=cost, color=whoSurv,shape=whoSurv), size=4) + 
        coord_flip() + facet_wrap(facets=vars(whoSurv), ncol=1) + 
        ggtitle("Costs among studies reporting all costs")
##############################################################################|
##### Format estimates and input data        ##################################
##############################################################################|
countryEst <- left_join(covariates, 
                        spread_draws(lmicSamps, 
                                     ### non-RR/MDR-TB
                                     dstb_dme_prediag_cost_country[country.id.new],
                                     dstb_dnme_prediag_cost_country[country.id.new],
                                     dstb_ie_prediag_cost_country[country.id.new],
                                     dstb_dme_intsvtx_cost_country[country.id.new],
                                     dstb_dnme_intsvtx_cost_country[country.id.new],
                                     dstb_ie_intsvtx_cost_country[country.id.new],
                                     dstb_dme_contntx_cost_country[country.id.new],
                                     dstb_dnme_contntx_cost_country[country.id.new],
                                     dstb_ie_contntx_cost_country[country.id.new],
                                     dstb_total_cost_country[country.id.new], 
                                     ### RR/MDR-TB
                                     drtb_dme_prediag_cost_country[country.id.new],
                                     drtb_dnme_prediag_cost_country[country.id.new],
                                     drtb_ie_prediag_cost_country[country.id.new],
                                     drtb_dme_intsvtx_cost_country[country.id.new],
                                     drtb_dnme_intsvtx_cost_country[country.id.new],
                                     drtb_ie_intsvtx_cost_country[country.id.new],
                                     drtb_dme_contntx_cost_country[country.id.new],
                                     drtb_dnme_contntx_cost_country[country.id.new],
                                     drtb_ie_contntx_cost_country[country.id.new],
                                     drtb_total_cost_country[country.id.new], 
                                     ndraws = 1000, seed = tbPtntCstSeed) %>% 
                            group_by(country.id.new) %>% 
                            mean_qi(dstb_dme_prediag_cost_country, 
                                      dstb_dnme_prediag_cost_country, 
                                      dstb_ie_prediag_cost_country,
                                      dstb_dme_intsvtx_cost_country, 
                                      dstb_dnme_intsvtx_cost_country, 
                                      dstb_ie_intsvtx_cost_country,
                                      dstb_dme_contntx_cost_country, 
                                      dstb_dnme_contntx_cost_country, 
                                      dstb_ie_contntx_cost_country,
                                      dstb_total_cost_country,
                                      drtb_dme_prediag_cost_country, 
                                      drtb_dnme_prediag_cost_country, 
                                      drtb_ie_prediag_cost_country,
                                      drtb_dme_intsvtx_cost_country, 
                                      drtb_dnme_intsvtx_cost_country, 
                                      drtb_ie_intsvtx_cost_country,
                                      drtb_dme_contntx_cost_country, 
                                      drtb_dnme_contntx_cost_country, 
                                      drtb_ie_contntx_cost_country,
                                      drtb_total_cost_country), 
                        join_by(country.id.new)) %>% 
    select(study.country, in.review, country.id.new,
           median.income, 
           log.median.income, 
           median.income.trunc,
           log.median.income.trunc,
           oop.perc.che, 
           oop.perc.che.trunc,
           uhc.index, 
           uhc.index.trunc,
           dstb_dme_prediag_cost_country, 
           dstb_dnme_prediag_cost_country, 
           dstb_ie_prediag_cost_country,
           dstb_dme_intsvtx_cost_country, 
           dstb_dnme_intsvtx_cost_country, 
           dstb_ie_intsvtx_cost_country,
           dstb_dme_contntx_cost_country, 
           dstb_dnme_contntx_cost_country, 
           dstb_ie_contntx_cost_country,
           dstb_total_cost_country,
           drtb_dme_prediag_cost_country, 
           drtb_dnme_prediag_cost_country, 
           drtb_ie_prediag_cost_country,
           drtb_dme_intsvtx_cost_country, 
           drtb_dnme_intsvtx_cost_country, 
           drtb_ie_intsvtx_cost_country,
           drtb_dme_contntx_cost_country, 
           drtb_dnme_contntx_cost_country, 
           drtb_ie_contntx_cost_country,
           drtb_total_cost_country) %>% 
    ungroup() %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(cntry_order = 1:n()) 

    covariatesAll <- covariates %>% 
        select(study.country, country.code, in.review, country.id.new) %>%
        left_join(regionWHO %>% 
                      select(! c(Year, Code, time))) %>% 
        mutate(who.region = gsub(" \\(WHO\\)", replacement="",
                                 World.regions.according.to.WHO)) %>% 
        select(! World.regions.according.to.WHO) %>% 
        mutate(who.region = case_match(study.country,
                                       c("Democratic Republic of the Congo",
                                         "United Republic of Tanzania",
                                         "Cabo Verde") ~ "Africa", 
                                       c("Syrian Arab Republic", 
                                         "Iran (Islamic Republic of)", 
                                         "Türkiye") ~ "Eastern Mediterranean", 
                                       c("Lao People's Democratic Republic",
                                         "Viet Nam", 
                                         "Timor-Leste") ~ "South-East Asia", 
                                       c("Venezuela (Bolivarian Republic of)",
                                         "Bolivia (Plurinational State of)", 
                                         "Saint Vincent and the Grenadines") ~ "Americas", 
                                       c("Republic of Moldova", 
                                         "Türkiye") ~ "Europe",
                                       "Micronesia (Federated States of)" ~ "Western Pacific",
                                       .default = who.region))
    
    covariatesAll <- left_join(covariatesAll, lmic)  %>% 
        mutate(class25 = case_match(study.country,
                                    c("Democratic Republic of the Congo", 
                                      "Syrian Arab Republic", 
                                      "Gambia", "Yemen",  
                                      "Somalia") ~ "L", 
                                    c("Cabo Verde", 
                                      "United Republic of Tanzania",
                                      "Lao People's Democratic Republic",
                                      "Viet Nam",  "Congo", "Cote d'Ivoire",
                                      "Egypt", "Timor-Leste",
                                      "Bolivia (Plurinational State of)",
                                      "Micronesia (Federated States of)", 
                                      "Kyrgyzstan", 
                                      "Sao Tome and Principe") ~ "LM", 
                                    c("Iran (Islamic Republic of)", 
                                      "Türkiye", "Saint Lucia",
                                      "Saint Vincent and the Grenadines",
                                      "Republic of Moldova", 
                                      "Venezuela (Bolivarian Republic of)") ~ "UM", 
                                    .default = class25),
               country.code = case_match(study.country, 
                                         "Congo" ~ "COG", 
                                         "Cote d'Ivoire" ~ "CIV",
                                         "Democratic Republic of the Congo" ~ "COD", 
                                         "Syrian Arab Republic" ~ "SYR", 
                                         "Gambia" ~ "GMB", 
                                         "Yemen" ~ "YEM",  
                                         "Somalia" ~ "SOM", 
                                         "Cabo Verde" ~ "CPV", 
                                         "United Republic of Tanzania" ~ "TZA",
                                         "Lao People's Democratic Republic" ~ "LAO",
                                         "Viet Nam" ~ "VNM",  
                                         "Egypt" ~ "EGY", 
                                         "Timor-Leste" ~ "TLS",
                                         "Bolivia (Plurinational State of)" ~ "BOL",
                                         "Micronesia (Federated States of)" ~ "FSM", 
                                         "Kyrgyzstan" ~ "KGZ", 
                                         "Sao Tome and Principe" ~ "STP", 
                                         "Iran (Islamic Republic of)" ~ "IRN", 
                                         "Türkiye" ~ "TUR", 
                                         "Saint Lucia" ~ "LCA",
                                         "Saint Vincent and the Grenadines" ~ "VCT",
                                         "Republic of Moldova" ~ "MDA", 
                                         "Venezuela (Bolivarian Republic of)" ~ "VEN",
                                         .default = country.code)) 

    ### Add in notifications 
    covariatesAll <- left_join(covariatesAll, notif %>%
                                   filter(study.end.year %in% 2023:2024, 
                                          country.code %in% covariatesAll$country.code) %>% 
                                   select(!who.region) %>% 
                                   pivot_wider(id_cols = country.code,  names_from = study.end.year, values_from = notif)) %>% 
        mutate(notif = ifelse(is.na(`2024`),
                              `2023`, `2024`)) %>% 
        select(! c(`2023`, `2024`))
    
countryEst <- left_join(countryEst, covariatesAll)

##############################################################################|
##### Figure 1 is PRISMA diagram ##############################################
##############################################################################|
### You can view it in supporting documentation

##############################################################################|
##### Figure 2 is descriptive    ##############################################
##############################################################################|
### Panel A - Global distribution of surveys used in the quantitative 
### analysis. 

nCountry <- modelData %>% select(study.country, covidence.id) %>% 
    distinct() %>% count(study.country)

world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

centroids <- world_map %>% 
    group_by(region) %>% 
    group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat)))) %>% 
    rename(study.country = region)

centroids[which(centroids$study.country == "Vietnam"), "study.country"] <- "Viet Nam"
centroids[which(centroids$study.country == "Laos"), "study.country"] <- "Lao PDR"
centroids[which(centroids$study.country == 
                    "Democratic Republic of the Congo"), "study.country"] <- "DR Congo"
centroids[which(centroids$study.country == "Swaziland"), "study.country"] <- "Eswatini"

nCountriesGeo <- left_join(nCountry, centroids)

plot2a <- ggplot(nCountry) +
    geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "grey95", color = "black", size = 0.25) +
    geom_map(map = world_map, aes(map_id = study.country, fill = n)) +
    scale_fill_gradient(low = "darkolivegreen1", high = "darkolivegreen",
                        name = "Number of patient costs surveys") +
    expand_limits(x = world_map$long, y = world_map$lat) + 
    theme_void(base_size = 18) + theme(legend.position = "inside",
                                       legend.position.inside = c(.15,.2)) + 
    with(nCountriesGeo, annotate(geom="text", x = lon, y=lat, label = n, size = 5))

ggsave(plot2a, file=here("output/figures/figure2a.pdf"), height=8, width = 12, units="in")

### Panel B - Surveys over time, country, and  ???
nCountryYearRep <- modelData %>% select(study.country, study.end.year, 
                                        whohb, covidence.id) %>% 
    distinct() %>% left_join(dataDesc) %>% 
    mutate("Rep" = ifelse(study.setting == "National", 
                          "Nationally representative", 
                          "Subnationally representative")) %>% 
    group_by(Rep, study.end.year) %>% count(study.country) %>% 
    rename(repCount=n) %>% ungroup()

nCountryYear <- modelData %>% select(study.country, study.end.year, 
                                     whohb, covidence.id) %>% 
    distinct() %>% group_by(study.end.year) %>% 
    count(study.country) %>% rename(yearCount=n)

nCountryYear2 <- left_join(nCountryYearRep, nCountryYear) %>% 
    mutate(rep2 = "All surveys") %>% 
    group_by(study.country) %>%
    mutate(totalCount = sum(yearCount)) %>% ungroup()


plot2b <- ggplot() + 
    geom_point(data=nCountryYear2, aes(x = study.end.year, y=reorder(study.country, totalCount), size=yearCount, color=rep2)) +
    geom_point(data=nCountryYear2 %>% filter(Rep == "Nationally representative"), 
               aes(x = study.end.year, y=study.country, color=Rep, size=repCount), shape=21, fill="light grey") + 
    theme_minimal(base_size = 16) +
    scale_x_continuous(breaks = seq(2008, 2025, 1)) +
    theme(
        legend.position="inside", legend.position.inside=c(.14, .2), 
        legend.background = element_rect(fill="white"),
        legend.box = "vertical") +
    labs(y = "", x = "Study end year", size="Number of surveys", 
         caption = "Number label represents total surveys for each country and year.") +
    scale_color_manual(values = c("black", "black"), name=NULL) + 
    scale_size_continuous(range = c(2,6)) +
    # geom_text(data=nCountryYear2, 
    #           aes(x=study.end.year, y=reorder(study.country, totalCount), label=yearCount),
    #           position = position_dodge(0.9),
    #           colour = "white", size=3) #+
    geom_text(data=nCountryYear2, 
              aes(x=study.end.year, y=reorder(study.country, totalCount), label=yearCount),
              hjust = 0, nudge_x = 0.2,# nudge_y=.2,
              colour = "black", size=3); plot2b

ggsave(plot2b, file=here("output/figures/figure2b.pdf"), height=8, width = 12, units="in")


##############################################################################|
##### Table SX                                       ##########################
##### All surveys in systematic review               ##########################
##############################################################################|   

allIncludedSurveys <- dataDesc %>% mutate(figure.id = paste0(study.country, " (", 
                                                             study.setting, ")")) %>% 
    select(covidence.id, 
           figure.id, 
           study.years, 
           who.region,
           who.handbook, 
           title.extracted, 
           study.pop.desc, 
           study.exclusion, 
           who.handbook, 
           risk.of.bias)

# write.csv(allIncludedSurveys, 
          # file=here("output/supplement/allStudiesQualitative.csv"))

##############################################################################|
##### Table SX                                       ##########################
##### Model data                                     ##########################
##############################################################################|   

modelDataFormatted <- modelData %>% 
    select(covidence.id, 
           study.country, 
           study.end.year, 
           pop.id, 
           cost, 
           contains("dme"),
           contains("dnme"), 
           contains("ie"), 
           drtb.wt, 
           median.income, 
           oop.perc.che,
           UHCindex)

write.csv(modelDataFormatted,
          file=here("output/supplement/modelData.csv"))

##############################################################################|
#### Risk of bias plot ####
##############################################################################|
studyQualDF <- dataList$quality %>% 
    select(!study.quality.risk.of.bias) %>% 
    transmute("Is the study population\nclearly described?" = study.quality.population.detail, 
              "Is the sample population\nrepresentative of\nthe study population?" = study.quality.representativeness, 
              "Is a well-defined research\nquestion posed in an\nanswerable form?" = study.quality.research.question, 
              "Is the economic study\ndesign appropriate\nto the stated objective?" = study.quality.appropriate.design, 
              "Is the time horizon\nappropriate to capture\nthe relevant costs?" = study.quality.time.horizon, 
              "Are all important and\nrelevant costs to the\nobjective identified?" = study.quality.relevant.costs, 
              "Does the article\nindicate no potential\nconflicts of interest?" = study.quality.no.conflicts, 
    ) %>% 
    pivot_longer(everything()) %>% 
    mutate(level = factor(value, levels = c('Yes', 'No', 'Unknown')))

qualPlot <- ggplot(data=studyQualDF %>% 
                       filter(name != "Overall risk of bias"), aes(x=name, fill=level)) + 
    geom_bar(color="black", position = position_fill(reverse = TRUE)) +
    theme_minimal() + theme(legend.position = "bottom",
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank()) + 
    coord_flip() +
    labs(x="", y="Fraction of surveys", 
         fill="") + 
    scale_fill_manual(values=c("forestgreen", "darkred", "darkgrey"))

studyBiasDF <- dataList$quality %>% 
    mutate("            Overall risk of bias" = gsub(" risk of bias", "", study.quality.risk.of.bias)) %>%
    pivot_longer(everything()) %>% 
    mutate(levels = factor(value, levels = c('Low', 'Medium', 'High', "Unknown", "Yes", "No")))



biasPlot <- ggplot(data=studyBiasDF %>% 
                       filter(name == "            Overall risk of bias"), aes(x=name, fill=levels)) + 
    geom_bar(color="black", position = position_fill(reverse = TRUE)) +
    theme_minimal() + theme(legend.position = "bottom",
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank()) + 
    coord_flip() +
    labs(x="", y="Fraction of surveys", 
         fill="") + 
    
    scale_fill_manual(values=c("forestgreen", "chocolate2", "darkred", "darkgrey")); biasPlot

combQualPlot <- ggpubr::ggarrange(qualPlot, biasPlot, ncol=1, nrow=2, common.legend = FALSE, legend="bottom", heights = c(3,1))
ggsave(combQualPlot, file="output/supplement/riskOfBiasPlot.pdf", height=11, width = 8.5, units = "in")

#### All data 
dataList$quality %>% 
    mutate("            Overall risk of bias" = gsub(" risk of bias", "", study.quality.risk.of.bias)) %>%
    pivot_longer(everything()) %>% 
    mutate(levels = factor(value, levels = c('Low', 'Medium', 'High', "Unknown", "Yes", "No"))) %>% 
    filter(name == "            Overall risk of bias") %>% 
    select(value) %>% 
    table()

#### Model data 
cbind(dataList$quality, "covidence.id" = dataList$descriptive$covidence.id) %>%
    filter(covidence.id %in% modelData$covidence.id) %>% 
    mutate("            Overall risk of bias" = gsub(" risk of bias", "", study.quality.risk.of.bias)) %>%
    pivot_longer(everything()) %>% 
    mutate(levels = factor(value, levels = c('Low', 'Medium', 'High', "Unknown", "Yes", "No"))) %>% 
    filter(name == "            Overall risk of bias") %>% 
    select(value) %>% 
    table()



##############################################################################|
##### Figure SX                                      ##########################
##### Explore total TB costs by country              ##########################
##############################################################################|   

### Total non-RR/MDR-TB costs 
ggplot() + 
    geom_col(data = countryEst,
             aes(x=fct_reorder(study.country, cntry_order),
                 y=dstb_total_cost_country)) + 
    theme_minimal(base_size = 8) + 
    coord_flip() + xlab("Country") + 
    ylab("Total costs associated with non-RR/MDR-TB")

ggplot() + 
    geom_col(data = countryEst,
             aes(x=fct_reorder(study.country, cntry_order),
                 y=drtb_total_cost_country)) + 
    theme_minimal(base_size = 8) + 
    coord_flip() + xlab("Country") + 
    ylab("Total costs associated with RR/MDR-TB")

##############################################################################|
##### Figure SX                                      ##########################
##### Total costs by covariates                      ##########################
##############################################################################|

# ggplot(data=countryEst) + 
#     geom_point(aes(x=log.median.income.trunc, y=dstb_total_cost_country)) + 
#     theme_minimal(base_size=18) + expand_limits(y=0) +
#     ggtitle("non-RR/MDR TB household costs by log median income") + 
#     ylab("Total household costs (2024 USD)")  + xlab("log median income") 

# ggplot(data=countryEst) + 
#     geom_point(aes(x=median.income.trunc, y=dstb_total_cost_country), alpha=.75, size=1) +
#     theme_minimal(base_size=18) + expand_limits(y=c(1000,30000), x=c(0,12000)) +
#     scale_x_continuous(breaks=seq(0,12000,2000)) +
#     ggrepel::geom_text_repel(aes(x=median.income.trunc, y=dstb_total_cost_country, label=country.code), size=4) + 
#     ggtitle("non-RR/MDR TB household costs by median income") + 
#     scale_y_log10() + 
#     ylab("Total household costs (2024 USD)")  + xlab("Median income")  

ggplot(data=countryEst) + 
    geom_point(aes(x=median.income.trunc, y=dstb_total_cost_country), alpha=.75, size=1) +
    theme_minimal(base_size=18) + expand_limits(y=0) +
    expand_limits(y=c(0, 30000), x=c(0,12000)) +
    scale_x_continuous(breaks=seq(0,12000,2000)) +
    scale_y_continuous(breaks=seq(0,30000,5000)) + 
    ggrepel::geom_text_repel(aes(x=median.income.trunc, y=dstb_total_cost_country, label=country.code), size=4) + 
    ggtitle("non-RR/MDR TB household costs by median income") + 
    ylab("Total household costs (2024 USD)")  + xlab("Median income")  

ggsave(plot=get_last_plot(), 
       file=here("output/supplement/costByMedianIncome.pdf"), 
       height = 7.5, width=11, units="in")

ggplot(data=countryEst) + 
    geom_point(aes(x=oop.perc.che.trunc, y=dstb_total_cost_country), alpha=.75, size=1) +
    theme_minimal(base_size=18) + expand_limits(y=c(0, 30000), x=c(0,90)) +
    scale_x_continuous(breaks=seq(0,90,10)) + 
    scale_y_continuous(breaks=seq(0,30000,5000)) + 
    ggrepel::geom_text_repel(aes(x=oop.perc.che.trunc, y=dstb_total_cost_country, label=country.code), size=4) +
    ggtitle("non-RR/MDR TB household costs by out-of-pocket costs as percentage of current health expenditures") +
    ylab("Total household costs (2024 USD)") + 
    xlab("Out-of-pocket costs as percentage of current health expenditures\n(for all healthcare)") 

ggsave(plot=get_last_plot(), 
       file=here("output/supplement/costByOOPCHE.pdf"), 
       height = 7.5, width=11, units="in")

ggplot(data=countryEst) + 
    geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country), alpha=.75, size=1) +
    theme_minimal(base_size=18) + expand_limits(y=c(0, 30000), x=c(0,90)) +
    scale_x_continuous(breaks=seq(0,90,10)) + 
    scale_y_continuous(breaks=seq(0,30000,5000)) + 
    ggrepel::geom_text_repel(aes(x=uhc.index.trunc, y=dstb_total_cost_country, label=country.code), size=4) +
    ggtitle("non-RR/MDR TB household costs by Universal Healthcare Coverage (UHC) index") +
    ylab("Total household costs (2024 USD)")  + xlab("UHC index")

ggsave(plot=get_last_plot(), 
       file=here("output/supplement/costByUHC.pdf"), 
       height = 7.5, width=11, units="in")

# ggplot(data=countryEst) + 
#     geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country, color=median.income.trunc)) + 
#     theme_minimal(base_size=18) + expand_limits(y=0) +
#     ggtitle("non-RR/MDR TB household costs by Universal Health Care Index") + 
#     ylab("Total household costs (2024 USD)")  + xlab("UHC index")  +
#     theme(legend.position = "bottom") + 
#     labs(color="Median income")

# library(plotly)
# 
# plot_ly(x=countryEst$median.income.trunc, 
#         y=countryEst$uhc.index.trunc, 
#         z=countryEst$dstb_total_cost_country, type="scatter3d", mode="markers", 
#         color=countryEst$dstb_total_cost_country)

##############################################################################|
##### Table X                                       ###########################
##### Coefficient values                            ###########################
##############################################################################|
spread_draws(lmicSamps, 
             beta_dme_prediag, 
             beta_dme_prediag, 
             beta_dnme_prediag, 
             beta_ie_prediag, 
             beta_dme_intsvtx, 
             beta_dnme_intsvtx, 
             beta_ie_intsvtx, 
             beta_dme_contntx, 
             beta_dnme_contntx, 
             beta_ie_contntx,
             ndraws = 1000, seed = tbPtntCstSeed) %>% 
    mean_qi() %>% 
    select(! c(.width, .point, .interval)) %>% 
    exp() %>%
    round(digits = 2) %>% 
    matrix(nrow=9, byrow = TRUE) %>%
    ##### Save as a CSV  ###########################################|
    write.csv(here("output/tables/mainPredCoefs.csv"))

rbind(
    spread_draws(lmicSamps, 
                 mult_drtb_dme_prediag, 
                 mult_drtb_dnme_prediag, 
                 mult_drtb_ie_prediag, 
                 mult_drtb_dme_intsvtx, 
                 mult_drtb_dnme_intsvtx, 
                 mult_drtb_ie_intsvtx,
                 mult_drtb_dme_contntx, 
                 mult_drtb_dnme_contntx, 
                 mult_drtb_ie_contntx,
                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
        mean_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE),
    spread_draws(lmicSamps, 
                 mult_year_dme_prediag, 
                 mult_year_dnme_prediag, 
                 mult_year_ie_prediag, 
                 mult_year_dme_intsvtx, 
                 mult_year_dnme_intsvtx, 
                 mult_year_ie_intsvtx,
                 mult_year_dme_contntx, 
                 mult_year_dnme_contntx, 
                 mult_year_ie_contntx,
                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
        mean_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE) ,
    spread_draws(lmicSamps, 
                 mult_med_income_dme_prediag, 
                 mult_med_income_dnme_prediag, 
                 mult_med_income_ie_prediag, 
                 mult_med_income_dme_intsvtx, 
                 mult_med_income_dnme_intsvtx, 
                 mult_med_income_ie_intsvtx,
                 mult_med_income_dme_contntx, 
                 mult_med_income_dnme_contntx, 
                 mult_med_income_ie_contntx,
                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
        mean_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE) ,
    
    spread_draws(lmicSamps, 
                 mult_oop_perc_che_dme_prediag, 
                 mult_oop_perc_che_dnme_prediag, 
                 mult_oop_perc_che_ie_prediag, 
                 mult_oop_perc_che_dme_intsvtx, 
                 mult_oop_perc_che_dnme_intsvtx, 
                 mult_oop_perc_che_ie_intsvtx,
                 mult_oop_perc_che_dme_contntx, 
                 mult_oop_perc_che_dnme_contntx, 
                 mult_oop_perc_che_ie_contntx,
                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
        mean_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE) ,
    spread_draws(lmicSamps, 
                 mult_uhc_dme_prediag, 
                 mult_uhc_dnme_prediag, 
                 mult_uhc_ie_prediag, 
                 mult_uhc_dme_intsvtx, 
                 mult_uhc_dnme_intsvtx, 
                 mult_uhc_ie_intsvtx,
                 mult_uhc_dme_contntx, 
                 mult_uhc_dnme_contntx, 
                 mult_uhc_ie_contntx,
                 ndraws = 1000, seed = tbPtntCstSeed)%>% 
        mean_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE) 
) %>% 
    ##### Save as a CSV  ###########################################|
    write.csv(here("output/tables/covariateCoefsNew.csv"))



##### Random effects for countries not included in review
spread_draws(lmicSamps, 
             randeff_country_pred[country.id.new],
             ndraws = 1000, seed = tbPtntCstSeed) %>% 
             group_by(country.id.new) %>% 
             mean_qi() %>% 
             select(! c(.width, .point, .interval)) %>%
##### Save as a CSV  ###########################################|
write.csv(here("output/tables/randomEffectCountry.csv"))


full_join(spread_draws(lmicSamps, 
                       randeff_country_pred[country.id.new],
                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
        filter(country.id.new == 32), 
spread_draws(lmicSamps, 
             beta_dme_prediag,
             mult_med_income_dme_prediag,
             mult_oop_perc_che_dme_prediag,
             mult_uhc_dme_prediag,
             mult_drtb_dme_prediag, 
             mult_year_dme_prediag,
             ndraws = 1000, seed = tbPtntCstSeed)) %>% 
    mutate(tempCost = exp(beta_dme_prediag + randeff_country_pred +
                          mult_med_income_dme_prediag * 0.585286708 + 
                          mult_oop_perc_che_dme_prediag * 1.07898964 +
                          mult_uhc_dme_prediag * 0.607630897) * 
               exp(mult_year_dme_prediag * 1.991117) * 
               (1-0.04328018)) %>% 
              mean_qi(tempCost) %>% 
              select(! c(.width, .point, .interval)) %>% t()

##############################################################################|
##### Figure X                                       ##########################
##### Costs by phases of TB care cascade             ##########################
##############################################################################|   
treatmentEstimates <- countryEst %>% ungroup() %>%
    reframe("preDiagTotal" = sum(dstb_dme_prediag_cost_country,
                                 dstb_dnme_prediag_cost_country, 
                                 dstb_ie_prediag_cost_country),
            "intsvTxTotal" = sum(dstb_dme_intsvtx_cost_country,
                                 dstb_dnme_intsvtx_cost_country, 
                                 dstb_ie_intsvtx_cost_country),
            "contnTxTotal" = sum(dstb_dme_contntx_cost_country,
                                 dstb_dnme_contntx_cost_country, 
                                 dstb_ie_contntx_cost_country), 
            "TxTotal" = sum(intsvTxTotal, contnTxTotal),
            "Total" = sum(preDiagTotal, intsvTxTotal, 
                          contnTxTotal),
            .by=study.country) %>% 
    arrange(preDiagTotal/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, 
           preDiagTotal, contains("TxTotal")) %>%
    rename(`Pre-treatment` = preDiagTotal, 
           `Intensive treatment` = intsvTxTotal,
           `Continuation treatment` = contnTxTotal) %>%
    melt(id.vars = c("study.country","cntry_order"), 
         value.name = "Cost", 
         variable.name = "Category") %>%
    transform( order = factor(
        Category,
        levels=c( 'Pre-treatment','Intensive treatment',
                  'Continuation treatment'),
        ordered =TRUE)) 

countryCareCascadePlot <- ggplot() + 
    geom_bar(data = treatmentEstimates %>% filter(Category != "TxTotal"), 
             
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Pre-treatment', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom",
          legend.box.spacing = unit(.1, "lines")); countryCareCascadePlot

ggsave(plot = countryCareCascadePlot, 
       filename = here("output/figures/countryCareCascadePlot.pdf"), width = 8.5, height=11, units = "in")


##### Recreate this plot for only the top 30 TB countries 
top30CountryCareCascadePlot <- ggplot() + 
    geom_bar(data = treatmentEstimates %>% filter(Category != "TxTotal", 
                                                  study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 12) + 
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Pre-treatment', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom",
          plot.margin = margin(t=0, b=0, l=0, r=.3, "cm"),
          legend.box.spacing = unit(.1, "lines"),); top30CountryCareCascadePlot

ggsave(plot = top30CountryCareCascadePlot, 
       filename = here("output/figures/top30CountryCareCascadePlot.pdf"), width = 11, height=8, units = "in")

treatmentEstimates %>% filter(Category != "TxTotal", 
                              study.country %in% top30)%>% 
    mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                     grepl("Tanzania", study.country) ~ "Tanzania", 
                                     .default = study.country)) %>% 
    group_by(study.country) %>% 
    reframe(order = order, 
            fraction = Cost/sum(Cost)) %>%  ungroup() %>% 
    group_by(order) %>% 
    reframe(lb = round(quantile(fraction, probs = 0)*100,1), 
            med = round(quantile(fraction, probs = 0.5)*100, 1), 
            ub = round(quantile(fraction, probs = 1)*100,1))

ggsave(plot = top30CountryCareCascadePlot, 
       filename = here("output/figures/top30CountryCareCascadePlot.pdf"), width = 11, height=8, units = "in")

treatmentEstimates %>% filter(Category != "TxTotal")%>% 
    mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                     grepl("Tanzania", study.country) ~ "Tanzania", 
                                     .default = study.country)) %>% 
    group_by(study.country) %>% 
    reframe(order = order, 
            fraction = Cost/sum(Cost)) %>%  ungroup() %>% 
    group_by(order) %>% 
    reframe(lb = round(quantile(fraction, probs = 0)*100,1), 
            med = round(quantile(fraction, probs = 0.5)*100, 1), 
            ub = round(quantile(fraction, probs = 1)*100,1))

##############################################################################|
##### Figure X                                       ##########################
##### Costs by cost category (marginal)              ##########################
##############################################################################|   
categoryEstimates <- countryEst %>% ungroup() %>%
    reframe("DM" = sum(dstb_dme_prediag_cost_country,
                       dstb_dme_intsvtx_cost_country,
                       dstb_dme_contntx_cost_country),
            "DNM" = sum(dstb_dnme_prediag_cost_country,
                        dstb_dnme_intsvtx_cost_country, 
                        dstb_dnme_contntx_cost_country),
            "IND" = sum(dstb_ie_prediag_cost_country,
                        dstb_ie_intsvtx_cost_country, 
                        dstb_ie_contntx_cost_country), 
            "Total" = sum(DM, DNM, IND),
            .by=study.country) %>% 
    arrange(DM/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, 
           DM, DNM, IND) %>%
    rename(`Direct\nmedical` = DM, 
           `Direct\nnon-medical` = DNM,
           Indirect = IND) %>%
    melt(id.vars = c("study.country","cntry_order"), 
         value.name = "Cost", 
         variable.name = "Category") %>%
    transform( order = factor(
        Category,
        levels=c( 'Direct\nmedical','Direct\nnon-medical',
                  'Indirect'),
        ordered =TRUE)) 

countryCostCategoryPlot <- ggplot() + 
    geom_bar(data = categoryEstimates, 
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    scale_y_continuous(expand = c(0, 0)) +
    
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#6699CC", "#CC6699", "#CC9966"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom",
          legend.box.spacing = unit(.1, "lines")); countryCostCategoryPlot

ggsave(plot = countryCostCategoryPlot, 
       filename = here("output/figures/countryCostCategoryPlot.pdf"), width = 8.5, height=11, units = "in")


##### Recreate this plot for only the top 30 TB countries 
top30countryCostCategoryPlot <- ggplot() + 
    geom_bar(data = categoryEstimates %>% filter(study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 12) + 
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c(c("#6699CC", "#CC6699", "#CC9966")), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom",
          plot.margin = margin(t=0, b=0, l=0, r=.3, "cm"),
          legend.box.spacing = unit(.1, "lines")); top30countryCostCategoryPlot

ggsave(plot = top30countryCostCategoryPlot, 
       filename = here("output/figures/top30CountryCostCategoryPlotWide.pdf"), width = 11, height=8, units = "in")


categoryEstimates %>% filter(Category != "TxTotal", 
                              study.country %in% top30)%>% 
    mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                     grepl("Tanzania", study.country) ~ "Tanzania", 
                                     .default = study.country)) %>% 
    group_by(study.country) %>% 
    reframe(order = order, 
            fraction = Cost/sum(Cost)) %>%  ungroup() %>% 
    group_by(order) %>% 
    reframe(lb = round(quantile(fraction, probs = 0)*100,1), 
            med = round(quantile(fraction, probs = 0.5)*100, 1), 
            ub = round(quantile(fraction, probs = 1)*100,1))

categoryEstimates %>% filter(Category != "TxTotal")%>% 
    mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                     grepl("Tanzania", study.country) ~ "Tanzania", 
                                     .default = study.country)) %>% 
    group_by(study.country) %>% 
    reframe(order = order, 
            fraction = Cost/sum(Cost)) %>%  ungroup() %>% 
    group_by(order) %>% 
    reframe(lb = round(quantile(fraction, probs = 0)*100,1), 
            med = round(quantile(fraction, probs = 0.5)*100, 1), 
            ub = round(quantile(fraction, probs = 1)*100,1))



stackMarginalPlot <- ggpubr::ggarrange(top30CountryCareCascadePlot, 
                                       top30countryCostCategoryPlot, nrow=2, ncol=1)

ggsave(plot = stackMarginalPlot, 
       filename = here("output/figures/stackMarginalDistPlot.pdf"), width = 8, height=11, units = "in")
##############################################################################|
##### Figure X                                       ##########################
##### Costs by cost type (DME, DNME, IE) within care phase ####################
##############################################################################| 

##### Pre-diagnosis household costs of DSTB          ##########################
preDiagEstimates <- countryEst %>%
    group_by(study.country) %>%
    mutate("Total" = sum(dstb_dme_prediag_cost_country, 
                         dstb_dnme_prediag_cost_country, 
                         dstb_ie_prediag_cost_country)) %>%
    ungroup() %>%
    arrange(dstb_dme_prediag_cost_country/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, 
           dstb_dme_prediag_cost_country, 
           dstb_dnme_prediag_cost_country, 
           dstb_ie_prediag_cost_country, Total) %>%
    rename(`Direct\nmedical` = dstb_dme_prediag_cost_country, 
           `Direct\nnon-medical` = dstb_dnme_prediag_cost_country,
           `Indirect` = dstb_ie_prediag_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryPrediagPlot <- ggplot() + 
    geom_bar(data = preDiagEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of diagnostic costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#448866", "#66CC99", "#99ddbb"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom", legend.box.spacing = unit(.1, "lines")); countryPrediagPlot

ggsave(plot = countryPrediagPlot, 
       filename = here("output/figures/countryPrediagPlot.pdf"), width = 8.5, height=11, units = "in")

top30CountryPrediagPlot <- ggplot() + 
    geom_bar(data = preDiagEstimates %>% filter(Category != "Total", 
                                                study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of diagnostic costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#448866", "#66CC99", "#99ddbb"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom", legend.box.spacing = unit(.1, "lines"),
          plot.margin = margin(t=0, b=0, l=0, r=.2, "cm")); top30CountryPrediagPlot

ggsave(plot = top30CountryPrediagPlot, 
       filename = here("output/figures/top30CountryPrediagPlot.pdf"), width = 8.5, height=8.5, units = "in")

##### Intensive treatment household costs of DSTB          ####################

IntsvTxEstimates <- countryEst %>%
    group_by(study.country) %>%
    mutate("Total" = sum(dstb_dme_intsvtx_cost_country, 
                         dstb_dnme_intsvtx_cost_country, 
                         dstb_ie_intsvtx_cost_country)) %>%
    ungroup() %>%
    arrange(dstb_dme_intsvtx_cost_country/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, 
           dstb_dme_intsvtx_cost_country, 
           dstb_dnme_intsvtx_cost_country, 
           dstb_ie_intsvtx_cost_country, Total) %>%
    rename(`Direct\nmedical` = dstb_dme_intsvtx_cost_country, 
           `Direct\nnon-medical` = dstb_dnme_intsvtx_cost_country,
           `Indirect` = dstb_ie_intsvtx_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryIntsvTxPlot <- ggplot() +
    geom_bar(data = IntsvTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of intensive treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#884444", "#CC6666", "#dd9999"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom"); countryIntsvTxPlot

ggsave(plot = countryIntsvTxPlot, 
       filename = here("output/figures/countryIntsvTxPlot.pdf"), width = 8.5, height=11, units = "in")


top30CountryIntsvTxPlot <- ggplot() +
    geom_bar(data = IntsvTxEstimates %>% filter(Category != "Total", 
                                                study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of intensive treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#884444", "#CC6666", "#dd9999"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom", legend.box.spacing = unit(.1, "lines"),
          plot.margin = margin(t=0, b=0, l=0, r=0.2, "cm")); top30CountryIntsvTxPlot

ggsave(plot = top30CountryIntsvTxPlot, 
       filename = here("output/figures/top30CountryIntsvTxPlot.pdf"), width = 8.5, height=8.5, units = "in")

##### Continuation treatment household costs of DSTB          #################

ContnTxEstimates <- countryEst %>%
    group_by(study.country) %>%
    mutate("Total" = sum(dstb_dme_contntx_cost_country, 
                         dstb_dnme_contntx_cost_country, 
                         dstb_ie_contntx_cost_country)) %>%
    ungroup() %>%
    arrange(dstb_dme_contntx_cost_country/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, 
           dstb_dme_contntx_cost_country, 
           dstb_dnme_contntx_cost_country, 
           dstb_ie_contntx_cost_country, Total) %>%
    rename(`Direct\nmedical` = dstb_dme_contntx_cost_country, 
           `Direct\nnon-medical` = dstb_dnme_contntx_cost_country,
           `Indirect` = dstb_ie_contntx_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryContnTxPlot <- ggplot() +
    geom_bar(data = ContnTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of continuation treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#725f7d", "#9999CC", "#e2ccdf"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom"); countryContnTxPlot

ggsave(plot = countryContnTxPlot, 
       filename = here("output/figures/countryContnTxPlot.pdf"), width = 8.5, height=11, units = "in")

top30CountryContnTxPlot <- ggplot() +
    geom_bar(data = ContnTxEstimates %>% filter(Category != "Total", 
                                                study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() + ylab("Fraction of continuation treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#725f7d", "#9999CC", "#e2ccdf"), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom",legend.box.spacing = unit(.1, "lines"),
          plot.margin = margin(t=0, b=0, l=0, r=.3, "cm")); top30CountryContnTxPlot

ggsave(plot = top30CountryContnTxPlot, 
       filename = here("output/figures/top30CountryContnTxPlot.pdf"), width = 8.5, height=8.5, units = "in")


#### Put the top30 panels together
bottom <- ggpubr::ggarrange(top30CountryPrediagPlot, top30CountryIntsvTxPlot, top30CountryContnTxPlot, ncol=3, nrow=1, 
                            common.legend = FALSE, legend="bottom")

top30Stack <- ggpubr::ggarrange(top30CountryCareCascadePlot, bottom, 
                                nrow=2, ncol=1, heights = c(6,7),
                                common.legend = FALSE, legend = "bottom")

ggsave(plot = top30Stack, 
       filename = here("output/figures/top30StackPlot.pdf"), width = 9, height=11, units = "in")


##############################################################################|
#### Geographic estimates ####
##############################################################################|

lmicDSTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            dstb_total_cost_country[country.id.new], 
                                            ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   mean_qi(dstb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    select(country.code, in.review, dstb_total_cost_country, median.income) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"), 
           dstb_total_medInc = dstb_total_cost_country/median.income)

lmicDRTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            drtb_total_cost_country[country.id.new], 
                                            ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   mean_qi(drtb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    select(country.code, in.review, drtb_total_cost_country,median.income) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"), 
           drtb_total_medInc = drtb_total_cost_country/median.income)


world_dstb <-  ne_countries() %>% 
    filter(sovereignt != "Antarctica") %>% 
    select(sovereignt, sov_a3, geometry) %>% 
    rename(country.code = sov_a3) %>% 
    left_join(lmicDSTBPred_plot) 

world_drtb <-  ne_countries() %>% 
    filter(sovereignt != "Antarctica") %>% 
    select(sovereignt, sov_a3, geometry) %>% 
    rename(country.code = sov_a3) %>% 
    left_join(lmicDRTBPred_plot)

dstbCarePlot <- ggplot() +
    geom_sf(data = world_dstb, fill="grey85") + 
    geom_sf(data = world_dstb, mapping=aes(geometry=geometry, fill=dstb_total_cost_country)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Total household cost of TB", 
                        breaks=c(1000, 3000, 5000, 7000)) +
    theme_void() + theme(legend.position = "inside",
                           legend.position.inside = c(.15,.2)) + 
    expand_limits(fill=7000)   +
    
    # ggtitle("non-RR/MDR-TB care costs")+ 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))); dstbCarePlot


ggsave(plot = dstbCarePlot, filename = here("output/figures/dstbCostMap.pdf"), width =13, height = 6)

##### Scale non-RR/MDRTB costs by median income        ########################
dstbCareScldPlot <- ggplot() +
    geom_sf(data = world_dstb, fill="grey85") + 
    geom_sf(data = world_dstb, mapping=aes(geometry=geometry, fill=dstb_total_medInc)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", 
                        name = "Ratio of TB care cost\nto median income") +
    theme_void() + 
    expand_limits(fill=5) + 
    # ggtitle("non-RR/MDR-TB care costs") +
    theme(legend.position = "inside",
          legend.position.inside = c(.15,.2)); dstbCareScldPlot
    # guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    
    

ggsave(plot = dstbCareScldPlot, filename = here("output/figures/dstbCostMapMedInc.pdf"), width =13, height = 6)

drtbCarePlot <- ggplot() +
    geom_sf(data = world_drtb, fill="grey85") + 
    geom_sf(data = world_drtb, mapping=aes(geometry=geometry, fill=drtb_total_cost_country)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Total household cost of TB", 
                        breaks=c(0,10000, 20000, 30000, 40000)) +
    theme_void() + 
    theme(legend.position = "inside",
          legend.position.inside = c(.15,.2)) + 
    # ggtitle("RR/MDR-TB care costs") + 
    expand_limits(fill=40000) + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))); drtbCarePlot

ggsave(plot = drtbCarePlot, filename = here("output/figures/drtbCostMap.pdf"), width =13, height = 6)

##### Scale RR/MDRTB costs by median income        ############################
drtbCareScldPlot <- ggplot() +
    geom_sf(data = world_drtb, fill="grey85") + 
    geom_sf(data = world_drtb, mapping=aes(geometry=geometry, fill=drtb_total_medInc)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Ratio of TB care costs\nto median income") +
    theme_void() + 
    expand_limits(fill=40)+
    # ggtitle("RR/MDR-TB care costs") + 
    theme(legend.position = "inside",
          legend.position.inside = c(.15,.2))    ; drtbCareScldPlot 
    # guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +    
    

ggsave(plot = drtbCareScldPlot, filename = here("output/figures/drtbCostMapMedInc.pdf"),width =13, height = 6)

##############################################################################|
##### Main text Table 1                                   #####################
##### Mean DSTB with uncertainty intervals by subgroup  #####################
##############################################################################|
samples <- spread_draws(lmicSampsAll[[16]], 
                        ### non-RR/MDR-TB
                        dstb_dme_prediag_cost_country[country.id.new],
                        dstb_dnme_prediag_cost_country[country.id.new],
                        dstb_ie_prediag_cost_country[country.id.new],
                        dstb_dme_intsvtx_cost_country[country.id.new],
                        dstb_dnme_intsvtx_cost_country[country.id.new],
                        dstb_ie_intsvtx_cost_country[country.id.new],
                        dstb_dme_contntx_cost_country[country.id.new],
                        dstb_dnme_contntx_cost_country[country.id.new],
                        dstb_ie_contntx_cost_country[country.id.new],
                        dstb_dme_cost_country[country.id.new], 
                        dstb_dnme_cost_country[country.id.new], 
                        dstb_ie_cost_country[country.id.new], 
                        dstb_total_cost_country[country.id.new], 
                        ndraws = 1000, seed = tbPtntCstSeed) 

##############################################################################|
##### Table SX                                      ###########################
##### Total Mean DSTB with uncertainty intervals  ###########################
##############################################################################|

summaryNotif <- notif %>%
    filter(study.end.year == 2024, 
           country.code %in% covariates$country.code) %>% 
    summarize(notifAll = round(sum(notif, na.rm=TRUE)/ 1e6, 2)); summaryNotif

summaryEstUcrntyTbl <- samples %>% 
    left_join(covariatesAll, by="country.id.new") %>% 
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    group_by(`.draw`, country.id.new) %>% 
    mutate(wgtDME_PT   = weighted.mean(dstb_dme_prediag_cost_country, notif), 
           wgtDNME_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtIE_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtDME_IT   = weighted.mean(dstb_dme_intsvtx_cost_country, notif), 
           wgtDNME_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtIE_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtDME_CT   = weighted.mean(dstb_dme_contntx_cost_country, notif), 
           wgtDNME_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtIE_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtPT = wgtDME_PT + wgtDNME_PT + wgtIE_PT,
           wgtIT = wgtDME_IT + wgtDNME_IT + wgtIE_IT,
           wgtCT = wgtDME_CT + wgtDNME_CT + wgtIE_CT,
           wgtDME   = weighted.mean(dstb_dme_cost_country, notif), 
           wgtDNME  = weighted.mean(dstb_dnme_cost_country, notif),
           wgtIE    = weighted.mean(dstb_ie_cost_country, notif),
           wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>% 
    ungroup() %>% 
    mean_qi(wgtDME_PT, wgtDNME_PT, wgtIE_PT,
              wgtDME_IT, wgtDNME_IT, wgtIE_IT,
              wgtDME_CT, wgtDNME_CT, wgtIE_CT,
              wgtPT, wgtIT, wgtCT, 
              wgtDME, wgtDNME, wgtIE, 
              wgtTotal) %>% 
    select(!c(.width, .point, .interval)) %>% round() 

colnames(summaryEstUcrntyTbl)[seq(1, ncol(summaryEstUcrntyTbl), 3)] <- paste(colnames(summaryEstUcrntyTbl)[seq(1, ncol(summaryEstUcrntyTbl), 3)], "est", sep=".")

summaryEstUcrntyTbl <- summaryEstUcrntyTbl %>% 
    pivot_longer(everything(), names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>% 
    pivot_wider(id_cols = Category, names_from = Type, values_from = Cost) %>% 
    mutate(across(est:upper, round), 
           formatted = paste0(est, " (", 
                              lower, ", ", 
                              upper, ")")) ; summaryEstUcrntyTbl

summaryEstUcrntyTbl %>% 
    write.csv(here("output/tables/allLMICMeanCosts.csv"))


##############################################################################|
##### Table SX                                      ###########################
##### Regional mean DSTB with uncertainty intervals  ########################
##############################################################################|

regionNotif <- notif %>%
    filter(study.end.year %in% 2023:2024) %>% 
    pivot_wider(id_cols = c(who.region,country.code),  names_from = study.end.year, values_from = notif) %>%
    mutate(notif = ifelse(is.na(`2024`),
                          `2023`, `2024`)) %>% 
    group_by(who.region) %>% 
    summarize(notifAll = round(sum(notif)/ 1e6, 2)); regionNotif

regionCntry <- notif %>%
    filter(study.end.year == 2024) %>% 
    group_by(who.region) %>% 
    count(who.region); regionCntry

regionEstUncrntyTbl <- samples %>% 
    left_join(covariatesAll, by="country.id.new") %>% 
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    group_by(`.draw`, country.id.new) %>% 
    mutate(wgtDME_PT   = weighted.mean(dstb_dme_prediag_cost_country, notif), 
           wgtDNME_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtIE_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtDME_IT   = weighted.mean(dstb_dme_intsvtx_cost_country, notif), 
           wgtDNME_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtIE_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtDME_CT   = weighted.mean(dstb_dme_contntx_cost_country, notif), 
           wgtDNME_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtIE_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtPT = wgtDME_PT + wgtDNME_PT + wgtIE_PT,
           wgtIT = wgtDME_IT + wgtDNME_IT + wgtIE_IT,
           wgtCT = wgtDME_CT + wgtDNME_CT + wgtIE_CT,
           wgtDME   = weighted.mean(dstb_dme_cost_country, notif), 
           wgtDNME  = weighted.mean(dstb_dnme_cost_country, notif),
           wgtIE    = weighted.mean(dstb_ie_cost_country, notif),
           wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>% 
    ungroup() %>% 
    group_by(who.region) %>% 
    mean_qi(wgtDME_PT, wgtDNME_PT, wgtIE_PT,
              wgtDME_IT, wgtDNME_IT, wgtIE_IT,
              wgtDME_CT, wgtDNME_CT, wgtIE_CT,
              wgtPT, wgtIT, wgtCT, 
              wgtDME, wgtDNME, wgtIE, 
              wgtTotal) %>% 
    select(!c(.width, .point, .interval))

colnames(regionEstUncrntyTbl)[seq(2, ncol(regionEstUncrntyTbl), 3)] <- paste(colnames(regionEstUncrntyTbl)[seq(2, ncol(regionEstUncrntyTbl), 3)], "est", sep=".")

regionEstUncrntyTbl2 <- regionEstUncrntyTbl %>% 
    pivot_longer(`wgtDME_PT.est`:`wgtTotal.upper`, names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>% 
    pivot_wider(id_cols = c(who.region, Category), names_from = Type, values_from = Cost) %>% 
    mutate(across(est:upper, round), 
           formatted = paste0(est, " (", 
                              lower, ", ", 
                              upper, ")")) 
regionEstUncrntyTbl2 %>% 
    write.csv(here("output/tables/regionMeanCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### TB Inc Group mean DSTB with uncertainty intervals  ####################
##############################################################################|
# tbIncQuant <- left_join(LMIC, incRate %>% select(!study.country)) %>% 
#     do(data.frame(t(quantile(.$inc100K, na.rm=TRUE)))) %>% unlist() %>%
#     as.numeric()
# 
# 
# tbIncEstUcrntyTbl <- left_join(countryEst, incRate) %>% 
#     mutate(inc100K = ifelse(study.country == "Cote d'Ivoire",
#                             99.00, inc100K), 
#            incQuant = case_when(inc100K < tbIncQuant[2] ~ 1,
#                                 inc100K < tbIncQuant[3] ~ 2,
#                                 inc100K < tbIncQuant[4] ~ 3,
#                                 inc100K < tbIncQuant[5] ~ 4)) %>%
#     select(incQuant, 
#            contains("dstb")) %>%
#     pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
#     group_by(name, incQuant) %>% 
#     mean_qi(value) %>% 
#     mutate(across(value:.upper, round), 
#            formatted = paste0(value, " (", 
#                               .lower, ", ", 
#                               .upper, ")")) %>% 
#     select(!c(value, .lower, .upper)) %>%
#     pivot_wider(names_from = name, values_from = formatted) %>% 
#     write.csv(here("output/tables/tbIncMeanCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### Top 30 TB mean DSTB with uncertainty intervals     ####################
##############################################################################|

### Note we did not include DPR Korea so we only have 29 countries 
top30notif <- notif %>% left_join(LMIC %>% 
                                  mutate(study.country = 
                                        case_when(study.country == "Congo, Rep." ~ "Congo", 
                                                  study.country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
                                                  study.country == "Tanzania" ~ "United Republic of Tanzania", 
                                                  .default=study.country))) %>% 
              filter(study.country %in% top30, 
                     study.end.year == 2024) %>% 
              summarize(notifAll = round(sum(notif)/ 1e6, 2)); top30notif





top30EstUcrntyTbl <- samples %>% 
    left_join(covariatesAll, by="country.id.new") %>% 
    filter(study.country %in% top30) %>%
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    group_by(`.draw`, country.id.new) %>% 
    mutate(wgtDME_PT   = weighted.mean(dstb_dme_prediag_cost_country, notif), 
           wgtDNME_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtIE_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtDME_IT   = weighted.mean(dstb_dme_intsvtx_cost_country, notif), 
           wgtDNME_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtIE_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtDME_CT   = weighted.mean(dstb_dme_contntx_cost_country, notif), 
           wgtDNME_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtIE_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtPT = wgtDME_PT + wgtDNME_PT + wgtIE_PT,
           wgtIT = wgtDME_IT + wgtDNME_IT + wgtIE_IT,
           wgtCT = wgtDME_CT + wgtDNME_CT + wgtIE_CT,
           wgtDME   = weighted.mean(dstb_dme_cost_country, notif), 
           wgtDNME  = weighted.mean(dstb_dnme_cost_country, notif),
           wgtIE    = weighted.mean(dstb_ie_cost_country, notif),
           wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>% 
    ungroup() %>% 
    mean_qi(wgtDME_PT, wgtDNME_PT, wgtIE_PT,
              wgtDME_IT, wgtDNME_IT, wgtIE_IT,
              wgtDME_CT, wgtDNME_CT, wgtIE_CT,
              wgtPT, wgtIT, wgtCT, 
              wgtDME, wgtDNME, wgtIE, 
              wgtTotal) %>% 
    select(!c(.width, .point, .interval)) %>% round() 

colnames(top30EstUcrntyTbl)[seq(1, ncol(top30EstUcrntyTbl), 3)] <- paste(colnames(top30EstUcrntyTbl)[seq(1, ncol(top30EstUcrntyTbl), 3)], "est", sep=".")

top30EstUcrntyTbl <- top30EstUcrntyTbl %>% 
    pivot_longer(everything(), names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>% 
    pivot_wider(id_cols = Category, names_from = Type, values_from = Cost) %>% 
    mutate(across(est:upper, round), 
           formatted = paste0(est, " (", 
                              lower, ", ", 
                              upper, ")")) ; top30EstUcrntyTbl

top30EstUcrntyTbl  %>%
    write.csv(here("output/tables/top30MeanCosts.csv"))

##############################################################################|
##### Table SX                                                    #############
##### World Bank Group mean DSTB with uncertainty intervals     #############
##############################################################################|
worldBankNotif <- notif %>%
    left_join(countryEst %>% select(country.code, class25)) %>%
    filter(study.end.year %in% 2023:2024) %>% 
    pivot_wider(id_cols = c(class25,country.code),  names_from = study.end.year, values_from = notif) %>%
    mutate(notif = ifelse(is.na(`2024`),
                          `2023`, `2024`)) %>% 
    group_by(class25) %>% 
    summarize(notifAll = round(sum(notif)/ 1e6, 2)); worldBankNotif

worldBankEstUncrntyTbl <- samples %>% 
    left_join(covariatesAll, by="country.id.new") %>% 
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
    group_by(`.draw`, country.id.new) %>% 
    mutate(wgtDME_PT   = weighted.mean(dstb_dme_prediag_cost_country, notif), 
           wgtDNME_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtIE_PT   = weighted.mean(dstb_dnme_prediag_cost_country, notif), 
           wgtDME_IT   = weighted.mean(dstb_dme_intsvtx_cost_country, notif), 
           wgtDNME_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtIE_IT   = weighted.mean(dstb_dnme_intsvtx_cost_country, notif), 
           wgtDME_CT   = weighted.mean(dstb_dme_contntx_cost_country, notif), 
           wgtDNME_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtIE_CT   = weighted.mean(dstb_dnme_contntx_cost_country, notif), 
           wgtPT = wgtDME_PT + wgtDNME_PT + wgtIE_PT,
           wgtIT = wgtDME_IT + wgtDNME_IT + wgtIE_IT,
           wgtCT = wgtDME_CT + wgtDNME_CT + wgtIE_CT,
           wgtDME   = weighted.mean(dstb_dme_cost_country, notif), 
           wgtDNME  = weighted.mean(dstb_dnme_cost_country, notif),
           wgtIE    = weighted.mean(dstb_ie_cost_country, notif),
           wgtTotal = weighted.mean(dstb_total_cost_country, notif)) %>% 
    ungroup() %>% 
    group_by(class25) %>% 
    mean_qi(wgtDME_PT, wgtDNME_PT, wgtIE_PT,
              wgtDME_IT, wgtDNME_IT, wgtIE_IT,
              wgtDME_CT, wgtDNME_CT, wgtIE_CT,
              wgtPT, wgtIT, wgtCT, 
              wgtDME, wgtDNME, wgtIE, 
              wgtTotal) %>% 
    select(!c(.width, .point, .interval))

colnames(worldBankEstUncrntyTbl)[seq(2, ncol(worldBankEstUncrntyTbl), 3)] <- paste(colnames(worldBankEstUncrntyTbl)[seq(2, ncol(worldBankEstUncrntyTbl), 3)], "est", sep=".")

worldBankEstUncrntyTbl2 <- worldBankEstUncrntyTbl %>% 
    pivot_longer(`wgtDME_PT.est`:`wgtTotal.upper`, names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>% 
    pivot_wider(id_cols = c(class25, Category), names_from = Type, values_from = Cost) %>% 
    mutate(across(est:upper, round), 
           formatted = paste0(est, " (", 
                              lower, ", ", 
                              upper, ")")) 
worldBankEstUncrntyTbl2 %>% 
    write.csv(here("output/tables/worldBankMeanCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### Country level DSTB with uncertainty intervals ###########################
##############################################################################|

countryEstUcrntyTbl <- left_join(covariates, 
                                 spread_draws(lmicSamps, 
                                              dstb_dme_prediag_cost_country[country.id.new],
                                              dstb_dnme_prediag_cost_country[country.id.new],
                                              dstb_ie_prediag_cost_country[country.id.new],
                                              dstb_dme_intsvtx_cost_country[country.id.new],
                                              dstb_dnme_intsvtx_cost_country[country.id.new],
                                              dstb_ie_intsvtx_cost_country[country.id.new],
                                              dstb_dme_contntx_cost_country[country.id.new],
                                              dstb_dnme_contntx_cost_country[country.id.new],
                                              dstb_ie_contntx_cost_country[country.id.new],
                                              dstb_total_cost_country[country.id.new], 
                                              ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                     group_by(country.id.new) %>% 
                                     mean_qi(dstb_dme_prediag_cost_country, 
                                               dstb_dnme_prediag_cost_country, 
                                               dstb_ie_prediag_cost_country,
                                               dstb_dme_intsvtx_cost_country, 
                                               dstb_dnme_intsvtx_cost_country, 
                                               dstb_ie_intsvtx_cost_country,
                                               dstb_dme_contntx_cost_country, 
                                               dstb_dnme_contntx_cost_country, 
                                               dstb_ie_contntx_cost_country,
                                               dstb_total_cost_country), 
                                 join_by(country.id.new)) %>% 
    mutate(across(dstb_dme_prediag_cost_country:dstb_total_cost_country.upper, round)) %>%
    mutate("DME pre-diagnosis" = paste0(dstb_dme_prediag_cost_country, " (", 
                                        dstb_dme_prediag_cost_country.lower, ", ", 
                                        dstb_dme_prediag_cost_country.upper, ")"), 
           "DNME pre-diagnosis" = paste0(dstb_dnme_prediag_cost_country, " (", 
                                         dstb_dnme_prediag_cost_country.lower, ", ", 
                                         dstb_dnme_prediag_cost_country.upper, ")"),
           "Indirect pre-diagnosis" = paste0(dstb_ie_prediag_cost_country, " (", 
                                             dstb_ie_prediag_cost_country.lower, ", ", 
                                             dstb_ie_prediag_cost_country.upper, ")"),
           "DME Intensive Tx" = paste0(dstb_dme_intsvtx_cost_country, " (", 
                                       dstb_dme_intsvtx_cost_country.lower, ", ", 
                                       dstb_dme_intsvtx_cost_country.upper, ")"), 
           "DNME Intensive Tx" = paste0(dstb_dnme_intsvtx_cost_country, " (", 
                                        dstb_dnme_intsvtx_cost_country.lower, ", ", 
                                        dstb_dnme_intsvtx_cost_country.upper, ")"),
           "Indirect Intensive Tx" = paste0(dstb_ie_intsvtx_cost_country, " (", 
                                            dstb_ie_intsvtx_cost_country.lower, ", ", 
                                            dstb_ie_intsvtx_cost_country.upper, ")"), 
           "DME Continuation Tx" = paste0(dstb_dme_contntx_cost_country, " (", 
                                          dstb_dme_contntx_cost_country.lower, ", ", 
                                          dstb_dme_contntx_cost_country.upper, ")"), 
           "DNME Continuation Tx" = paste0(dstb_dnme_contntx_cost_country, " (", 
                                           dstb_dnme_contntx_cost_country.lower, ", ", 
                                           dstb_dnme_contntx_cost_country.upper, ")"),
           "Indirect Continuation Tx" = paste0(dstb_ie_contntx_cost_country, " (", 
                                               dstb_ie_contntx_cost_country.lower, ", ", 
                                               dstb_ie_contntx_cost_country.upper, ")"),
           "Total TB care costs" = paste0(dstb_total_cost_country, " (", 
                                          dstb_total_cost_country.lower, ", ", 
                                          dstb_total_cost_country.upper, ")")
    ) %>% select(study.country, in.review, 
                 `DME pre-diagnosis`:`Total TB care costs`)

##### Save as a CSV for Supplement ###########################################|
write.csv(countryEstUcrntyTbl, here("output/tables/countryDSTBCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### Country level DRTB with uncertainty intervals ###########################
##############################################################################|

countryEstDRTBUcrntyTbl <- left_join(covariates, 
                                 spread_draws(lmicSamps, 
                                              drtb_dme_prediag_cost_country[country.id.new],
                                              drtb_dnme_prediag_cost_country[country.id.new],
                                              drtb_ie_prediag_cost_country[country.id.new],
                                              drtb_dme_intsvtx_cost_country[country.id.new],
                                              drtb_dnme_intsvtx_cost_country[country.id.new],
                                              drtb_ie_intsvtx_cost_country[country.id.new],
                                              drtb_dme_contntx_cost_country[country.id.new],
                                              drtb_dnme_contntx_cost_country[country.id.new],
                                              drtb_ie_contntx_cost_country[country.id.new],
                                              drtb_total_cost_country[country.id.new], 
                                              ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                     group_by(country.id.new) %>% 
                                     mean_qi(drtb_dme_prediag_cost_country, 
                                               drtb_dnme_prediag_cost_country, 
                                               drtb_ie_prediag_cost_country,
                                               drtb_dme_intsvtx_cost_country, 
                                               drtb_dnme_intsvtx_cost_country, 
                                               drtb_ie_intsvtx_cost_country,
                                               drtb_dme_contntx_cost_country, 
                                               drtb_dnme_contntx_cost_country, 
                                               drtb_ie_contntx_cost_country,
                                               drtb_total_cost_country), 
                                 join_by(country.id.new)) %>% 
    mutate(across(drtb_dme_prediag_cost_country:drtb_total_cost_country.upper, round)) %>%
    mutate("DME pre-diagnosis" = paste0(drtb_dme_prediag_cost_country, " (", 
                                        drtb_dme_prediag_cost_country.lower, ", ", 
                                        drtb_dme_prediag_cost_country.upper, ")"), 
           "DNME pre-diagnosis" = paste0(drtb_dnme_prediag_cost_country, " (", 
                                         drtb_dnme_prediag_cost_country.lower, ", ", 
                                         drtb_dnme_prediag_cost_country.upper, ")"),
           "Indirect pre-diagnosis" = paste0(drtb_ie_prediag_cost_country, " (", 
                                             drtb_ie_prediag_cost_country.lower, ", ", 
                                             drtb_ie_prediag_cost_country.upper, ")"),
           "DME Intensive Tx" = paste0(drtb_dme_intsvtx_cost_country, " (", 
                                       drtb_dme_intsvtx_cost_country.lower, ", ", 
                                       drtb_dme_intsvtx_cost_country.upper, ")"), 
           "DNME Intensive Tx" = paste0(drtb_dnme_intsvtx_cost_country, " (", 
                                        drtb_dnme_intsvtx_cost_country.lower, ", ", 
                                        drtb_dnme_intsvtx_cost_country.upper, ")"),
           "Indirect Intensive Tx" = paste0(drtb_ie_intsvtx_cost_country, " (", 
                                            drtb_ie_intsvtx_cost_country.lower, ", ", 
                                            drtb_ie_intsvtx_cost_country.upper, ")"), 
           "DME Continuation Tx" = paste0(drtb_dme_contntx_cost_country, " (", 
                                          drtb_dme_contntx_cost_country.lower, ", ", 
                                          drtb_dme_contntx_cost_country.upper, ")"), 
           "DNME Continuation Tx" = paste0(drtb_dnme_contntx_cost_country, " (", 
                                           drtb_dnme_contntx_cost_country.lower, ", ", 
                                           drtb_dnme_contntx_cost_country.upper, ")"),
           "Indirect Continuation Tx" = paste0(drtb_ie_contntx_cost_country, " (", 
                                               drtb_ie_contntx_cost_country.lower, ", ", 
                                               drtb_ie_contntx_cost_country.upper, ")"),
           "Total TB care costs" = paste0(drtb_total_cost_country, " (", 
                                          drtb_total_cost_country.lower, ", ", 
                                          drtb_total_cost_country.upper, ")")
    ) %>% select(study.country, in.review, 
                 `DME pre-diagnosis`:`Total TB care costs`)

##### Save as a CSV for Supplement ###########################################|
write.csv(countryEstDRTBUcrntyTbl, here("output/tables/countryDRTBCosts.csv"))


##############################################################################|
##### Difference in resistant TB and non-resistant TB  ########################
##############################################################################|
drtbRatioEst <-
left_join(covariates, 
          spread_draws(lmicSamps, 
                       dstb_total_cost_country[country.id.new], 
                       drtb_total_cost_country[country.id.new], 
                       ndraws = 1000, seed = tbPtntCstSeed) %>% 
              group_by(country.id.new) %>% 
              mutate(drtb_ratio = drtb_total_cost_country/dstb_total_cost_country) %>% 
              mean_qi(drtb_ratio), 
          join_by(country.id.new)) %>% 
    select(study.country, country.code, drtb_ratio, .lower, .upper) %>% 
mutate(across(drtb_ratio:.upper, \(x) round(x, digits=1)),
       formatted = paste0(drtb_ratio, " (",
                          .lower, ", ",
                          .upper, ")"))

##### Save as a CSV for Supplement ###########################################|
write.csv(drtbRatioEst, here("output/tables/countryDrTbRatio.csv"))


spread_draws(lmicSamps, 
             dstb_total_cost_country[country.id.new], 
             drtb_total_cost_country[country.id.new], 
             ndraws = 1000, seed = tbPtntCstSeed) %>% 
mutate(drtb_ratio = drtb_total_cost_country/dstb_total_cost_country) %>% 
ungroup() %>% 
mean_qi(drtb_ratio) %>% 
select(drtb_ratio, .lower, .upper) %>% 
mutate(across(drtb_ratio:.upper, \(x) round(x, digits=1)),
       formatted = paste0(drtb_ratio, " (",
                          .lower, ", ",
                          .upper, ")"))

countryEstBurden <- left_join(countryEst, incRate)

ggplot(countryEstBurden, aes(x=dstb_total_cost_country, 
                             y=drtb_total_cost_country, 
                             color=inc100K)) + geom_point(size=4)  + 
    theme_minimal(base_size=24) + 
    ylab("Estimated DRTB care cost") + 
    xlab("Estimated DSTB care cost") + 
    labs(color = "WHO estimated TB incidence per 100K persons") +
    theme(legend.position = "bottom") + 
    scale_x_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    guides(color = guide_colourbar(barwidth = 30))

ggplot(countryEstBurden, aes(x=dstb_total_cost_country, 
                             y=drtb_total_cost_country, 
                             color=median.income)) + geom_point(size=4)  + 
    theme_minimal(base_size=24) + 
    ylab("Estimated DRTB care cost") + 
    xlab("Estimated DSTB care cost") + 
    labs(color = "Median annual income") +
    theme(legend.position = "bottom") + 
    scale_x_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    # scale_color_gradient(low="darkgreen", high="green1") +  
    guides(color = guide_colourbar(barwidth = 30)) + 
    ggtitle("Country-level household costs due to tuberculosis disease")

ggplot(countryEstBurden, aes(x=dstb_total_cost_country, 
                             y=drtb_total_cost_country, 
                             color=uhc.index)) + geom_point(size=4)  + 
    # geom_hline(aes(yintercept = median(drtb_total_cost_country))) + 
    # geom_vline(aes(xintercept = median(dstb_total_cost_country))) + 
    
    theme_minimal(base_size=24) + 
    ylab("Estimated DRTB care cost") + 
    xlab("Estimated DSTB care cost") + 
    labs(color = "WHO Universal health\ncare coverage index") +
    theme(legend.position = "bottom") + 
    scale_x_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    # scale_color_gradient(low="darkgreen", high="green1") + 
    guides(color = guide_colourbar(barwidth = 30)) + 
    ggtitle("Country-level household costs due to tuberculosis disease")

ggplot(countryEstBurden, aes(x=dstb_total_cost_country, 
                             y=drtb_total_cost_country, 
                             color=oop.perc.che)) + geom_point(size=4)  + 
    theme_minimal(base_size=24) + 
    ylab("Estimated DRTB care cost") + 
    xlab("Estimated DSTB care cost") + 
    labs(color = "Out-of-pocket costs as percentage\nof current health expenditures") +
    theme(legend.position = "bottom") + 
    scale_x_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  # labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    # scale_color_gradient(low="darkgreen", high="green1") +  
    guides(color = guide_colourbar(barwidth = 30)) + 
    ggtitle("Country-level household costs due to tuberculosis disease")

##############################################################################|
##### Table SX                                      ###########################
##### Time effect                                   ####################
##############################################################################|

# annualPC <- spread_draws(totalModelTimeOnly, 
#                          
#                          mult_year_dme_prediag,
#                          mult_year_dnme_prediag,
#                          mult_year_ie_prediag,
#                          mult_year_dme_intsvtx,
#                          mult_year_dnme_intsvtx,
#                          mult_year_ie_intsvtx,
#                          mult_year_dme_contntx,
#                          mult_year_dnme_contntx,
#                          mult_year_ie_contntx, 
#                          
#                          mult_yrdr_intx_dme_prediag,
#                          mult_yrdr_intx_dnme_prediag,
#                          mult_yrdr_intx_ie_prediag,
#                          mult_yrdr_intx_dme_intsvtx,
#                          mult_yrdr_intx_dnme_intsvtx,
#                          mult_yrdr_intx_ie_intsvtx,
#                          mult_yrdr_intx_dme_contntx,
#                          mult_yrdr_intx_dnme_contntx,
#                          mult_yrdr_intx_ie_contntx,
#                          ndraws = 1000, seed = tbPtntCstSeed) %>%
#     mutate(dstb_yr_dme_prediag = exp(mult_year_dme_prediag / sd(modelData$study.end.year)) - 1, 
#            dstb_yr_dnme_prediag = exp(mult_year_dnme_prediag / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_prediag = exp(mult_year_ie_prediag / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx / sd(modelData$study.end.year)) -1,  
#            dstb_yr_dme_contntx = exp(mult_year_dme_contntx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dnme_contntx = exp(mult_year_dnme_contntx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_contntx = exp(mult_year_ie_contntx / sd(modelData$study.end.year)) -1,
#            drtb_yr_dme_prediag = exp(mult_year_dme_prediag + mult_yrdr_intx_dme_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_prediag = exp(mult_year_dnme_prediag + mult_yrdr_intx_dnme_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_prediag = exp(mult_year_ie_prediag + mult_yrdr_intx_ie_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1,  
#            drtb_yr_dme_contntx = exp(mult_year_dme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_contntx = exp(mult_year_dnme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_contntx = exp(mult_year_ie_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year) - 1)) %>%
#     select(! contains("mult")) %>% 
#     pivot_longer(dstb_yr_dme_prediag:drtb_yr_ie_contntx, names_to = "Parameter", values_to = "Value") %>%
#     group_by(Parameter) %>% 
#     mean_qi() %>% 
#     select(! c(.width, .point, .interval)) %>%
#     mutate(across(Value:.upper, \(x) round(x, digits = 2)*100), 
#            formatted = paste0(Value, " (", 
#                               .lower, ", ", 
#                               .upper, ")")) 
# 
# write.csv(annualPC, file=here("output/tables/annualPercChngTimeOnly.csv"))
# 
# annualPC_aCovs <- spread_draws(totalModelTimeaCovs, 
#                                
#                                mult_year_dme_prediag,
#                                mult_year_dnme_prediag,
#                                mult_year_ie_prediag,
#                                mult_year_dme_intsvtx,
#                                mult_year_dnme_intsvtx,
#                                mult_year_ie_intsvtx,
#                                mult_year_dme_contntx,
#                                mult_year_dnme_contntx,
#                                mult_year_ie_contntx, 
#                                
#                                mult_yrdr_intx_dme_prediag,
#                                mult_yrdr_intx_dnme_prediag,
#                                mult_yrdr_intx_ie_prediag,
#                                mult_yrdr_intx_dme_intsvtx,
#                                mult_yrdr_intx_dnme_intsvtx,
#                                mult_yrdr_intx_ie_intsvtx,
#                                mult_yrdr_intx_dme_contntx,
#                                mult_yrdr_intx_dnme_contntx,
#                                mult_yrdr_intx_ie_contntx,
#                                ndraws = 1000, seed = tbPtntCstSeed) %>%
#     mutate(dstb_yr_dme_prediag = exp(mult_year_dme_prediag / sd(modelData$study.end.year)) - 1, 
#            dstb_yr_dnme_prediag = exp(mult_year_dnme_prediag / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_prediag = exp(mult_year_ie_prediag / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx / sd(modelData$study.end.year)) -1,  
#            dstb_yr_dme_contntx = exp(mult_year_dme_contntx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_dnme_contntx = exp(mult_year_dnme_contntx / sd(modelData$study.end.year)) -1, 
#            dstb_yr_ie_contntx = exp(mult_year_ie_contntx / sd(modelData$study.end.year)) -1,
#            drtb_yr_dme_prediag = exp(mult_year_dme_prediag + mult_yrdr_intx_dme_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_prediag = exp(mult_year_dnme_prediag + mult_yrdr_intx_dnme_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_prediag = exp(mult_year_ie_prediag + mult_yrdr_intx_ie_prediag / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1,  
#            drtb_yr_dme_contntx = exp(mult_year_dme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_dnme_contntx = exp(mult_year_dnme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
#            drtb_yr_ie_contntx = exp(mult_year_ie_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year) - 1)) %>%
#     select(! contains("mult")) %>% 
#     pivot_longer(dstb_yr_dme_prediag:drtb_yr_ie_contntx, names_to = "Parameter", values_to = "Value") %>%
#     group_by(Parameter) %>% 
#     mean_qi() %>% 
#     select(! c(.width, .point, .interval)) %>%
#     mutate(across(Value:.upper, \(x) round(x, digits = 2)*100), 
#            formatted = paste0(Value, " (", 
#                               .lower, ", ", 
#                               .upper, ")")) 
# 
# write.csv(annualPC_aCovs, file=here("output/tables/annualPercChngAllCovs.csv"))

##############################################################################|
#### Time trend estimates ####
##############################################################################|
##### Time plots from running the model for all countries and years
##############################################################################|
allYearsCostAll <- data.frame()
for (yr in 2009:2024){  
    left_join(covariatesTime %>% filter(study.end.year == yr), 
              spread_draws(lmicSampsTime[[as.character(yr)]], 
                           ### non-RR/MDR-TB
                           dstb_dme_prediag_cost_country[country.id.new],
                           dstb_dnme_prediag_cost_country[country.id.new],
                           dstb_ie_prediag_cost_country[country.id.new],
                           dstb_dme_intsvtx_cost_country[country.id.new],
                           dstb_dnme_intsvtx_cost_country[country.id.new],
                           dstb_ie_intsvtx_cost_country[country.id.new],
                           dstb_dme_contntx_cost_country[country.id.new],
                           dstb_dnme_contntx_cost_country[country.id.new],
                           dstb_ie_contntx_cost_country[country.id.new],
                           dstb_total_cost_country[country.id.new], 
                           ### RR/MDR-TB
                           # drtb_dme_prediag_cost_country[country.id.new],
                           # drtb_dnme_prediag_cost_country[country.id.new],
                           # drtb_ie_prediag_cost_country[country.id.new],
                           # drtb_dme_intsvtx_cost_country[country.id.new],
                           # drtb_dnme_intsvtx_cost_country[country.id.new],
                           # drtb_ie_intsvtx_cost_country[country.id.new],
                           # drtb_dme_contntx_cost_country[country.id.new],
                           # drtb_dnme_contntx_cost_country[country.id.new],
                           # drtb_ie_contntx_cost_country[country.id.new],
                           # drtb_total_cost_country[country.id.new], 
                           ndraws = 1000, seed = tbPtntCstSeed), 
              by = "country.id.new") %>%
        select(country.code, study.country, `.draw`, 
               study.end.year,
               contains("dstb")) -> y
    
    
    allYearsCostAll <- rbind(allYearsCostAll, y)
}

# Check the total costs unweighted 
# tmp <- allYearsCostAll %>% 
#        group_by(country.code, study.end.year) %>% 
#        mean_qi(dstb_total_cost_country) %>% 
#        left_join(regionWHO %>% rename(who.region = `World.regions.according.to.WHO`, 
#                                       country.code = Code), by="country.code") %>% 
#        left_join(covariatesTime) 
# 
# ggplot(data=tmp %>% filter(grepl("Western", who.region))) +
#     geom_point(aes(x=study.end.year, y=dstb_total_cost_country)) +
#     theme_minimal() +
#     facet_wrap(facets=vars(country.code)) +
#     expand_limits(y=0) + 
#     theme(legend.position = "bottom")
#      
# ggplot(data=tmp) +
#        geom_point(aes(x=median.income.trunc, y=dstb_total_cost_country, color=study.end.year)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")
# 
# ggplot(data=tmp) +
#        geom_point(aes(x=median.income.trunc, y=dstb_total_cost_country, color=who.region)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")
# 
# ggplot(data=tmp) +
#        geom_point(aes(x=oop.perc.che.trunc, y=dstb_total_cost_country, color=study.end.year)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")
# 
# ggplot(data=tmp) +
#        geom_point(aes(x=oop.perc.che.trunc, y=dstb_total_cost_country, color=who.region)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")
# 
# ggplot(data=tmp) +
#        geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country, color=study.end.year)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")
# 
# ggplot(data=tmp) +
#        geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country, color=who.region)) +
#        theme_minimal() +
#        facet_wrap(facets=vars(country.code)) +
#        expand_limits(y=0) + 
#        theme(legend.position = "bottom")

### Read in notification data and format
notif <- read.csv(here("data/TB_notifications_2025-12-19.csv")) %>% 
    filter(year < 2025, year > 2008) %>%
    select(country, iso3, g_whoregion, year, new_sp, new_sn, new_su, new_ep, new_oth, 
           newrel_m014, newrel_m15plus, newrel_mu,
           newrel_f014, newrel_f15plus, newrel_fu,
           newrel_sexunk014, newrel_sexunk15plus, newrel_sexunkageunk) %>%
    rowwise() %>% 
    mutate(
        oldCasesAll = ifelse(!is.na(new_sp) | !is.na(new_sn) | !is.na(new_su) | !is.na(new_ep) | !is.na(new_oth), 
                             sum(new_sp, new_sn, new_su, new_ep, new_oth, na.rm = TRUE),
                             NA), 
        # oldCases = ifelse(!is.na(new_sp) | !is.na(new_sn) | !is.na(new_su), 
        #                   sum(new_sp, new_sn, new_su, na.rm = TRUE),
        #                   NA), 
        newCases = ifelse(!is.na(newrel_m014) | !is.na(newrel_m15plus) | !is.na(newrel_mu) |
                              !is.na(newrel_f014) | !is.na(newrel_f15plus) | !is.na(newrel_fu) | 
                              !is.na(newrel_sexunk014) | !is.na(newrel_sexunk15plus) | !is.na(newrel_sexunkageunk),      
                          sum(newrel_m014, newrel_m15plus, newrel_mu,
                              newrel_f014, newrel_f15plus, newrel_fu,
                              newrel_sexunk014, newrel_sexunk15plus, newrel_sexunkageunk, na.rm = TRUE),
                          NA), 
        # notifSmall = ifelse(is.na(newCases), 
        #                oldCases, newCases),
        notif = ifelse(is.na(newCases), 
                       as.numeric(oldCasesAll), 
                       as.numeric(newCases))) %>% 
    select(iso3, g_whoregion, year, notif) %>%
    rename(country.code = iso3, 
           study.end.year = year, 
           who.region=g_whoregion) %>% 
    filter(country.code %in% allYearsCostAll$country.code) %>% ungroup()

# Check the notifications
# ggplot(data=notif %>% filter(country.code == "CHN")) +
#     geom_point(aes(x=study.end.year, y=notif, group = country.code)) +
#     theme_minimal() +
#     facet_wrap(facets=vars(country.code)) +
#     scale_y_log10()  + expand_limits(y=1) +
#     theme(legend.position = "bottom")

# ggplot(data=notif %>% filter(who.region %in% c("AMR", "EUR", "WPR"))) +
#     geom_point(aes(x=study.end.year, y=notif, group = country.code, color=who.region)) +
#     theme_minimal() +
#     facet_wrap(facets=vars(country.code)) +
#     scale_y_log10()  + expand_limits(y=1) +
#     theme(legend.position = "bottom")
# 
# ggplot(data=notif %>% filter(who.region %in% c("EMR", "EUR"))) +
#     geom_point(aes(x=study.end.year, y=notif, group = country.code, color=who.region)) +
#     theme_minimal() +
#     facet_wrap(facets=vars(country.code)) +
#     scale_y_log10()  + expand_limits(y=1)
# 
# ggplot(data=notif %>% filter(who.region %in% c("AMR", "WPR"))) + 
#     geom_point(aes(x=study.end.year, y=notif, group = country.code, color=who.region)) + 
#     theme_minimal() + 
#     facet_wrap(facets=vars(country.code))
# 
# ggplot(data=notif %>% filter(who.region %in% c("AFR"))) + 
#     geom_point(aes(x=study.end.year, y=notif, group = country.code, color=who.region)) + 
#     theme_minimal() + 
#     facet_wrap(facets=vars(country.code))
# 
# ggplot(data=notif %>% filter(who.region %in% c("SEA"))) + 
#     geom_point(aes(x=study.end.year, y=notif, group = country.code, color=who.region)) + 
#     theme_minimal() + 
#     facet_wrap(facets=vars(country.code))
# 
# ggplot(data=notif %>% filter(who.region %in% c("AMR", "WPR"))) +
#     geom_point(aes(x=study.end.year, y=notif, group = country.code), color="blue") +
#     geom_point(aes(x=study.end.year, y=notifSmall, group = country.code), color="green") +
#     theme_minimal() +
#     facet_wrap(facets=vars(country.code)) +
#     scale_y_log10()  + expand_limits(y=1)

### Join notification data to estimates and calculate weighted averages by
### iteration and year, then report the mean of these weighted averages 
### across iterations for each year. 
allYearsCostAll2a <- left_join(allYearsCostAll, notif)  %>% 
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>% 
    select(!who.region) %>% ungroup() %>% 
    # filter(country.code != "CHN") %>% 
    summarise(dme_prediag  = weighted.mean(dstb_dme_prediag_cost_country, notif, na.rm = TRUE),
              dnme_prediag = weighted.mean(dstb_dnme_prediag_cost_country, notif, na.rm = TRUE),
              ie_prediag   = weighted.mean(dstb_ie_prediag_cost_country, notif, na.rm = TRUE),
              dme_intsvtx  = weighted.mean(dstb_dme_intsvtx_cost_country, notif, na.rm = TRUE),
              dnme_intsvtx = weighted.mean(dstb_dnme_intsvtx_cost_country, notif, na.rm = TRUE),
              ie_intsvtx   = weighted.mean(dstb_ie_intsvtx_cost_country, notif, na.rm = TRUE),
              dme_contntx  = weighted.mean(dstb_dme_contntx_cost_country, notif, na.rm = TRUE),
              dnme_contntx = weighted.mean(dstb_dnme_contntx_cost_country, notif, na.rm = TRUE),
              ie_contntx   = weighted.mean(dstb_ie_contntx_cost_country, notif, na.rm = TRUE),
              total_total  = weighted.mean(dstb_total_cost_country, notif, na.rm = TRUE), 
              .by=c(`.draw`, study.end.year)) 

allYearsCostAll2 <- allYearsCostAll2a %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval))

colnames(allYearsCostAll2)[seq(2,30,3)] <- paste(colnames(allYearsCostAll2)[seq(2,30,3)], "est", sep = "_")
colnames(allYearsCostAll2)[seq(3,30,3)] <- gsub("\\.", "_", colnames(allYearsCostAll2)[seq(3,30,3)])
colnames(allYearsCostAll2)[seq(4,33,3)] <- gsub("\\.", "_", colnames(allYearsCostAll2)[seq(4,33,3)])

allYearsCostAll2b <- allYearsCostAll2 %>% 
    pivot_longer(dme_prediag_est:total_total_upper, values_to = "Cost", 
                 names_to = c("Category", "Treatment phase", "Type"), names_sep = "_") %>%
    mutate(Category = case_when(Category =="dme" ~ "Direct medical costs during", 
                                Category =="dnme" ~ "Direct non-medical costs during", 
                                Category =="ie" ~ "Indirect costs during", 
                                .default="Total cost"),
           `Treatment phase` = case_when(`Treatment phase`== "prediag" ~ "the pre-treatment phase", 
                                         `Treatment phase`=="intsvtx" ~ "the intensive treatment phase", 
                                         `Treatment phase`=="contntx" ~ "the continuation treatment phase", 
                                         .default="Total"), 
           `Treatment phase` = factor(`Treatment phase`, levels = c("the pre-treatment phase", 
                                                                    "the intensive treatment phase", "the continuation treatment phase", "total"))) %>% 
    pivot_wider(names_from = Type, values_from = Cost)

allYearsCostAll3a <- allYearsCostAll2a %>% mutate(dme = dme_prediag + dme_intsvtx + dme_contntx, 
                           dnme = dnme_prediag + dnme_intsvtx + dnme_contntx, 
                           ie = ie_prediag + ie_intsvtx + ie_contntx) 

allYearsCostAll3 <- allYearsCostAll3a %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year) %>% 
    mean_qi(dme, dnme, ie) %>% select(!c(.width, .point, .interval))

colnames(allYearsCostAll3)[seq(2,10,3)] <- paste(colnames(allYearsCostAll3)[seq(2,10,3)], "est", sep = ".")


allYearsCostAll3 <- allYearsCostAll3  %>%
                     pivot_longer(dme.est:ie.upper, values_to = "Cost", 
                                   names_to = c("Category", "Type"), names_sep = "\\.") %>%     
    mutate(Category = case_when(Category =="dme" ~ "Direct medical costs", 
                                Category =="dnme" ~ "Direct non-medical costs", 
                                Category =="ie" ~ "Indirect costs")) %>% 
    pivot_wider(names_from = Type, values_from = Cost)



allYearsCostAll4a <- allYearsCostAll2a %>% mutate(prediag = dme_prediag + dnme_prediag + ie_prediag, 
                                                 intsvtx = dme_intsvtx + dnme_intsvtx + ie_intsvtx, 
                                                 contntx = dme_contntx + dnme_contntx + ie_contntx) 
allYearsCostAll4 <- allYearsCostAll4a %>%
                    ungroup() %>% 
                    select(!`.draw`) %>% 
                    group_by(study.end.year) %>% 
                    mean_qi(prediag, intsvtx, contntx) %>% 
                    select(!c(.width, .point, .interval))  

colnames(allYearsCostAll4)[seq(2,10,3)] <- paste(colnames(allYearsCostAll4)[seq(2,10,3)], "est", sep = ".")


allYearsCostAll4 <- allYearsCostAll4 %>%     
    pivot_longer(prediag.est:contntx.upper, values_to = "Cost", 
                 names_to = c("Treatment phase", "Type"), names_sep = "\\.") %>% 
    mutate(`Treatment phase` = case_when(`Treatment phase`== "prediag" ~ "Pre-treatment phase", 
                                  `Treatment phase`=="intsvtx" ~ "Intensive treatment phase", 
                                  `Treatment phase`=="contntx" ~ "Continuation treatment phase", 
                                  .default="Total"), 
          `Treatment phase` = factor(`Treatment phase`, levels = c("Pre-treatment phase", 
                                                         "Intensive treatment phase", "Continuation treatment phase"))) %>% 
    pivot_wider(names_from = Type, values_from = Cost)


###### Change over time #######################################################
# left_join(allYearsCostAll, notif)  %>% 
#     filter(study.end.year %in% c(2009, 2024)) %>%
#     mutate(notif = ifelse(is.na(notif), 0, notif)) %>% 
#     select(!who.region) %>% ungroup() %>% 
#     # filter(country.code != "CHN") %>% 
#     summarise(total  = weighted.mean(dstb_dme_prediag_cost_country + 
#                                      dstb_dnme_prediag_cost_country + 
#                                      dstb_ie_prediag_cost_country, notif, na.rm = TRUE), 
#               .by=c(`.draw`, study.end.year)) %>%
#     pivot_wider(names_from=study.end.year, values_from = total, id_cols = .draw) %>% 
#     mutate(percChng = ((`2024` - `2009`)/ `2009`) /16* 100) %>% 
#     ungroup() %>% 
#     mean_qi(percChng) %>% select(!c(.width, .point, .interval))
###### Pre-diagnostic #######################################################
# left_join(allYearsCostAll, notif)  %>% 
#     filter(study.end.year %in% c(2009, 2023)) %>%
#     mutate(notif = ifelse(is.na(notif), 0, notif)) %>% 
#     select(!who.region) %>% ungroup() %>% 
#     # filter(country.code != "CHN") %>% 
#     summarise(total  = weighted.mean(dstb_total_cost_country, notif, na.rm = TRUE), 
#               .by=c(`.draw`, study.end.year)) %>%
#     pivot_wider(names_from=study.end.year, values_from = total, id_cols = .draw) %>% 
#     mutate(percChng = (`2023` - `2009`)/ `2009` /13* 100) %>% 
#     ungroup() %>% 
#     mean_qi(percChng) %>% select(!c(.width, .point, .interval))

plotEndYr <- 2009
df2 <- crossing(Category=unique(allYearsCostAll2b$Category)[-4],
                "Treatment phase" = as.character(unique(allYearsCostAll2b$`Treatment phase`)[-4]))
df2$value <- c(rep(1000,3), rep(1500,3), rep(1500,3))

df2$`Treatment phase` <- factor(df2$`Treatment phase`, levels = c("the pre-treatment phase", 
                                                                  "the intensive treatment phase", "the continuation treatment phase"))

timeCatPlot <- ggplot(data = allYearsCostAll2b %>% 
                          filter(Category != "Total cost", 
                                 study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
    geom_line(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper, fill = `Treatment phase`), alpha=.35) + 
    facet_wrap(facets = ~ Category + `Treatment phase`, #scales="free_y",
               labeller = function (labels) {
                   labels <- lapply(labels, as.character)
                   list(do.call(paste, c(labels, list(sep = "\n"))))
               }) +
    # geom_point(data = df2, aes(x = 2011, y = value), colour = NA) +
    
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 1500)) + 
    scale_y_continuous(breaks=c(0,250,500,750, 1000, 1250, 1500)) +
    scale_x_continuous(breaks=seq(plotEndYr,2024,3)) +
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing.x = unit(0.5, "lines"), 
          strip.text.y = element_text(margin = margin(t=0, b=0.5, 
                                                      l=0.5, r=0.5, unit="lines")), 
          legend.position = "bottom", legend.title = element_blank(), 
          panel.grid.minor.x = element_blank()) + 
    scale_color_discrete(type=c("#9999CC","#CC6666", "#66CC99")) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99")); timeCatPlot
# breaks=c('Pre-treatment phase', 'Intensive treatment phase', 'Continuation treatment phase'))

timeTotPlot <- ggplot(data = allYearsCostAll2b %>% filter(Category == "Total cost", 
                                                          study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est), color="#CC9966") + 
    geom_line(aes(x=study.end.year, y=est), color="#CC9966") + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), fill="#CC9966", alpha=.35) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 6000)) + 
    scale_x_continuous(breaks=seq(plotEndYr,2024,3)) +
    scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000)) + 
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.spacing = unit(0.5, "lines"), 
        panel.grid.minor.x = element_blank()) + 
    facet_wrap(facets = ~ Category); timeTotPlot

timePlot <- ggpubr::ggarrange(timeTotPlot, timeCatPlot, ncol=1, nrow=2, 
                              common.legend = FALSE, legend="none", heights=c(3,6))


ggsave(timePlot, file=here("output/figures/timeTrendPlot06-09.pdf"), width = 8.5, height=11, units = "in")

timeTotPlot <- ggplot(data = allYearsCostAll2b %>% filter(Category == "Total cost", 
                                                          study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est)) + 
    geom_line(aes(x=study.end.year, y=est)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), alpha=.4) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 6000)) + 
    scale_x_continuous(breaks=seq(2009,2024,3)) + 
    scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000, 6000)) + 
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.spacing = unit(0.5, "lines"), 
        panel.grid.minor.x = element_blank()) + 
    facet_wrap(facets = ~ Category)
ggsave(timeTotPlot, file=here("output/figures/timeTrendTotCostsPlot06-09.pdf"), 
       height = 8.5, width=11, units = "in")

###########################################################################
###### Marginal costs
###########################################################################
### Join notification data to estimates and calculate weighted averages by
### iteration and year, then report the mean of these weighted averages 
### across iterations for each year. 

timeCareCascadePlot <-
    ggplot(data = allYearsCostAll4 %>% 
                          filter(study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
    geom_line(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper, fill = `Treatment phase`), alpha=.35) + 
    # geom_point(data = df2, aes(x = 2011, y = value), colour = NA) +
    facet_wrap(facets = ~ `Treatment phase`, #scales="free_y",
               labeller = function (labels) {
                   labels <- lapply(labels, as.character)
                   list(do.call(paste, c(labels, list(sep = "\n"))))
               }) +
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 3500)) +
    # scale_y_continuous(breaks=c(0,250,500,750, 1000, 1250, 1500)) +
    scale_x_continuous(breaks=seq(plotEndYr,2024,3)) +
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing.x = unit(0.5, "lines"), 
          strip.text.y = element_text(margin = margin(t=0, b=0.5, 
                                                      l=0.5, r=0.5, unit="lines")), 
          legend.position = "none", legend.title = element_blank(), 
          panel.grid.minor.x = element_blank()) + 
    scale_color_discrete(type=c("#9999CC","#CC6666", "#66CC99")) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99")) ; timeCareCascadePlot

ggsave(timeCareCascadePlot, file=here("output/figures/timeTrendCareCascadePlot.pdf"), 
       height = 6, width=11, units = "in")

timeCostCatPlot <-
    ggplot(data = allYearsCostAll3 %>% 
               filter(study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est, color = Category)) + 
    geom_line(aes(x=study.end.year, y=est, color = Category)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper, fill = Category), alpha=.35) + 
    # geom_point(data = df2, aes(x = 2011, y = value), colour = NA) +
    facet_wrap(facets = ~ Category, #scales="free_y",
               labeller = function (labels) {
                   labels <- lapply(labels, as.character)
                   list(do.call(paste, c(labels, list(sep = "\n"))))
               }) +
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 300)) +
    # scale_y_continuous(breaks=c(0,250,500,750, 1000, 1250, 1500)) +
    scale_x_continuous(breaks=seq(plotEndYr,2024,3)) +
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing.x = unit(0.5, "lines"), 
          strip.text.y = element_text(margin = margin(t=0, b=0.5, 
                                                      l=0.5, r=0.5, unit="lines")), 
          legend.position = "none", legend.title = element_blank(), 
          panel.grid.minor.x = element_blank()) + 
    scale_color_discrete(type=c("#6699CC", "#CC6699", "#CC9966")) + 
    scale_fill_discrete(type=c("#6699CC", "#CC6699", "#CC9966")) ; timeCostCatPlot

ggsave(timeCostCatPlot, file=here("output/figures/timeTrendCostCatPlot.pdf"), 
       height = 6, width=11, units = "in")


timeMargPlot <- ggpubr::ggarrange(timeTotPlot, timeCareCascadePlot, timeCostCatPlot, ncol=1, nrow=3, 
                              common.legend = FALSE, legend="none", heights=c(3,3, 3))

ggsave(timeMargPlot, file=here("output/figures/timeTrendMarginStackPlot.pdf"), 
        height = 11, width=8.5, units = "in")

###########################################################################
###### What about with world region 
###########################################################################
### Join notification data to estimates and calculate weighted averages by
### iteration and year, then report the mean of these weighted averages 
### across iterations for each year. 
allYearsCostRegion0 <- left_join(allYearsCostAll, notif) %>%
    mutate(notif = ifelse(is.na(notif), 0, notif), 
           who.region = ifelse(study.country == "South Sudan", 
                               "AFR", who.region)) %>%
    group_by(`.draw`, study.end.year, who.region) %>% 
    summarise(dme_prediag  = weighted.mean(dstb_dme_prediag_cost_country, notif, na.rm = TRUE),
              dnme_prediag = weighted.mean(dstb_dnme_prediag_cost_country, notif, na.rm = TRUE),
              ie_prediag   = weighted.mean(dstb_ie_prediag_cost_country, notif, na.rm = TRUE),
              dme_intsvtx  = weighted.mean(dstb_dme_intsvtx_cost_country, notif, na.rm = TRUE),
              dnme_intsvtx = weighted.mean(dstb_dnme_intsvtx_cost_country, notif, na.rm = TRUE),
              ie_intsvtx   = weighted.mean(dstb_ie_intsvtx_cost_country, notif, na.rm = TRUE),
              dme_contntx  = weighted.mean(dstb_dme_contntx_cost_country, notif, na.rm = TRUE),
              dnme_contntx = weighted.mean(dstb_dnme_contntx_cost_country, notif, na.rm = TRUE),
              ie_contntx  = weighted.mean(dstb_ie_contntx_cost_country, notif, na.rm = TRUE),
              total_total  = weighted.mean(dstb_total_cost_country, notif, na.rm = TRUE)) 

allYearsCostRegion <- allYearsCostRegion0
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year, who.region) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval))

colnames(allYearsCostRegion)[seq(3,32,3)] <- paste(colnames(allYearsCostRegion)[seq(3,32,3)], "est", sep = "_")
colnames(allYearsCostRegion)[seq(4,32,3)] <- gsub("\\.", "_", colnames(allYearsCostRegion)[seq(4,32,3)])
colnames(allYearsCostRegion)[seq(5,32,3)] <- gsub("\\.", "_", colnames(allYearsCostRegion)[seq(5,32,3)])

allYearsCostRegion2 <- allYearsCostRegion %>% 
    pivot_longer(dme_prediag_est:total_total_upper, values_to = "Cost", 
                 names_to = c("Category", "Treatment phase", "Type"), names_sep = "_") %>%
    mutate(Category = case_when(Category =="dme" ~ "Direct medical costs during", 
                                Category =="dnme" ~ "Direct non-medical costs during", 
                                Category =="ie" ~ "Indirect costs during", 
                                .default="Total cost"),
           `Treatment phase` = case_when(`Treatment phase`== "prediag" ~ "the pre-treatment phase", 
                                         `Treatment phase`=="intsvtx" ~ "the intensive treatment phase", 
                                         `Treatment phase`=="contntx" ~ "the continuation treatment phase", 
                                         .default="Total"), 
           `Treatment phase` = factor(`Treatment phase`, levels = c("the pre-treatment phase", 
                                                                    "the intensive treatment phase", "the continuation treatment phase", "total"))) %>% 
    pivot_wider(names_from = Type, values_from = Cost)



timeRegionPlot <- ggplot(data = allYearsCostRegion2 %>% filter(Category == "Total cost", 
                                                               study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est)) + 
    geom_line(aes(x=study.end.year, y=est)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), alpha=.4) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 12000)) +
    scale_x_continuous(breaks=seq(plotEndYr,2024,3)) + 
    scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000)) + 
    xlab("Year") + ylab("Costs (2024 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing = unit(0.5, "lines"), 
          panel.grid.minor = element_blank()) + 
    facet_wrap(facets = ~ who.region);timeRegionPlot



ggsave(timeRegionPlot, file=here("output/figures/timeTrendRegionPlot.pdf"), 
       height = 8.5, width=11, units = "in")


allYearsCostAll3 <- left_join(allYearsCostAll, notif)  %>% 
    left_join(covariatesTime) %>%
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>% 
    select(!who.region) %>% ungroup() %>% 
    summarise(total  = weighted.mean(dstb_total_cost_country/median.income.trunc, 
                                     notif, na.rm = TRUE), 
              .by=c(`.draw`, study.end.year)) %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year) %>% 
    mean_qi(total) %>% select(!c(.width, .point, .interval))

ggplot(data = allYearsCostAll3) + 
    geom_point(aes(x=study.end.year, y=total)) + 
    geom_line(aes(x=study.end.year, y=total)) + 
    geom_ribbon(aes(x=study.end.year, ymin=.lower, ymax=.upper), alpha=.4) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 5)) +
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    xlab("Year") + ylab("Total household costs as a multiple of annual household median income") +
    theme(axis.text.x = element_text(angle = 45, hjust=1))

                    
allYearsCostAllRegion3 <- left_join(allYearsCostAll, notif)  %>% 
    left_join(covariatesTime) %>%
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>% 
    ungroup() %>% 
    summarise(total  = weighted.mean(dstb_total_cost_country/median.income.trunc, 
                                     notif, na.rm = TRUE), 
              .by=c(`.draw`, study.end.year, who.region)) %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year, who.region) %>% 
    mean_qi(total) %>% select(!c(.width, .point, .interval))

ggplot(data = allYearsCostAllRegion3 %>% filter(!is.na(who.region))) + 
    geom_point(aes(x=study.end.year, y=total)) + 
    geom_line(aes(x=study.end.year, y=total)) + 
    geom_ribbon(aes(x=study.end.year, ymin=.lower, ymax=.upper), alpha=.4) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 10)) +
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    xlab("Year") + 
    ylab("Total household costs as a multiple of annual household median income") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing = unit(0.5, "lines")) + 
    facet_wrap(facets = ~ who.region)


##############################################################################|
### Average annual percentage change in TB costs (Total)
##############################################################################|
### (Total)

allYearsCostAll2a %>%
        group_by(.draw) %>% 
        arrange(study.end.year, .by_group = TRUE) %>%
        mutate(pct_change = (total_total/lag(total_total) - 1) * 100) %>%
        na.omit() %>% 
        ungroup() %>%
        mean_qi(pct_change) %>% 
    select(! c(.width, .point, .interval)) %>% round(digits = 2) %>% t()

### (Cost category)
allYearsCostAll3a %>%
    group_by(.draw) %>% 
    arrange(study.end.year, .by_group = TRUE) %>%
    mutate(dme_pct_change = (dme/lag(dme) - 1) * 100,
           dnme_pct_change = (dnme/lag(dnme) - 1) * 100,
           ie_pct_change = (ie/lag(ie) - 1) * 100) %>%
    na.omit() %>% 
    ungroup() %>%
    mean_qi(dme_pct_change, dnme_pct_change, ie_pct_change) %>% 
    select(! c(.width, .point, .interval)) %>% round(digits = 2) %>% t()

### (Treatment phase)
allYearsCostAll4a %>%
    group_by(.draw) %>% 
    arrange(study.end.year, .by_group = TRUE) %>%
    mutate(prediag_pct_change = (prediag/lag(prediag) - 1) * 100,
           intsvtx_pct_change = (intsvtx/lag(intsvtx) - 1) * 100,
           contntx_pct_change = (contntx/lag(contntx) - 1) * 100) %>%
    na.omit() %>% 
    ungroup() %>%
    mean_qi(prediag_pct_change, intsvtx_pct_change, contntx_pct_change) %>% 
    select(! c(.width, .point, .interval)) %>% round(digits = 2) %>% t()

# timeScaledRegionPlot <- ggplot(data = allYearsCostAllRegion3 %>% filter(Category == "Total cost", 
#                                                                    study.end.year >= plotEndYr)) + 
#     geom_point(aes(x=study.end.year, y=est)) + 
#     geom_line(aes(x=study.end.year, y=est)) + 
#     geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), alpha=.4) + 
#     theme_minimal(base_size=14) + 
#     expand_limits(y=c(0, 15000)) + 
#     scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
#     scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000, 14000, 16000)) + 
#     xlab("Year") + ylab("Costs (2024 USD)") +
#     theme(axis.text.x = element_text(angle = 45, hjust=1),
#           panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
#           panel.spacing = unit(0.5, "lines")) + 
#     facet_wrap(facets = ~ who.region);timeScaledRegionPlot
    
    
    
# 
# allYearsCostChina <- allYearsCostAll %>% ungroup() %>% 
#                      filter(country.code == "CHN") %>%  
#                     select(!c(country.code, study.country)) %>% 
#                     group_by(study.end.year) %>% 
#                      mean_qi() %>% 
#                      select(!c(.width, .point, .interval)) 
# 
# colnames(allYearsCostChina)[2:31] <- gsub("dstb_", "", colnames(allYearsCostChina))[2:31]
# colnames(allYearsCostChina)[2:31] <- gsub("_cost_country", "", colnames(allYearsCostChina), fixed = TRUE)[2:31]
# colnames(allYearsCostChina)[seq(2,31,3)] <- paste(colnames(allYearsCostChina)[seq(2,31,3)], "est", sep = "_")
# colnames(allYearsCostChina)[seq(3,31,3)] <- gsub("\\.", "_", colnames(allYearsCostChina)[seq(3,31,3)])
# colnames(allYearsCostChina)[seq(4,31,3)] <- gsub("\\.", "_", colnames(allYearsCostChina)[seq(4,31,3)])
# colnames(allYearsCostChina) <- gsub("total_", "total_total_", colnames(allYearsCostChina))
# 
# 
# allYearsCostChina2 <- allYearsCostChina %>% 
#     pivot_longer(dme_prediag_est:total_total_upper, values_to = "Cost", 
#                  names_to = c("Category", "Treatment phase", "Type"), names_sep = "_") %>%
#     mutate(Category = case_when(Category =="dme" ~ "Direct medical costs during", 
#                                 Category =="dnme" ~ "Direct non-medical costs during", 
#                                 Category =="ie" ~ "Indirect costs during", 
#                                 .default="Total cost"),
#            `Treatment phase` = case_when(`Treatment phase`== "prediag" ~ "the pre-treatment phase", 
#                                          `Treatment phase`=="intsvtx" ~ "the intensive treatment phase", 
#                                          `Treatment phase`=="contntx" ~ "the continuation treatment phase", 
#                                          .default="Total"), 
#            `Treatment phase` = factor(`Treatment phase`, levels = c("the pre-treatment phase", 
#                                                                     "the intensive treatment phase", "the continuation treatment phase", "total"))) %>% 
#     pivot_wider(names_from = Type, values_from = Cost) %>% as.data.frame()
# 
# 
# timeCatChinaPlot <- ggplot(data = allYearsCostChina2 %>% 
#                           filter(Category != "Total cost", 
#                                  study.end.year >= plotEndYr)) + 
#     geom_point(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
#     geom_line(aes(x=study.end.year, y=est, color = `Treatment phase`)) + 
#     geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper, fill = `Treatment phase`), alpha=.35) + 
#     facet_wrap(facets = ~ Category + `Treatment phase`, #scales="free_y",
#                labeller = function (labels) {
#                    labels <- lapply(labels, as.character)
#                    list(do.call(paste, c(labels, list(sep = "\n"))))
#                }) +
#     # geom_point(data = df2, aes(x = 2011, y = value), colour = NA) +
#     
#     theme_minimal(base_size=14) + 
#     # expand_limits(y=c(0, 1500)) + 
#     # scale_y_continuous(breaks=c(0,250,500,750, 1000, 1250, 1500)) +
#     scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
#     xlab("Year") + ylab("Costs (2024 USD)") +
#     theme(axis.text.x = element_text(angle = 45, hjust=1),
#           panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
#           panel.spacing.x = unit(0.5, "lines"), 
#           strip.text.y = element_text(margin = margin(t=0, b=0.5, 
#                                                       l=0.5, r=0.5, unit="lines")), 
#           legend.position = "bottom", legend.title = element_blank()) + 
#     scale_color_discrete(type=c("#9999CC","#CC6666", "#66CC99")) + 
#     scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99")); timeCatChinaPlot
# 
# 
# chinaPlot <- ggplot(covariatesTime %>% filter(country.code == "CHN")) + 
#     geom_point(aes(x=study.end.year, y=median.income.trunc)) + 
#     geom_line(aes(x=study.end.year, y=median.income.trunc)) + 
#     
#     geom_point(aes(x=study.end.year, y=median.income), color = "violet") + 
#     geom_line(aes(x=study.end.year, y=median.income), color = "violet") + 
#     theme_minimal()
# 
# 
# ggsave(timeCatChinaPlot, file=here("output/figures/timeTrend-China-06-01.pdf"), 
#        height = 8.5, width=11, units = "in")


rateMedIncome <- left_join(covariates, 
                           spread_draws(lmicSamps, 
                                        dstb_dme_prediag_cost_country[country.id.new],
                                        dstb_dnme_prediag_cost_country[country.id.new],
                                        dstb_ie_prediag_cost_country[country.id.new],
                                        dstb_dme_intsvtx_cost_country[country.id.new],
                                        dstb_dnme_intsvtx_cost_country[country.id.new],
                                        dstb_ie_intsvtx_cost_country[country.id.new],
                                        dstb_dme_contntx_cost_country[country.id.new],
                                        dstb_dnme_contntx_cost_country[country.id.new],
                                        dstb_ie_contntx_cost_country[country.id.new],
                                        dstb_total_cost_country[country.id.new], 
                                        ndraws = 1000, seed = tbPtntCstSeed)) %>% 
    mutate(ratioMedInc = dstb_total_cost_country / median.income) %>% 
    group_by(study.country) %>% mean_qi(ratioMedInc) 



round(1-(dim(rateMedIncome %>% filter(ratioMedInc<1))[1] / dim(rateMedIncome)[1]), 2)*100


rateMedIncome %>% filter(ratioMedInc<1)

round(quantile(rateMedIncome$ratioMedInc, seq(0,1,.05)), 2)


rateMedIncome <- left_join(covariates, 
                           spread_draws(lmicSamps, 
                                        dstb_dme_prediag_cost_country[country.id.new],
                                        dstb_dnme_prediag_cost_country[country.id.new],
                                        dstb_ie_prediag_cost_country[country.id.new],
                                        dstb_dme_intsvtx_cost_country[country.id.new],
                                        dstb_dnme_intsvtx_cost_country[country.id.new],
                                        dstb_ie_intsvtx_cost_country[country.id.new],
                                        dstb_dme_contntx_cost_country[country.id.new],
                                        dstb_dnme_contntx_cost_country[country.id.new],
                                        dstb_ie_contntx_cost_country[country.id.new],
                                        dstb_total_cost_country[country.id.new], 
                                        ndraws = 1000, seed = tbPtntCstSeed)) %>% 
    mutate(perc = dstb_total_cost_country) %>% 
    group_by(study.country) %>% mean_qi(ratioMedInc) 