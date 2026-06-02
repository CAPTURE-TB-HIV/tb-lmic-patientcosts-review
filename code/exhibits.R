
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
library(forcats)

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
covariatesTime <- readRDS(here("output/covariatesTime.rds"))

### Load in the samples 
lmicSamps <- readRDS(here("output/goodModelRun-06-01.rds"))
lmicSampsTime <- readRDS(here("output/timeSamps_06-01.rds"))

### Create a vector of the Top 30 TB burden countries 
top30 <- c("Brazil", "Central African Republic", "Congo", "Ethiopia", 
           "Gabon", "Kenya", "Lesotho", "Liberia", "Namibia", "Thailand", 
           "Uganda", "United Republic of Tanzania", "China", "India", 
           "Indonesia", "Mozambique", "Myanmar", "Nigeria", "Philippines", 
           "South Africa", "Zambia", "Angola", "Bangladesh", "DPR Korea", 
           "Mongolia", "Pakistan", "Papua New Guinea", "Viet Nam", 
           "Sierra Leone", "Democratic Republic of the Congo")

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
                            median_qi(dstb_dme_prediag_cost_country, 
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
    select(study.country, in.review, 
           median.income, 
           median.income.trunc,
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
    mutate(cntry_order = 1:n(),
           in.review = ifelse(in.review == 0, "No", "Yes")) 

countryEst <- left_join(countryEst, regionWHO) %>% 
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

countryEst <- left_join(countryEst, lmic)  %>% 
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
                                  "Republic of Moldova") ~ "UM", 
                                .default = class25))

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
               aes(x = study.end.year, y=study.country, color=Rep, size=repCount)) + 
    theme_minimal(base_size = 16) +
    scale_x_continuous(breaks = seq(2008, 2025, 1)) +
    theme(
        legend.position="inside", legend.position.inside=c(.14, .2), 
        legend.background = element_rect(fill="white"),
        legend.box = "vertical") +
    labs(y = "", x = "Study end year", size="Number of surveys", 
         caption = "Number label represents total surveys for each country and year.") +
    scale_color_manual(values = c("dodgerblue2", "darkred"), name=NULL) + 
    scale_size_continuous(range = c(2,6)) +
    # geom_text(data=nCountryYear2, 
    #           aes(x=study.end.year, y=reorder(study.country, totalCount), label=yearCount),
    #           position = position_dodge(0.9),
    #           colour = "white", size=3) #+
    geom_text(data=nCountryYear2, 
              aes(x=study.end.year, y=reorder(study.country, totalCount), label=yearCount),
              hjust = 0, nudge_x = 0.2,# nudge_y=.2,
              colour = "black", size=3) #+
# ggtitle("Number of prevalence surveys by publication year")#; plot2b

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

write.csv(allIncludedSurveys, 
          file=here("output/supplement/allStudiesQualitative.csv"))

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

ggplot(data=countryEst) + 
    geom_point(aes(x=median.income.trunc, y=dstb_total_cost_country)) + 
    theme_minimal(base_size=18) + expand_limits(y=0) +
    ggtitle("non-RR/MDR TB household costs by median income") + 
    ylab("Total household costs (2024 USD)")  + xlab("Median income") 

ggplot(data=countryEst) + 
    geom_point(aes(x=oop.perc.che.trunc, y=dstb_total_cost_country)) + 
    theme_minimal(base_size=18) + expand_limits(y=0) +
    ggtitle("non-RR/MDR TB household costs by OOP % CHE") +
    ylab("Total household costs (2024 USD)") + xlab("OOP % CHE") 

ggplot(data=countryEst) + 
    geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country)) + 
    theme_minimal(base_size=18) + expand_limits(y=0) +
    ggtitle("non-RR/MDR TB household costs by Universal Health Care Index") +
    ylab("Total household costs (2024 USD)")  + xlab("UHC index")

ggplot(data=countryEst) + 
    geom_point(aes(x=uhc.index.trunc, y=dstb_total_cost_country, color=median.income.trunc)) + 
    theme_minimal(base_size=18) + expand_limits(y=0) +
    ggtitle("non-RR/MDR TB household costs by Universal Health Care Index") + 
    ylab("Total household costs (2024 USD)")  + xlab("UHC index")  +
    theme(legend.position = "bottom") + 
    labs(color="Median income")

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
    median_qi() %>% 
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
        median_qi() %>% 
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
        median_qi() %>% 
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
        median_qi() %>% 
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
        median_qi() %>% 
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
        median_qi() %>% 
        select(! c(.width, .point, .interval)) %>% 
        exp() %>%
        round(digits = 2) %>% 
        matrix(nrow=9, byrow = TRUE) 
) %>% 
    ##### Save as a CSV  ###########################################|
    write.csv(here("output/tables/covariateCoefsNew.csv"))


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
    rename(Diagnostic = preDiagTotal, 
           `Intensive treatment` = intsvTxTotal,
           `Continuation treatment` = contnTxTotal) %>%
    melt(id.vars = c("study.country","cntry_order"), 
         value.name = "Cost", 
         variable.name = "Category") %>%
    transform( order = factor(
        Category,
        levels=c( 'Diagnostic','Intensive treatment',
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
                        breaks=c('Diagnostic', 'Intensive treatment', 'Continuation treatment')) +
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
    theme_minimal(base_size = 9) + 
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Diagnostic', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom",
          plot.margin = margin(t=0, b=0, l=0, r=.3, "cm"),
          legend.box.spacing = unit(.1, "lines"),); top30CountryCareCascadePlot

ggsave(plot = top30CountryCareCascadePlot, 
       filename = here("output/figures/top30CountryCareCascadePlot.pdf"), width = 8.5, height=8.5, units = "in")

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

ggsave(plot = countryCareCascadePlot, 
       filename = here("output/figures/countryCareCascadePlot.pdf"), width = 8.5, height=11, units = "in")


##### Recreate this plot for only the top 30 TB countries 
top30countryCostCategoryPlot <- ggplot() + 
    geom_bar(data = categoryEstimates %>% filter(study.country %in% top30)%>% 
                 mutate(study.country = case_when(grepl("tic Rep", study.country) ~ "DR Congo",
                                                  grepl("Tanzania", study.country) ~ "Tanzania", 
                                                  .default = study.country)),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 9) + 
    coord_flip() + ylab("Fraction of total costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_x_discrete(labels = label_wrap(16)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_discrete(type=c(c("#6699CC", "#CC6699", "#CC9966")), 
                        breaks=c('Direct\nmedical', 'Direct\nnon-medical', 'Indirect')) +
    theme(legend.position = "bottom",
          plot.margin = margin(t=0, b=0, l=0, r=.3, "cm"),
          legend.box.spacing = unit(.1, "lines")); top30countryCostCategoryPlot

ggsave(plot = top30CountryCareCascadePlot, 
       filename = here("output/figures/top30CountryCareCascadePlot.pdf"), width = 8.5, height=8.5, units = "in")

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
                                   median_qi(dstb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    select(country.code, in.review, dstb_total_cost_country, median.income) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"), 
           dstb_total_medInc = dstb_total_cost_country/median.income)

lmicDRTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            drtb_total_cost_country[country.id.new], 
                                            ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   median_qi(drtb_total_cost_country), 
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
                        na.value = "grey85", name = "Total household cost of TB") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    
    ggtitle("non-RR/MDR-TB care costs"); dstbCarePlot


ggsave(plot = dstbCarePlot, filename = here("output/figures/dstbCostMap.pdf"), width =11, height = 8.5)

##### Scale non-RR/MDRTB costs by median income        ########################
dstbCareScldPlot <- ggplot() +
    geom_sf(data = world_dstb, fill="grey85") + 
    geom_sf(data = world_dstb, mapping=aes(geometry=geometry, fill=dstb_total_medInc)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", 
                        name = "Ratio of TB care cost to median income") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    # guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    
    ggtitle("non-RR/MDR-TB care costs"); dstbCareScldPlot

ggsave(plot = dstbCareScldPlot, filename = here("output/figures/dstbCostMapMedInc.pdf"), width =11, height = 8.5)

drtbCarePlot <- ggplot() +
    geom_sf(data = world_drtb, fill="grey85") + 
    geom_sf(data = world_drtb, mapping=aes(geometry=geometry, fill=drtb_total_cost_country)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Total household cost of TB") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    ggtitle("RR/MDR-TB care costs"); drtbCarePlot

ggsave(plot = drtbCarePlot, filename = here("output/figures/drtbCostMap.pdf"), width =11, height = 8.5)

##### Scale RR/MDRTB costs by median income        ############################
drtbCareScldPlot <- ggplot() +
    geom_sf(data = world_drtb, fill="grey85") + 
    geom_sf(data = world_drtb, mapping=aes(geometry=geometry, fill=drtb_total_medInc)) + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Ratio of TB care costs to median income") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    ggtitle("RR/MDR-TB care costs"); drtbCareScldPlot

ggsave(plot = drtbCareScldPlot, filename = here("output/figures/drtbCostMapMedInc.pdf"), width =11, height = 8.5)

##############################################################################|
##### Table SX                                      ###########################
##### Total median DSTB with uncertainty intervals  ###########################
##############################################################################|

summaryEstUcrntyTbl <- countryEst %>% 
    select(study.country, who.region, 
           contains("dstb")) %>%
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
    group_by(name) %>% 
    median_qi(value) %>% 
    mutate(across(value:.upper, round), 
           formatted = paste0(value, " (", 
                              .lower, ", ", 
                              .upper, ")")) 

##############################################################################|
##### Table SX                                      ###########################
##### Regional median DSTB with uncertainty intervals  ########################
##############################################################################|

regionEstUcrntyTbl <- countryEst %>% 
    select(study.country, who.region, 
           contains("dstb")) %>%
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
    group_by(name, who.region) %>% 
    median_qi(value) %>% 
    mutate(across(value:.upper, round), 
           formatted = paste0(value, " (", 
                              .lower, ", ", 
                              .upper, ")")) %>% 
    select(!c(value, .lower, .upper)) %>%
    pivot_wider(names_from = name, values_from = formatted) %>% 
    write.csv(here("output/tables/regionMedianCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### TB Inc Group median DSTB with uncertainty intervals  ####################
##############################################################################|
tbIncQuant <- left_join(LMIC, incRate %>% select(!study.country)) %>% 
    do(data.frame(t(quantile(.$inc100K, na.rm=TRUE)))) %>% unlist() %>%
    as.numeric()


tbIncEstUcrntyTbl <- left_join(countryEst, incRate) %>% 
    mutate(inc100K = ifelse(study.country == "Cote d'Ivoire",
                            99.00, inc100K), 
           incQuant = case_when(inc100K < tbIncQuant[2] ~ 1,
                                inc100K < tbIncQuant[3] ~ 2,
                                inc100K < tbIncQuant[4] ~ 3,
                                inc100K < tbIncQuant[5] ~ 4)) %>%
    select(incQuant, 
           contains("dstb")) %>%
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
    group_by(name, incQuant) %>% 
    median_qi(value) %>% 
    mutate(across(value:.upper, round), 
           formatted = paste0(value, " (", 
                              .lower, ", ", 
                              .upper, ")")) %>% 
    select(!c(value, .lower, .upper)) %>%
    pivot_wider(names_from = name, values_from = formatted) %>% 
    write.csv(here("output/tables/tbIncMedianCosts.csv"))

##############################################################################|
##### Table SX                                      ###########################
##### Top 30 TB median DSTB with uncertainty intervals     ####################
##############################################################################|

top30EstUcrntyTbl <- 
    countryEst %>% 
    filter(study.country %in% top30) %>%
    select(contains("dstb")) %>%
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
    group_by(name) %>% 
    median_qi(value) %>% 
    mutate(across(value:.upper, round), 
           formatted = paste0(value, " (", 
                              .lower, ", ", 
                              .upper, ")")) %>% 
    select(!c(value, .lower, .upper)) %>%
    write.csv(here("output/tables/top30MedianCosts.csv"))

##############################################################################|
##### Table SX                                                    #############
##### World Bank Group median DSTB with uncertainty intervals     #############
##############################################################################|

worldBankEstUcrntyTbl <- 
    countryEst %>% 
    select(contains("dstb"), study.country, class25) %>%
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country) %>% 
    group_by(name, class25) %>% 
    median_qi(value) %>% 
    mutate(across(value:.upper, round), 
           formatted = paste0(value, " (", 
                              .lower, ", ", 
                              .upper, ")")) %>% 
    select(!c(value, .lower, .upper)) %>%
    pivot_wider(names_from = name, values_from = formatted) %>% 
    write.csv(here("output/tables/WorldBankMedianCosts.csv"))

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
                                     median_qi(dstb_dme_prediag_cost_country, 
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

annualPC <- spread_draws(totalModelTimeOnly, 
                         
                         mult_year_dme_prediag,
                         mult_year_dnme_prediag,
                         mult_year_ie_prediag,
                         mult_year_dme_intsvtx,
                         mult_year_dnme_intsvtx,
                         mult_year_ie_intsvtx,
                         mult_year_dme_contntx,
                         mult_year_dnme_contntx,
                         mult_year_ie_contntx, 
                         
                         mult_yrdr_intx_dme_prediag,
                         mult_yrdr_intx_dnme_prediag,
                         mult_yrdr_intx_ie_prediag,
                         mult_yrdr_intx_dme_intsvtx,
                         mult_yrdr_intx_dnme_intsvtx,
                         mult_yrdr_intx_ie_intsvtx,
                         mult_yrdr_intx_dme_contntx,
                         mult_yrdr_intx_dnme_contntx,
                         mult_yrdr_intx_ie_contntx,
                         ndraws = 1000, seed = tbPtntCstSeed) %>%
    mutate(dstb_yr_dme_prediag = exp(mult_year_dme_prediag / sd(modelData$study.end.year)) - 1, 
           dstb_yr_dnme_prediag = exp(mult_year_dnme_prediag / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_prediag = exp(mult_year_ie_prediag / sd(modelData$study.end.year)) -1, 
           dstb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           dstb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx / sd(modelData$study.end.year)) -1,  
           dstb_yr_dme_contntx = exp(mult_year_dme_contntx / sd(modelData$study.end.year)) -1, 
           dstb_yr_dnme_contntx = exp(mult_year_dnme_contntx / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_contntx = exp(mult_year_ie_contntx / sd(modelData$study.end.year)) -1,
           drtb_yr_dme_prediag = exp(mult_year_dme_prediag + mult_yrdr_intx_dme_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_prediag = exp(mult_year_dnme_prediag + mult_yrdr_intx_dnme_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_prediag = exp(mult_year_ie_prediag + mult_yrdr_intx_ie_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1,  
           drtb_yr_dme_contntx = exp(mult_year_dme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_contntx = exp(mult_year_dnme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_contntx = exp(mult_year_ie_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year) - 1)) %>%
    select(! contains("mult")) %>% 
    pivot_longer(dstb_yr_dme_prediag:drtb_yr_ie_contntx, names_to = "Parameter", values_to = "Value") %>%
    group_by(Parameter) %>% 
    median_qi() %>% 
    select(! c(.width, .point, .interval)) %>%
    mutate(across(Value:.upper, \(x) round(x, digits = 2)*100), 
           formatted = paste0(Value, " (", 
                              .lower, ", ", 
                              .upper, ")")) 

write.csv(annualPC, file=here("output/tables/annualPercChngTimeOnly.csv"))

annualPC_aCovs <- spread_draws(totalModelTimeaCovs, 
                               
                               mult_year_dme_prediag,
                               mult_year_dnme_prediag,
                               mult_year_ie_prediag,
                               mult_year_dme_intsvtx,
                               mult_year_dnme_intsvtx,
                               mult_year_ie_intsvtx,
                               mult_year_dme_contntx,
                               mult_year_dnme_contntx,
                               mult_year_ie_contntx, 
                               
                               mult_yrdr_intx_dme_prediag,
                               mult_yrdr_intx_dnme_prediag,
                               mult_yrdr_intx_ie_prediag,
                               mult_yrdr_intx_dme_intsvtx,
                               mult_yrdr_intx_dnme_intsvtx,
                               mult_yrdr_intx_ie_intsvtx,
                               mult_yrdr_intx_dme_contntx,
                               mult_yrdr_intx_dnme_contntx,
                               mult_yrdr_intx_ie_contntx,
                               ndraws = 1000, seed = tbPtntCstSeed) %>%
    mutate(dstb_yr_dme_prediag = exp(mult_year_dme_prediag / sd(modelData$study.end.year)) - 1, 
           dstb_yr_dnme_prediag = exp(mult_year_dnme_prediag / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_prediag = exp(mult_year_ie_prediag / sd(modelData$study.end.year)) -1, 
           dstb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           dstb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx / sd(modelData$study.end.year)) -1,  
           dstb_yr_dme_contntx = exp(mult_year_dme_contntx / sd(modelData$study.end.year)) -1, 
           dstb_yr_dnme_contntx = exp(mult_year_dnme_contntx / sd(modelData$study.end.year)) -1, 
           dstb_yr_ie_contntx = exp(mult_year_ie_contntx / sd(modelData$study.end.year)) -1,
           drtb_yr_dme_prediag = exp(mult_year_dme_prediag + mult_yrdr_intx_dme_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_prediag = exp(mult_year_dnme_prediag + mult_yrdr_intx_dnme_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_prediag = exp(mult_year_ie_prediag + mult_yrdr_intx_ie_prediag / sd(modelData$study.end.year)) -1, 
           drtb_yr_dme_intsvtx = exp(mult_year_dme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_intsvtx = exp(mult_year_dnme_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_intsvtx = exp(mult_year_ie_intsvtx + mult_yrdr_intx_dme_intsvtx / sd(modelData$study.end.year)) -1,  
           drtb_yr_dme_contntx = exp(mult_year_dme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
           drtb_yr_dnme_contntx = exp(mult_year_dnme_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year)) -1, 
           drtb_yr_ie_contntx = exp(mult_year_ie_contntx + mult_yrdr_intx_dme_contntx / sd(modelData$study.end.year) - 1)) %>%
    select(! contains("mult")) %>% 
    pivot_longer(dstb_yr_dme_prediag:drtb_yr_ie_contntx, names_to = "Parameter", values_to = "Value") %>%
    group_by(Parameter) %>% 
    median_qi() %>% 
    select(! c(.width, .point, .interval)) %>%
    mutate(across(Value:.upper, \(x) round(x, digits = 2)*100), 
           formatted = paste0(Value, " (", 
                              .lower, ", ", 
                              .upper, ")")) 

write.csv(annualPC_aCovs, file=here("output/tables/annualPercChngAllCovs.csv"))

##############################################################################|
#### Time trend estimates ####
##############################################################################|
##### Time plots from running the model for all countries and years
##############################################################################|
allYearsCostAll <- data.frame()
for (yr in 2009:2023){  
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
#        median_qi(dstb_total_cost_country) %>% 
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
    filter(year < 2024, year > 2008) %>%
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
### iteration and year, then report the median of these weighted averages 
### across iterations for each year. 
allYearsCostAll2 <- left_join(allYearsCostAll, notif)  %>% 
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
              .by=c(`.draw`, study.end.year)) %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year) %>% 
    median_qi() %>% select(!c(.width, .point, .interval))

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

plotEndYr <- 2011
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
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    xlab("Year") + ylab("Costs (2023 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing.x = unit(0.5, "lines"), 
          strip.text.y = element_text(margin = margin(t=0, b=0.5, 
                                                      l=0.5, r=0.5, unit="lines")), 
          legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_discrete(type=c("#9999CC","#CC6666", "#66CC99")) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99")); timeCatPlot
# breaks=c('Pre-treatment phase', 'Intensive treatment phase', 'Continuation treatment phase'))

timeTotPlot <- ggplot(data = allYearsCostAll2b %>% filter(Category == "Total cost", 
                                                          study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est), color="#CC9966") + 
    geom_line(aes(x=study.end.year, y=est), color="#CC9966") + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), fill="#CC9966", alpha=.35) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 5000)) + 
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000)) + 
    xlab("Year") + ylab("Costs (2023 USD)") +
    theme(
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.spacing = unit(0.5, "lines")) + 
    facet_wrap(facets = ~ Category)

timePlot <- ggpubr::ggarrange(timeTotPlot, timeCatPlot, ncol=1, nrow=2, 
                              common.legend = FALSE, legend="none", heights=c(3,6))


ggsave(timePlot, file=here("output/figures/timeTrendPlot-06-01.pdf"), width = 8.5, height=11, units = "in")

timeTotPlot <- ggplot(data = allYearsCostAll2b %>% filter(Category == "Total cost", 
                                                          study.end.year >= plotEndYr)) + 
    geom_point(aes(x=study.end.year, y=est)) + 
    geom_line(aes(x=study.end.year, y=est)) + 
    geom_ribbon(aes(x=study.end.year, ymin=lower, ymax=upper), alpha=.4) + 
    theme_minimal(base_size=14) + 
    expand_limits(y=c(0, 5000)) + 
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000)) + 
    xlab("Year") + ylab("Costs (2023 USD)") +
    theme(
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.spacing = unit(0.5, "lines")) + 
    facet_wrap(facets = ~ Category)
ggsave(timeTotPlot, file=here("output/figures/timeTrendTotCostsPlot-06-01.pdf"), 
       height = 8.5, width=11, units = "in")

###########################################################################
###### What about with world region 
###########################################################################
### Join notification data to estimates and calculate weighted averages by
### iteration and year, then report the median of these weighted averages 
### across iterations for each year. 
allYearsCostRegion <- left_join(allYearsCostAll, notif) %>%
    mutate(notif = ifelse(is.na(notif), 0, notif)) %>%
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
              total_total  = weighted.mean(dstb_total_cost_country, notif, na.rm = TRUE)) %>%
    ungroup() %>% 
    select(!`.draw`) %>% 
    group_by(study.end.year, who.region) %>% 
    median_qi() %>% select(!c(.width, .point, .interval))

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
    expand_limits(y=c(0, 15000)) + 
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000, 14000, 16000)) + 
    xlab("Year") + ylab("Costs (2023 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing = unit(0.5, "lines")) + 
    facet_wrap(facets = ~ who.region);timeRegionPlot



ggsave(timeRegionPlot, file=here("output/figures/timeTrendRegionPlot-06-01.pdf"), 
       height = 8.5, width=11, units = "in")


allYearsCostChina <- allYearsCostAll %>% ungroup() %>% 
                     filter(country.code == "CHN") %>%  
                    select(!c(country.code, study.country)) %>% 
                    group_by(study.end.year) %>% 
                     median_qi() %>% 
                     select(!c(.width, .point, .interval)) 

colnames(allYearsCostChina)[2:31] <- gsub("dstb_", "", colnames(allYearsCostChina))[2:31]
colnames(allYearsCostChina)[2:31] <- gsub("_cost_country", "", colnames(allYearsCostChina), fixed = TRUE)[2:31]
colnames(allYearsCostChina)[seq(2,31,3)] <- paste(colnames(allYearsCostChina)[seq(2,31,3)], "est", sep = "_")
colnames(allYearsCostChina)[seq(3,31,3)] <- gsub("\\.", "_", colnames(allYearsCostChina)[seq(3,31,3)])
colnames(allYearsCostChina)[seq(4,31,3)] <- gsub("\\.", "_", colnames(allYearsCostChina)[seq(4,31,3)])
colnames(allYearsCostChina) <- gsub("total_", "total_total_", colnames(allYearsCostChina))


allYearsCostChina2 <- allYearsCostChina %>% 
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
    pivot_wider(names_from = Type, values_from = Cost) %>% as.data.frame()


timeCatChinaPlot <- ggplot(data = allYearsCostChina2 %>% 
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
    # expand_limits(y=c(0, 1500)) + 
    # scale_y_continuous(breaks=c(0,250,500,750, 1000, 1250, 1500)) +
    scale_x_continuous(breaks=seq(plotEndYr,2023,2)) + 
    xlab("Year") + ylab("Costs (2023 USD)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.spacing.x = unit(0.5, "lines"), 
          strip.text.y = element_text(margin = margin(t=0, b=0.5, 
                                                      l=0.5, r=0.5, unit="lines")), 
          legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_discrete(type=c("#9999CC","#CC6666", "#66CC99")) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99")); timeCatChinaPlot


chinaPlot <- ggplot(covariatesTime %>% filter(country.code == "CHN")) + 
    geom_point(aes(x=study.end.year, y=median.income.trunc)) + 
    geom_line(aes(x=study.end.year, y=median.income.trunc)) + 
    
    geom_point(aes(x=study.end.year, y=median.income), color = "violet") + 
    geom_line(aes(x=study.end.year, y=median.income), color = "violet") + 
    theme_minimal()


ggsave(timeCatChinaPlot, file=here("output/figures/timeTrend-China-06-01.pdf"), 
       height = 8.5, width=11, units = "in")
