
##############################################################################|
#### Load in packages #### 
##############################################################################|
library(reshape2)
library(geosphere)
library(forcats)
library(scale)

incRate <- read.csv("~/Downloads/TB_burden_countries_2026-04-13.csv") %>% 
            filter(year == 2024) %>% 
            select(country, iso3, e_inc_100k) %>% 
            rename(study.country = country, 
                   country.code = iso3, 
                   inc100K = e_inc_100k)

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

ggplot(nCountry) +
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

### Panel B - Surveys over time, country, and  ???
nCountryYear <- modelData %>% select(study.country, study.end.year, 
                                     whohb, covidence.id) %>% 
    distinct() %>% group_by(study.end.year) %>% count(study.country)


ggplot(nCountryYear, aes(x = study.end.year, y=study.country)) + 
    geom_point(aes(size=n)) + theme_minimal() +
    scale_x_continuous(breaks = seq(2008, 2025, 1)) +
    theme(legend.position="bottom") +
    labs(y = "Number of prevalence surveys", x = "Publication year") +
    # geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("Number of prevalence surveys by publication year")

    
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
write.csv(here("output/tables/covariateCoefs.csv"))


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
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of total out-of-pocket costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Diagnostic', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom"); countryCareCascadePlot

ggsave(plot = countryCareCascadePlot, 
       filename = here("output/figures/countryCareCascadePlot.pdf"), width = 8.5, height=11, units = "in")

##############################################################################|
##### Figure X                                       ##########################
##### Costs by cost type (DME, DNME, IE)             ##########################
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
    rename(`Direct medical` = dstb_dme_prediag_cost_country, 
           `Direct non-medical` = dstb_dnme_prediag_cost_country,
           `Indirect` = dstb_ie_prediag_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryPrediagPlot <- ggplot() + 
    geom_bar(data = preDiagEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of diagnostic costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#448866", "#66CC99", "#99ddbb"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom"); countryPrediagPlot

ggsave(plot = countryPrediagPlot, 
       filename = here("output/figures/countryPrediagPlot.pdf"), width = 8.5, height=11, units = "in")

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
    rename(`Direct medical` = dstb_dme_intsvtx_cost_country, 
           `Direct non-medical` = dstb_dnme_intsvtx_cost_country,
           `Indirect` = dstb_ie_intsvtx_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryIntsvTxPlot <- ggplot() +
    geom_bar(data = IntsvTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of intensive treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#884444", "#CC6666", "#dd9999"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom"); countryIntsvTxPlot

ggsave(plot = countryIntsvTxPlot, 
       filename = here("output/figures/countryIntsvTxPlot.pdf"), width = 8.5, height=11, units = "in")

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
    rename(`Direct medical` = dstb_dme_contntx_cost_country, 
           `Direct non-medical` = dstb_dnme_contntx_cost_country,
           `Indirect` = dstb_ie_contntx_cost_country) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category")

countryContnTxPlot <- ggplot() +
    geom_bar(data = ContnTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of continuation treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#725f7d", "#9999CC", "#e2ccdf"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom"); countryContnTxPlot

ggsave(plot = countryContnTxPlot, 
       filename = here("output/figures/countryContnTxPlot.pdf"), width = 8.5, height=11, units = "in")

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
                  labels = label_dollar(scale_cut = cut_short_scale())) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  labels = label_dollar(scale_cut = cut_short_scale())) +
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
                  labels = label_dollar(scale_cut = cut_short_scale())) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  labels = label_dollar(scale_cut = cut_short_scale())) +
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
                  labels = label_dollar(scale_cut = cut_short_scale())) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  labels = label_dollar(scale_cut = cut_short_scale())) +
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
                  labels = label_dollar(scale_cut = cut_short_scale())) +
    scale_y_log10(breaks = 500 * 2^seq(0, 9, by = 1),
                  labels = label_dollar(scale_cut = cut_short_scale())) +
    # scale_color_gradient(low="darkgreen", high="green1") +  
    guides(color = guide_colourbar(barwidth = 30)) + 
    ggtitle("Country-level household costs due to tuberculosis disease")




