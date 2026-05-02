#### Load in packages #### 
library(reshape2)
library(geosphere)

countryEstimates <- left_join(covariates, 
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
    select(study.country, in.review, 
           dstb_dme_prediag_cost_country, 
           dstb_dnme_prediag_cost_country, 
           dstb_ie_prediag_cost_country,
           dstb_dme_intsvtx_cost_country, 
           dstb_dnme_intsvtx_cost_country, 
           dstb_ie_intsvtx_cost_country,
           dstb_dme_contntx_cost_country, 
           dstb_dnme_contntx_cost_country, 
           dstb_ie_contntx_cost_country,
           dstb_total_cost_country) %>% 
    ungroup() %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(cntry_order = 1:n(),
           in.review = ifelse(in.review == 0, "No", "Yes")) 
    

### Total costs 
ggplot() + 
    geom_col(data = countryEstimates,
             aes(x=fct_reorder(study.country, cntry_order),
                 y=dstb_total_cost_country)) + 
    theme_minimal(base_size = 8) + 
    coord_flip() + xlab("Country") + 
    ylab("Total costs associated with non-RR/MDR-TB")

#### Table of coefficient values #### 
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
write.csv("~/Desktop/tempTotals2.csv")

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
        matrix(nrow=10, byrow = TRUE),
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
        matrix(nrow=10, byrow = TRUE) ,
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
        matrix(nrow=10, byrow = TRUE) ,
    
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
        matrix(nrow=10, byrow = TRUE) ,
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
        matrix(nrow=10, byrow = TRUE) 
    ) %>% write.csv("~/Desktop/temp2.csv")


#### Treatment phases 
treatmentEstimates <- countryEstimates %>%
    ungroup() %>%
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
            "Total" = sum(preDiagTotal, intsvTxTotal, contnTxTotal),
            .by=study.country) %>% 
    arrange(preDiagTotal/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(study.country, cntry_order, preDiagTotal, contains("TxTotal")) %>%
    rename(Diagnostic = preDiagTotal, 
           `Intensive treatment` = intsvTxTotal,
           `Continuation treatment` = contnTxTotal) %>%
    melt(id.vars = c("study.country","cntry_order"), value.name = "Cost", variable.name = "Category") %>%
    transform( order = factor(
        Category,
        levels=c( 'Diagnostic','Intensive treatment','Continuation treatment'),
        ordered =TRUE)) 

countryCostTypePlot <- ggplot() + 
    geom_bar(data = treatmentEstimates %>% filter(Category != "TxTotal"),
             aes(x=fct_reorder(study.country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 8) + 
    coord_flip() + ylab("Fraction of total out-of-pocket costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Diagnostic', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom"); countryCostTypePlot

ggsave(plot = countryCostTypePlot, 
       filename = here("output/figures/countryCostTypePlot.pdf"), width = 8.5, height=11, units = "in")

#### Now by cost type 
preDiagEstimates <- countryEstimates %>%
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

IntsvTxEstimates <- countryEstimates %>%
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

### continuation tx
ContnTxEstimates <- countryEstimates %>%
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

#### Geographic estimates ####
lmicDSTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            dstb_total_cost_country[country.id.new], 
                                            ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   median_qi(dstb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    select(country.code, in.review, dstb_total_cost_country) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"))

lmicDRTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            drtb_total_cost_country[country.id.new], 
                                            ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   median_qi(drtb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    select(country.code, in.review, drtb_total_cost_country) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"))


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
    geom_sf_pattern(data=world_dstb %>% na.omit() %>% rename("In review data" = in.review), 
                    aes(pattern_density=`In review data`,
                        fill=dstb_total_cost_country), 
                    pattern_scale = 1,
                    pattern_size = .1, 
                    pattern_fill="darkolivegreen",
                    pattern_colour = "darkolivegreen",
                    pattern_key_scale_factor = 0.25,
                    pattern = "stripe") + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Total household cost of TB") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    
    ggtitle("non-RR/MDR-TB care costs"); dstbCarePlot

ggsave(plot = dstbCarePlot, filename = here("output/figures/dstbCostMap.pdf"), width =11, height = 8.5)

drtbCarePlot <- ggplot() +
    geom_sf(data = world_drtb, fill="grey85") + 
    geom_sf_pattern(data=world_drtb %>% na.omit() %>% rename("In review data" = in.review), 
                    aes(pattern_density=`In review data`,
                        fill=drtb_total_cost_country), 
                    pattern_scale = 1,
                    pattern_size = .1, 
                    pattern_fill="darkolivegreen",
                    pattern_colour = "darkolivegreen",
                    pattern_key_scale_factor = 0.25,
                    pattern = "stripe") + 
    scale_fill_gradient(low = "darkolivegreen1", 
                        high = "darkolivegreen", 
                        na.value = "grey85", name = "Total household cost of TB") +
    theme_void() + 
    theme(legend.position = "bottom") + 
    guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
    ggtitle("RR/MDR-TB care costs"); drtbCarePlot

ggsave(plot = drtbCarePlot, filename = here("output/figures/drtbCostMap.png"), width =11, height = 8.5)
