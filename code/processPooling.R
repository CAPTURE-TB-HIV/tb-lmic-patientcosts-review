library(reshape2)
library(forcats)
library(ggplot2)
library(dplyr)
processPooledModel <- function(modelFit){
    filename <- here(paste0("output/descriptive/tracePlotsDRDSTB.pdf"))
    pdf(file = filename, height = 8.5,  width= 11.5)
    
    print(traceplot(modelFit, 
              inc_warmup = FALSE))
    
    
    print(stan_hist(modelFit, pars = c("beta_dme_prediag", "beta_dnme_prediag",
                                    "beta_ie_prediag", "beta_dme_intsvtx",
                                    "beta_dnme_intsvtx", "beta_ie_intsvtx", 
                                    "beta_dme_contntx", "beta_dnme_contntx",
                                    "beta_ie_contntx")))
    
    print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("randeff_ie_prediag[1]","randeff_ie_prediag[2]",
                     "randeff_ie_prediag[3]", "randeff_ie_prediag[4]",
                     "randeff_ie_prediag[5]","randeff_ie_prediag[6]",
                     "randeff_ie_prediag[7]","randeff_ie_prediag[8]",
                     "randeff_ie_prediag[9]","randeff_ie_prediag[10]",
                     "randeff_ie_prediag[11]","randeff_ie_prediag[12]")))
    
    print(stan_hist(modelFit, pars = c("randeff_ie_prediag[1]","randeff_ie_prediag[2]",
                                    "randeff_ie_prediag[3]", "randeff_ie_prediag[4]",
                                    "randeff_ie_prediag[5]","randeff_ie_prediag[6]",
                                    "randeff_ie_prediag[7]","randeff_ie_prediag[8]",
                                    "randeff_ie_prediag[9]","randeff_ie_prediag[10]",
                                    "randeff_ie_prediag[11]","randeff_ie_prediag[12]")))
    
    print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("randeff_ie_prediag[13]","randeff_ie_prediag[14]",
                     "randeff_ie_prediag[15]", "randeff_ie_prediag[16]",
                     "randeff_ie_prediag[17]","randeff_ie_prediag[18]",
                     "randeff_ie_prediag[19]","randeff_ie_prediag[20]",
                     "randeff_ie_prediag[21]","randeff_ie_prediag[22]",
                     "randeff_ie_prediag[23]","randeff_ie_prediag[24]")))
    
    # print(stan_hist(modelFit, pars = c("randeff_ie_prediag[13]","randeff_ie_prediag[14]",
    #                                 "randeff_ie_prediag[15]", "randeff_ie_prediag[16]",
    #                                 "randeff_ie_prediag[17]","randeff_ie_prediag[18]",
    #                                 "randeff_ie_prediag[19]","randeff_ie_prediag[20]",
    #                                 "randeff_ie_prediag[21]","randeff_ie_prediag[22]",
    #                                 "randeff_ie_prediag[23]","randeff_ie_prediag[24]")))
    
    # print(traceplot(modelFit, 
    #           inc_warmup = FALSE, 
    #           pars=c("randeff_ie_prediag[25]","randeff_ie_prediag[26]",
    #                  "randeff_ie_prediag[27]", "randeff_ie_prediag[28]",
    #                  "randeff_ie_prediag[29]","randeff_ie_prediag[30]",
    #                  "randeff_ie_prediag[31]","randeff_ie_prediag[32]",
    #                  "randeff_ie_prediag[33]","randeff_ie_prediag[34]",
    #                  "randeff_ie_prediag[35]","randeff_ie_prediag[36]")))
    
    print(traceplot(modelFit,
              inc_warmup = FALSE,
              pars=c("sigma_randeff_dme_prediag", "sigma_randeff_dnme_prediag",
                     "sigma_randeff_ie_prediag", "sigma_randeff_dme_intsvtx",
                     "sigma_randeff_dnme_intsvtx", "sigma_randeff_ie_intsvtx", 
                     "sigma_randeff_dme_contntx", "sigma_randeff_dnme_contntx",
                     "sigma_randeff_ie_contntx")))
    
          print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("cost_dme_prediag[1]","cost_dme_prediag[2]",
                     "cost_dme_prediag[3]", "cost_dme_prediag[4]",
                     "cost_dme_prediag[5]","cost_dme_prediag[6]",
                     "cost_dme_prediag[7]","cost_dme_prediag[8]",
                     "cost_dme_prediag[9]","cost_dme_prediag[10]",
                     "cost_dme_prediag[11]","cost_dme_prediag[12]")))
    
          print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("cost_dme_prediag[13]","cost_dme_prediag[14]",
                     "cost_dme_prediag[15]", "cost_dme_prediag[16]",
                     "cost_dme_prediag[17]","cost_dme_prediag[18]",
                     "cost_dme_prediag[19]","cost_dme_prediag[20]",
                     "cost_dme_prediag[21]","cost_dme_prediag[22]",
                     "cost_dme_prediag[23]","cost_dme_prediag[24]")))
    
              # print(traceplot(modelFit, 
              # inc_warmup = FALSE, 
              # pars=c("cost_dme_prediag[25]","cost_dme_prediag[26]",
              #        "cost_dme_prediag[27]", "cost_dme_prediag[28]",
              #        "cost_dme_prediag[29]","cost_dme_prediag[30]",
              #        "cost_dme_prediag[31]","cost_dme_prediag[32]",
              #        "cost_dme_prediag[33]","cost_dme_prediag[34]",
              #        "cost_dme_prediag[35]","cost_dme_prediag[36]")))
              
              print(traceplot(modelFit, 
                              inc_warmup = FALSE, 
                              pars=c("cost_ie_contntx[1]","cost_ie_contntx[2]",
                                     "cost_ie_contntx[3]", "cost_ie_contntx[4]",
                                     "cost_ie_contntx[5]","cost_ie_contntx[6]",
                                     "cost_ie_contntx[7]","cost_ie_contntx[8]",
                                     "cost_ie_contntx[9]","cost_ie_contntx[10]",
                                     "cost_ie_contntx[11]","cost_ie_contntx[12]")))
              
              
              print(stan_hist(modelFit, 
                              pars=c("cost_ie_contntx[1]","cost_ie_contntx[2]",
                                     "cost_ie_contntx[3]", "cost_ie_contntx[4]",
                                     "cost_ie_contntx[5]","cost_ie_contntx[6]",
                                     "cost_ie_contntx[7]","cost_ie_contntx[8]",
                                     "cost_ie_contntx[9]","cost_ie_contntx[10]",
                                     "cost_ie_contntx[11]","cost_ie_contntx[12]")))
              
              print(traceplot(modelFit, 
                              inc_warmup = FALSE, 
                              pars=c("cost_ie_contntx[13]","cost_ie_contntx[14]",
                                     "cost_ie_contntx[15]", "cost_ie_contntx[16]",
                                     "cost_ie_contntx[17]","cost_ie_contntx[18]",
                                     "cost_ie_contntx[19]","cost_ie_contntx[20]",
                                     "cost_ie_contntx[21]","cost_ie_contntx[22]",
                                     "cost_ie_contntx[23]","cost_ie_contntx[24]")))
              
              
              print(stan_hist(modelFit, 
                              pars=c("cost_ie_contntx[13]","cost_ie_contntx[14]",
                                     "cost_ie_contntx[15]", "cost_ie_contntx[16]",
                                     "cost_ie_contntx[17]","cost_ie_contntx[18]",
                                     "cost_ie_contntx[19]","cost_ie_contntx[20]",
                                     "cost_ie_contntx[21]","cost_ie_contntx[22]",
                                     "cost_ie_contntx[23]","cost_ie_contntx[24]")))
              
              print(traceplot(modelFit, 
                              inc_warmup = FALSE, 
                              pars=c("cost_ie_contntx[25]","cost_ie_contntx[26]",
                                     "cost_ie_contntx[27]", "cost_ie_contntx[28]",
                                     "cost_ie_contntx[29]","cost_ie_contntx[30]",
                                     "cost_ie_contntx[31]","cost_ie_contntx[32]",
                                     "cost_ie_contntx[33]","cost_ie_contntx[34]",
                                     "cost_ie_contntx[35]","cost_ie_contntx[36]")))
              
              
              print(stan_hist(modelFit, 
                              pars=c("cost_ie_contntx[25]","cost_ie_contntx[26]",
                                     "cost_ie_contntx[27]", "cost_ie_contntx[28]",
                                     "cost_ie_contntx[29]","cost_ie_contntx[30]",
                                     "cost_ie_contntx[31]","cost_ie_contntx[32]",
                                     "cost_ie_contntx[33]","cost_ie_contntx[34]",
                                     "cost_ie_contntx[35]","cost_ie_contntx[36]")))
    
    
              print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("mu[1]","mu[2]",
                     "mu[3]", "mu[4]",
                     "mu[5]","mu[6]",
                     "mu[7]","mu[8]",
                     "mu[9]","mu[10]",
                     "mu[11]","mu[12]")))
    
    
              print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("mu[13]","mu[14]",
                     "mu[15]", "mu[16]",
                     "mu[17]","mu[18]",
                     "mu[19]","mu[20]",
                     "mu[21]","mu[22]",
                     "mu[23]","mu[24]")))
    
              print(traceplot(modelFit, 
              inc_warmup = FALSE, 
              pars=c("mu[25]","mu[26]",
                     "mu[27]", "mu[28]",
                     "mu[29]","mu[30]",
                     "mu[31]","mu[32]",
                     "mu[33]","mu[34]",
                     "mu[35]","mu[36]")))
    
    dev.off()
}
library(forcats)
library(ggplot2)
dataFull <- modelData %>% left_join(dataDesc %>% 
                                        select(covidence.id, 
                                               study.country, 
                                               publication.year)) %>%
            mutate(category = case_when(dme.prediag == 1 ~ "dme prediagnosis", 
                                        dnme.prediag == 1 ~ "dnme prediagnosis", 
                                        ie.prediag == 1 ~ "ie prediagnosis",
                                        dme.intsvtx == 1 ~ "dme intensive treatment", 
                                        dnme.intsvtx == 1 ~ "dnme intensive treatment", 
                                        ie.intsvtx == 1 ~ "ie intensive treatment",
                                        dme.contntx == 1 ~ "dme continuation treatment", 
                                        dnme.contntx == 1 ~ "dnme continuation treatment", 
                                        ie.contntx == 1 ~ "ie continuation treatment")
)

################################################################################
### Create country level estimates #############################################
################################################################################
### INCLUDES ALL ESTIMATES 
modelEstimates <- get_posterior_mean(lmicSamps)[,"mean-all chains"]

### Number of unique 

countryEstimates <- data.frame(
    country = sort(unique(modelData$study.country)),
    dmePreDiagEst = 
        modelEstimates[grep("cost_dme_prediag", names(modelEstimates))],

    dnmePreDiagEst =
        modelEstimates[grep("cost_dnme_prediag", names(modelEstimates))],

    iePreDiagEst =
        modelEstimates[grep("cost_ie_prediag", names(modelEstimates))],
    
    dmeIntsvTxEst =
        modelEstimates[grep("cost_dme_intsvtx", names(modelEstimates))],

    dnmeIntsvTxEst =
        modelEstimates[grep("cost_dnme_intsvtx", names(modelEstimates))],

    ieIntsvTxEst =
        modelEstimates[grep("cost_ie_intsvtx", names(modelEstimates))],

    dmeContnTxEst =
        modelEstimates[grep("cost_dme_contntx", names(modelEstimates))],

    dnmeContnTxEst =
        modelEstimates[grep("cost_dnme_contntx", names(modelEstimates))],

    ieContnTxEst =
        modelEstimates[grep("cost_ie_contntx", names(modelEstimates))]

) 

countryEstimates$totalEst <- rowSums(countryEstimates[,-1])

ggplot() + 
    geom_col(data = reshape2::melt(countryEstimates, id.vars = "country"), 
               aes(x=country, y=value)) + 
    facet_wrap(facets = vars(variable)) + theme_minimal() + 
    scale_y_log10() + coord_flip()

ggplot() + 
    geom_col(data = countryEstimates,
             aes(x=country, y=totalEst)) + 
    theme_minimal() + 
    coord_flip()

ggplot() + 
    geom_col(data = countryEstimates,
             aes(x=country, y=totalEst)) + 
    theme_minimal() + 
    coord_flip()

################################################################################
### INCLUDES ONLY THOSE WITH WHO HANDBOOK 
# modelEstimatesHB <- get_posterior_mean(whoHbModel)[,4]

# countryEstimatesHB <- data.frame(
#     country = sort(unique(modelDataHB$study.country)),
#     dmePreDiagEst = 
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dme_prediag[1]") + 0:31],
#     
#     dnmePreDiagEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dnme_prediag[1]") + 0:31],
#     
#     iePreDiagEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_ie_prediag[1]") + 0:31],
#     
#     dmeIntsvTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dme_intsvtx[1]") + 0:31],
#     
#     dnmeIntsvTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dnme_intsvtx[1]") + 0:31],
#     
#     ieIntsvTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_ie_intsvtx[1]") + 0:31],
#     
#     dmeContnTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dme_contntx[1]") + 0:31],
#     
#     dnmeContnTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_dnme_contntx[1]") + 0:31],
#     
#     ieContnTxEst =
#         modelEstimatesHB[which(names(modelEstimatesHB) == "cost_ie_contntx[1]") + 0:31]
#     
# ) 
# 
# countryEstimatesHB$totalEst <- rowSums(countryEstimatesHB[,-1])
# 
# ggplot() + 
#     geom_col(data = reshape2::melt(countryEstimates %>% select(!totalEst), id.vars = "country"), 
#              aes(x=country, y=value)) + 
#     facet_wrap(facets = vars(variable)) + theme_minimal() + 
#     coord_flip()
# 
# ggplot() + 
#     geom_col(data = countryEstimatesHB %>% mutate(country = fct_reorder(country, totalEst)),
#              aes(x=country, y=totalEst)) + 
#     theme_minimal() + 
#     coord_flip()


#### Intensive vs. continuation treatment 
library(reshape2)
treatmentEstimates <- countryEstimates %>%
                      reframe("preDiagTotal" = sum(dmePreDiagEst, dnmePreDiagEst, iePreDiagEst),
                              "intsvTxTotal" = sum(dmeIntsvTxEst, dnmeIntsvTxEst, ieIntsvTxEst),
                              "contnTxTotal" = sum(dmeContnTxEst, dnmeContnTxEst, ieContnTxEst), 
                              "TxTotal" = sum(dmeIntsvTxEst, dnmeIntsvTxEst, ieIntsvTxEst,
                                             dmeContnTxEst, dnmeContnTxEst, ieContnTxEst),
                              "Total" = sum(dmePreDiagEst, dnmePreDiagEst, iePreDiagEst,
                                           dmeIntsvTxEst, dnmeIntsvTxEst, ieIntsvTxEst,
                                           dmeContnTxEst, dnmeContnTxEst, ieContnTxEst),
                             .by=country) %>% 
                      arrange(preDiagTotal/Total) %>% 
                      mutate(cntry_order = 1:n()) %>% 
                      select(country, cntry_order, preDiagTotal, contains("TxTotal")) %>%
                      rename(Diagnostic = preDiagTotal, 
                             `Intensive treatment` = intsvTxTotal,
                             `Continuation treatment` = contnTxTotal) %>%
                      melt(id.vars = c("country","cntry_order"), value.name = "Cost", variable.name = "Category") %>%
                      transform( order = factor(
                                    Category,
                                    levels=c( 'Diagnostic','Intensive treatment','Continuation treatment'),
                                    ordered =TRUE)) 

ggplot() + 
    geom_bar(data = treatmentEstimates %>% filter(Category != "TxTotal"),
             aes(x=fct_reorder(country, cntry_order), y=Cost, fill=fct_rev(order)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 16) + 
    coord_flip() + ylab("Fraction of total out-of-pocket costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#9999CC","#CC6666", "#66CC99"), 
                        breaks=c('Diagnostic', 'Intensive treatment', 'Continuation treatment')) +
    theme(legend.position = "bottom")



#### Now by cost type 
preDiagEstimates <- countryEstimates %>%
    group_by(country) %>%
    mutate("Total" = sum(dmePreDiagEst, dnmePreDiagEst, iePreDiagEst)) %>%
    ungroup() %>%
    arrange(dmePreDiagEst/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(country, cntry_order, dmePreDiagEst, dnmePreDiagEst, iePreDiagEst, Total) %>%
    rename(`Direct medical` = dmePreDiagEst, 
           `Direct non-medical` = dnmePreDiagEst,
           `Indirect` = iePreDiagEst) %>%
    melt(id.vars = c("country","cntry_order"), value.name = "Cost", variable.name = "Category")

ggplot() + 
    geom_bar(data = preDiagEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 16) + 
    coord_flip() + ylab("Fraction of diagnostic costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#448866", "#66CC99", "#99ddbb"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom")

IntsvTxEstimates <- countryEstimates %>%
    group_by(country) %>%
    mutate("Total" = sum(dmeIntsvTxEst, dnmeIntsvTxEst, ieIntsvTxEst)) %>%
    ungroup() %>%
    arrange(dmeIntsvTxEst/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(country, cntry_order, dmeIntsvTxEst, dnmeIntsvTxEst, ieIntsvTxEst, Total) %>%
    rename(`Direct medical` = dmeIntsvTxEst, 
           `Direct non-medical` = dnmeIntsvTxEst,
           `Indirect` = ieIntsvTxEst) %>%
    melt(id.vars = c("country","cntry_order"), value.name = "Cost", variable.name = "Category")

ggplot() + 
    geom_bar(data = IntsvTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 16) + 
    coord_flip() + ylab("Fraction of intensive treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#884444", "#CC6666", "#dd9999"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom")

### continuation tx
ContnTxEstimates <- countryEstimates %>%
    group_by(country) %>%
    mutate("Total" = sum(dmeContnTxEst, dnmeContnTxEst, ieContnTxEst)) %>%
    ungroup() %>%
    arrange(dmeContnTxEst/Total) %>% 
    mutate(cntry_order = 1:n()) %>% 
    select(country, cntry_order, dmeContnTxEst, dnmeContnTxEst, ieContnTxEst, Total) %>%
    rename(`Direct medical` = dmeContnTxEst, 
           `Direct non-medical` = dnmeContnTxEst,
           `Indirect` = ieContnTxEst) %>%
    melt(id.vars = c("country","cntry_order"), value.name = "Cost", variable.name = "Category")

ggplot() + 
    geom_bar(data = ContnTxEstimates %>% filter(Category != "Total"),
             aes(x=fct_reorder(country, cntry_order), y=Cost, fill=fct_rev(Category)),
             position="fill", stat="identity") + 
    theme_minimal(base_size = 16) + 
    coord_flip() + ylab("Fraction of continuation treatment costs") +
    xlab("") + theme(legend.title = element_blank()) + 
    scale_fill_discrete(type=c("#725f7d", "#9999CC", "#e2ccdf"), 
                        breaks=c('Direct medical', 'Direct non-medical', 'Indirect')) +
    theme(legend.position = "bottom")

#### Geographic estimates 
world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

world_map[which(world_map$region == "Democratic Republic of the Congo"), "region"] <- "Democratic Republic of Congo"
world_map[which(world_map$region == "Laos"), "region"] <- "Lao PDR"
world_map[which(world_map$region == "Gambia"), "region"] <- "The Gambia"
world_map[which(world_map$region == "Vietnam"), "region"] <- "Viet Nam"

library(geosphere)
centroids <- world_map %>% 
    group_by(region) %>% 
    group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat))))

costCountriesGeo <- left_join(countryEstimates %>% 
                               select(country, totalEst), centroids %>% rename(country = region))

    ggplot(costCountriesGeo) +
        geom_map(
            dat = world_map, map = world_map, aes(map_id = region),
            fill = "grey95", color = "black", size = 0.25) +
        geom_map(map = world_map, aes(map_id = country, fill = totalEst), 
                 color="black", size = 0.25) +
        scale_fill_gradient(breaks = seq(0,7500, 1500),
                            high="darkolivegreen1", 
                            low = "darkolivegreen",
                            name = "2024 USD") +
        expand_limits(x = world_map$long, y = world_map$lat) + 
        theme_void(base_size = 18) + theme(legend.position = "inside",
                                           legend.position.inside = c(.15,.2)) + 
        # with(costCountriesGeo, annotate(geom="text", x = lon, y=lat, label = n, size = 5)) +
        ggtitle ("Total out-of-pocket costs of TB")



### Intensive vs. continuation over years 
# modelEstimatesStudy <- modelEstimates[which(names(modelEstimates) == "cost_dme_prediag[1]") + 0:36]