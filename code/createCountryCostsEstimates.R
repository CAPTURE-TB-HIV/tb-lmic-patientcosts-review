##############################################################################|
#### Load in packages #### 
##############################################################################|
library(reshape2)
library(geosphere)
library(forcats)
library(here)
library(dplyr)
library(tidyr) 
library(tidybayes)
library(scales)
library(forcats)
library(ggplot2)

tbPtntCstSeed <- 513

lmicSampsAll <- readRDS(here("output/allYears_10-19.rds"))
lmicSamps <- lmicSampsAll[[16]]
rm(lmicSampsAll)

covariatesTime <- readRDS(here("data/covariatesTime.rds"))
covariates <- covariatesTime %>% filter(study.end.year == 2024)
rm(covariatesTime)

samples <- spread_draws(lmicSamps, 
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
                        dstb_prediag_cost_country[country.id.new], 
                        dstb_intsvtx_cost_country[country.id.new], 
                        dstb_contntx_cost_country[country.id.new], 
                        dstb_total_cost_country[country.id.new], 
                        ndraws = 10000, seed = tbPtntCstSeed) 



whoCostEsts0 <- left_join(covariates %>% select(country.id.new, study.country), 
                                 samples %>%
                                mutate(dstb_total_alltx_cost_country = dstb_intsvtx_cost_country + dstb_contntx_cost_country, 
                                       dstb_dme_alltx_cost_country = dstb_dme_intsvtx_cost_country + dstb_dme_contntx_cost_country,
                                       dstb_dnme_alltx_cost_country = dstb_dnme_intsvtx_cost_country + dstb_dnme_contntx_cost_country,
                                       dstb_ie_alltx_cost_country = dstb_ie_intsvtx_cost_country + dstb_ie_contntx_cost_country) %>% 
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
                                        dstb_dme_alltx_cost_country, 
                                        dstb_dnme_alltx_cost_country, 
                                        dstb_ie_alltx_cost_country, 
                                        dstb_prediag_cost_country,
                                        dstb_intsvtx_cost_country,
                                        dstb_contntx_cost_country,
                                        dstb_total_cost_country), 
                                join_by(country.id.new)) %>% select(!c(.width, .point, .interval))


colnames(whoCostEsts0)[seq(3, ncol(whoCostEsts0), 3)] <- paste(colnames(whoCostEsts0)[seq(3, ncol(whoCostEsts0), 3)], "mean", sep=".")

whoCostEsts <- whoCostEsts0 %>% 
                    group_by(study.country) %>% 
                    pivot_longer(`dstb_dme_prediag_cost_country.mean`:`dstb_total_cost_country.upper`, names_sep = "\\.", names_to = c("Category", "Type"), values_to = "Cost") %>%
                    pivot_wider(id_cols = c(study.country, Category), names_from = Type, values_from = Cost) %>% 
                    mutate(Category = gsub("_cost_country", "", Category))


write.csv(whoCostEsts, "~/Desktop/countryPatientCostEstimates.csv")

