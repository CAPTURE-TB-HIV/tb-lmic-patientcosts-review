library(ggpattern)
library(sf)
library(rnaturalearth)
library(magick)
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(rstan)

##############################################################################|
##### Set seed, number of iterations (warmup and total)                   #####
##### for sampling in model fits.                                         ####|
##### Needs to be the same in the runPoolingModel.R script.               ####|
##############################################################################|

tbPtntCstSeed <- 513
tbPtntCstIter <- 1000
tbPtntCstWarmup <- 500

###############################################################################
##### Read in model and other data setup 
###############################################################################
### Model fit to data from systematic review
predModel <- stan_model("code/inst/tbPatientCosts-randEffCov.stan")
    #readRDS(here("output/modelswithunitnormal5.rds"))[["9covs"]]

### Data used to fit the model 
# modelData <- readRDS(here("output/modelswithunitnormal5.rds"))[["Data"]]

### All LMICs in 2025
LMIC <- read.csv(here("data/LMIC2025.csv")) %>% 
        rename(study.country = country.name) %>%
        select(! class25)

### External covariate datasets 
### Missingness of 2024 is too high so we use 2023

medianIncome <- readRDS(here("data/covariateData/medianIncomePrediction.rds")) %>% 
    select(Country.Code, study.end.year, median.income.calc.usd) %>% 
    rename(country.code = Country.Code, 
           median.income = median.income.calc.usd) %>% 
     filter(study.end.year == 2023, 
           country.code %in% LMIC$country.code) 

oopSpending <- readRDS(here("data/covariateData/healthAccounts.rds")) %>% 
               select(country.code, study.end.year, 
                      `Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`) %>% 
               mutate(oop.perc.che = as.numeric(`Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)`),
                      study.end.year = as.numeric(study.end.year)) %>%
               filter(study.end.year == 2023)

uhcIndex <- readRDS(here("data/covariateData/uhcIndex.rds")) %>% 
            rename(uhc.index = UHCindex) %>%
            mutate(study.end.year = as.numeric(study.end.year)) %>%
            filter(study.end.year == 2023, 
                   country.code %in% LMIC$country.code)

###############################################################################
##### Setup covariates for the prediction 
###############################################################################

covariates00 <- left_join(medianIncome, oopSpending, by = c("country.code", "study.end.year"))
covariates0  <- left_join(covariates00, uhcIndex, by = c("country.code", "study.end.year"))

### DRTB was internal to the review dataset and was a binary indicator
### We will model both so each entry above will get a 0 and 1 for drtb.ind
# drtb.ind <- 0:1
# covariates <- crossing(covariates, drtb.ind)

### Mean center study year 
covariates <- covariates0 %>% 
    filter(median.income >= min(modelData$median.income) & 
               median.income <= max(modelData$median.income), 
           uhc.index >= min(modelData$UHCindex) & 
               uhc.index <= max(modelData$UHCindex),
           oop.perc.che >= min(modelData$oop.perc.che) & 
               oop.perc.che <= max(modelData$oop.perc.che)) %>% 
              mutate(pred.year = study.end.year - floor(mean(modelData$study.end.year, na.rm=TRUE))) %>% 
                group_by(study.country) %>% 
                na.omit() %>%
                mutate(country.id.new = cur_group_id(), 
                       in.review = ifelse(country.code %in% modelData$country.code, 
                                          1, 0)) %>% 
                left_join(modelData %>% ungroup() %>% 
                          select(country.code, country.id) %>% unique()) %>% 
                mutate(in.review.id = ifelse(is.na(country.id), 99, country.id), 
                       oop.perc.che = as.numeric(oop.perc.che)) %>% 
                select(!country.id) 

### Save the countries for which we don't have covariates
countriesNoCovs <- covariates0 %>% filter(! country.code %in% covariates$country.code) 

# covariatesFilt <- covariates %>% 
#                   filter(median.income >= min(modelData$median.income) & 
#                          median.income <= max(modelData$median.income), 
#                          uhc.index >= min(modelData$UHCindex) & 
#                          uhc.index <= max(modelData$UHCindex),
#                          oop.perc.che >= min(modelData$oop.perc.che) & 
#                          oop.perc.che <= max(modelData$oop.perc.che))
# 
# countriesCovOutRange <- covariates %>% filter(! country.code %in% covariatesFilt$country.code) 
# 

# covariates <- covariatesFilt
### Stan requires a list for data. 
predictionData <- list(
            N = length(modelData$cost),
            cost = modelData$cost + 1,
            se = rep(0, length(modelData$cost)),
            se = as.numeric(modelData$se)+ 0.01,
            dme_prediag = modelData$dme.prediag, 
            dnme_prediag =  modelData$dnme.prediag, 
            ie_prediag =  modelData$ie.prediag, 
            dme_intsvtx = modelData$dme.intsvtx, 
            dnme_intsvtx =  modelData$dnme.intsvtx, 
            ie_intsvtx =  modelData$ie.intsvtx,
            dme_contntx = modelData$dme.contntx, 
            dnme_contntx =  modelData$dnme.contntx, 
            ie_contntx =  modelData$ie.contntx, 
            N_survey = max(modelData$survey.id),
            survey_id = modelData$survey.id, 
            N_country = max(modelData$country.id),
            country_id = modelData$country.id, 
            ### Internal covariate
            drtb_ind= modelData$drtb.ind,
            survey_year = modelData$year_z,
            ### External covariates
            median_income = ((modelData$median.income - mean(modelData$median.income)) / sd(modelData$median.income)),
            oop_perc_che =  (modelData$oop.perc.che - mean(modelData$oop.perc.che))/sd(modelData$UHCindex), 
            uhc_index =     (modelData$UHCindex - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
            ### Prediction data 
            N_country_new = max(covariates$country.id.new),
            review_ind = covariates$in.review, 
            in_review_id = covariates$in.review.id, # note that this is different from modelData country.id
            pred_year = unique(covariates$pred.year),
            median_income_new = ((covariates$median.income - mean(modelData$median.income)) / sd(modelData$median.income)), 
            oop_perc_che_new = (covariates$oop.perc.che - mean(modelData$oop.perc.che)) / sd(modelData$oop.perc.che), 
            uhc_index_new = (covariates$uhc.index - mean(modelData$UHCindex)) / sd(modelData$UHCindex), 
            prior_sigma = 2
)

##############################################################################|
##### Estimate the predicted costs using the model above
##############################################################################|
# predModel <- stan_model("code/inst/tbPatientCosts-randEffCov.stan")

lmicSamps <- rstan::sampling(
                    predModel, predictionData,
                    chains = 2, cores = 4,
                    warmup = tbPtntCstWarmup/4,
                    # warmup = 200,
                    iter = tbPtntCstIter/4,
                    control=list(max_treedepth=14),
                    refresh = 0, seed = tbPtntCstSeed
)

##############################################################################|
##### Some model checks
##############################################################################|

### Are the random effects mean close to 0 
indTemp<- grep("randeff_country", rownames(summary(lmicSamps)[[1]]))
mean(summary(lmicSamps)[[1]][indTemp[1:23],"mean"])

indTemp<- grep("randeff_survey", rownames(summary(lmicSamps)[[1]]))
mean(summary(lmicSamps)[[1]][indTemp[-(length(indTemp))],"mean"])

### check that observed costs are the same when no prediction


##############################################################################|
### Join the predictions and the data 
##############################################################################|
lmicDRTBPred <- left_join(covariates, 
                          spread_rvars(lmicSamps, 
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
                                       ndraws = 1000, seed = tbPtntCstSeed), 
                          join_by(country.id.new))

lmicDRTBPredInReview <- lmicDRTBPred %>% filter(in.review ==1) %>% left_join(modelData)

ggplot() + 
    geom_point(data=lmicDRTBPredInReview, aes(x=median.income, y=uhc.index), size=2) + 
    theme_minimal(base_size = 20)


lmicDRTBCovs <- spread_rvars(lmicSamps, 
                             beta_drtb_overall, 
                             beta_drtb_dme_prediag, 
                             beta_drtb_dnme_prediag, 
                             beta_drtb_ie_prediag, 
                             beta_drtb_dme_intsvtx, 
                             beta_drtb_dnme_intsvtx, 
                             beta_drtb_ie_intsvtx, 
                             beta_drtb_dme_contntx, 
                             beta_drtb_dnme_contntx, 
                             beta_drtb_ie_contntx,
                                       ndraws = 1000, seed = tbPtntCstSeed) %>% View()

lmicREs <- spread_rvars(lmicSamps, randeff_country, 
                             ndraws = 1000, seed = tbPtntCstSeed) %>% View()

lmicDSTBPred <- left_join(covariates, 
                          spread_rvars(lmicSamps, 
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
                                       ndraws = 250, seed = tbPtntCstSeed), 
                          join_by(country.id.new))

lmicDSTBPred_plot <- left_join(covariates, 
                          spread_draws(lmicSamps, 
                                       # dstb_dme_prediag_cost_country[country.id.new], 
                                       # dstb_dnme_prediag_cost_country[country.id.new], 
                                       # dstb_ie_prediag_cost_country[country.id.new], 
                                       # dstb_dme_intsvtx_cost_country[country.id.new], 
                                       # dstb_dnme_intsvtx_cost_country[country.id.new], 
                                       # dstb_ie_intsvtx_cost_country[country.id.new], 
                                       # dstb_dme_contntx_cost_country[country.id.new], 
                                       # dstb_dnme_contntx_cost_country[country.id.new], 
                                       # dstb_ie_contntx_cost_country[country.id.new], 
                                       dstb_total_cost_country[country.id.new], 
                                       ndraws = 250, seed = tbPtntCstSeed) %>% 
                              group_by(country.id.new) %>% 
                          mean_qi(dstb_total_cost_country), 
                          join_by(country.id.new)) %>% 
                        select(country.code, in.review, dstb_total_cost_country, .lower, .upper) %>% 
                        mutate(in.review = ifelse(in.review == 0, "No", "Yes"))

lmicDRTBPred_plot <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            # dstb_dme_prediag_cost_country[country.id.new], 
                                            # dstb_dnme_prediag_cost_country[country.id.new], 
                                            # dstb_ie_prediag_cost_country[country.id.new], 
                                            # dstb_dme_intsvtx_cost_country[country.id.new], 
                                            # dstb_dnme_intsvtx_cost_country[country.id.new], 
                                            # dstb_ie_intsvtx_cost_country[country.id.new], 
                                            # dstb_dme_contntx_cost_country[country.id.new], 
                                            # dstb_dnme_contntx_cost_country[country.id.new], 
                                            # dstb_ie_contntx_cost_country[country.id.new], 
                                            drtb_total_cost_country[country.id.new], 
                                            ndraws = 250, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   mean_qi(drtb_total_cost_country), 
                               join_by(country.id.new)) %>% 
    # select(country.code, in.review, drtb_total_cost_country, .lower, .upper) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes"))

lmicDRTBPred_tblst <- left_join(covariates, 
                               spread_draws(lmicSamps, 
                                            # dstb_dme_prediag_cost_country[country.id.new], 
                                            # dstb_dnme_prediag_cost_country[country.id.new], 
                                            # dstb_ie_prediag_cost_country[country.id.new], 
                                            # dstb_dme_intsvtx_cost_country[country.id.new], 
                                            # dstb_dnme_intsvtx_cost_country[country.id.new], 
                                            # dstb_ie_intsvtx_cost_country[country.id.new], 
                                            # dstb_dme_contntx_cost_country[country.id.new], 
                                            drtb_dnme_contntx_cost_country[country.id.new],
                                            # dstb_ie_contntx_cost_country[country.id.new], 
                                            # drtb_total_cost_country[country.id.new], 
                                            ndraws = 250, seed = tbPtntCstSeed) %>% 
                                   group_by(country.id.new) %>% 
                                   mean_qi(drtb_dnme_contntx_cost_country), 
                               join_by(country.id.new)) %>% 
    # select(country.code, in.review, drtb_total_cost_country, .lower, .upper) %>% 
    mutate(in.review = ifelse(in.review == 0, "No", "Yes")) 

    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_dnme_contntx_cost_country, 
               y=median.income)) + geom_point()
    
    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_dnme_contntx_cost_country, 
               y=oop.perc.che)) + geom_point()

    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_dnme_contntx_cost_country, 
               y=uhc.index)) + geom_point()
    
    lmicDRTBPred_tblst <- left_join(covariates, 
                                    spread_draws(lmicSamps, 
                                                 # dstb_dme_prediag_cost_country[country.id.new], 
                                                 # dstb_dnme_prediag_cost_country[country.id.new], 
                                                 # dstb_ie_prediag_cost_country[country.id.new], 
                                                 # dstb_dme_intsvtx_cost_country[country.id.new], 
                                                 # dstb_dnme_intsvtx_cost_country[country.id.new], 
                                                 # dstb_ie_intsvtx_cost_country[country.id.new], 
                                                 # dstb_dme_contntx_cost_country[country.id.new], 
                                                 drtb_dnme_contntx_cost_country[country.id.new],
                                                 # drtb_ie_contntx_cost_country[country.id.new],
                                                 drtb_total_cost_country[country.id.new],
                                                 ndraws = 1000, seed = tbPtntCstSeed) %>% 
                                        group_by(country.id.new) %>% 
                                        mutate(perc_dnme_contntx = drtb_dnme_contntx_cost_country/drtb_total_cost_country*100)%>%
                                        mean_qi(perc_dnme_contntx, drtb_total_cost_country), 
                                    join_by(country.id.new)) %>% 
        # select(country.code, in.review, drtb_total_cost_country, .lower, .upper) %>% 
        mutate(in.review = ifelse(in.review == 0, "No", "Yes")) 
    
    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_total_cost_country, 
               y=perc_dnme_contntx)) + geom_point(size=4)  + 
        theme_minimal(base_size=24)
    
    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_ie_contntx_cost_country, 
               y=oop.perc.che)) + geom_point()
    
    ggplot(data=lmicDRTBPred_tblst, 
           aes(x=drtb_ie_contntx_cost_country, 
               y=uhc.index)) + geom_point()
    
    
    ggplot(data=lmicDRTBPred_plot, 
           aes(x=drtb_total_cost_country, 
               y=median.income, 
               color = uhc.index)) + geom_point(size=4)  + 
        theme_minimal(base_size=24)
    
    ggplot(data=lmicDRTBPred_plot, 
           aes(x=drtb_total_cost_country, 
               y=oop.perc.che, 
               color=median.income)) + geom_point() + geom_point(size=4)  + 
        theme_minimal(base_size=24)
    
    ggplot(data=lmicDRTBPred_plot, 
           aes(x=drtb_total_cost_country, 
               y=uhc.index,
               color = oop.perc.che)) + geom_point() + geom_point(size=4)  + 
        theme_minimal(base_size=24)

### Format predictions 
##############################################################################|
##### Save predictions as a CSV and R dataset 
##############################################################################|

# saveRDS(lmicPred, here("output/costPredictions.rds"), version=2)
# write.csv(lmicPred, here("output/costPredictions.csv"))

##############################################################################|
##### Create a plot of all costs 
##############################################################################|
world_dstb <-  ne_countries() %>% 
          filter(sovereignt != "Antarctica") %>% 
          select(sovereignt, sov_a3, geometry) %>% 
          rename(country.code = sov_a3) %>% 
          left_join(lmicDSTBPred_plot) 

lmicDSTBPred_plot %>% ungroup() %>% filter(in.review == "Yes") %>% select(dstb_total_cost_country) %>% range() %>% round()
lmicDSTBPred_plot %>% ungroup() %>% filter(in.review == "Yes") %>% select(dstb_total_cost_country) %>% unlist() %>% mean()

lmicDSTBPred_plot %>% ungroup() %>% select(dstb_total_cost_country) %>% range() %>% round()
lmicDSTBPred_plot %>% ungroup() %>% select(dstb_total_cost_country) %>% unlist() %>% mean()


world_drtb <-  ne_countries() %>% 
    filter(sovereignt != "Antarctica") %>% 
    select(sovereignt, sov_a3, geometry) %>% 
    rename(country.code = sov_a3) %>% 
    left_join(lmicDRTBPred_plot)

lmicDRTBPred_plot %>% ungroup() %>% filter(in.review == "Yes") %>% select(drtb_total_cost_country) %>% range() %>% round()
lmicDRTBPred_plot %>% ungroup() %>% filter(in.review == "Yes") %>% select(drtb_total_cost_country) %>% unlist() %>% mean()

lmicDRTBPred_plot %>% ungroup() %>% select(drtb_total_cost_country) %>% range() %>% round()
lmicDRTBPred_plot %>% ungroup() %>% select(drtb_total_cost_country) %>% unlist() %>% mean()

lmicDRTBPred_plot %>% filter(drtb_total_cost_country > 10000) %>% write.csv("~/Desktop/countriesOver10.csv")

dstbCarePlot <- ggplot() +
                geom_sf(data = world_dstb, fill="grey85") + 
                geom_sf_pattern(data=world_dstb %>% na.omit(), 
                                    aes(pattern_density=in.review,
                                    fill=dstb_total_cost_country), 
                                pattern_scale = 0.1, 
                                pattern_fill="darkolivegreen",
                                pattern_size = 0,
                                pattern = "circle") + 
                scale_fill_gradient(low = "darkolivegreen1", 
                                    high = "darkolivegreen", 
                                    na.value = "grey85") +
                guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
                theme_minimal() + 
                theme(legend.position = "bottom") + 
                ggtitle("DSTB care costs")

ggsave(plot = dstbCarePlot, filename = here("output/dstbCostMap.png"))

drtbCarePlot <- ggplot() +
                geom_sf(data = world_drtb, fill="grey85") + 
                geom_sf_pattern(data=world_drtb %>% na.omit(), 
                                aes(pattern_density=in.review,
                                    fill=drtb_total_cost_country), 
                                pattern_scale = 0.1, 
                                pattern_fill="darkolivegreen",
                                pattern_size = 0,
                                pattern = "circle") + 
                scale_fill_gradient(low = "darkolivegreen1", 
                                    high = "darkolivegreen", 
                                    na.value = "grey85") +
                # guides(pattern = guide_legend(override.aes = list(fill = c("darkolivegreen1")))) +
                theme_minimal() + 
                theme(legend.position = "bottom") + 
                ggtitle("DRTB care costs"); drtbCarePlot

ggsave(plot = drtbCarePlot, filename = here("output/drtbCostMap.png"))


tmp <- left_join(lmicDSTBPred_plot, lmicDRTBPred_plot, by=c("study.country", "country.code", "in.review"))

ggplot(tmp, aes(x=dstb_total_cost_country, y=drtb_total_cost_country)) + geom_point(size=4)  + 
    theme_minimal(base_size=24)
