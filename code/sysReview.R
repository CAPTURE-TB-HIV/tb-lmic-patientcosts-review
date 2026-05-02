###############################################################################
##### Load in the necessary packages 
###############################################################################
library(here)
library(dplyr)
library(tidyr)
library(rstan)

###############################################################################
##### Read in the model data                                              #####
###############################################################################
dataList <- readRDS(here("data/tempCleanData.rds"))

###############################################################################
##### Summarize data by categories                                        #####
###############################################################################

countryData <- left_join(dataList$data, 
                         dataList$descriptive %>% select(covidence.id, 
                                                         study.country), 
                         by = "covidence.id") %>% 
                mutate(dme.tbcare.total.mean = as.numeric(dme.tbcare.total.mean),
                       dme.tbcare.total.sd = as.numeric(dme.tbcare.total.sd))

countryData %>% select(contains("dme.tbcare.total.mean"), 
                                study.country) %>%
        reframe(meanCost = mean(as.numeric(dme.tbcare.total.mean), na.rm=TRUE), 
                .by = study.country) 

library(brms)

brms::brm(formula = dme.tbcare.total.mean | se(dme.tbcare.total.sd, sigma=TRUE) ~ 
              1  + (1|study.country) + (1|covidence.id),
          data = countryData,
          # prior = prior(student_t(7, 0, 1.5), class = Intercept) +
          #     prior(exponential(2), class = sd),
          family = gaussian(),
          control = list(adapt_delta = 0.99),
          chains = 4,
          cores = 4,
          iter = 4000)
