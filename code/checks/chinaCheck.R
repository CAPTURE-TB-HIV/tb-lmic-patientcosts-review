library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(rstan)
library(tidybayes)

##############################################################################|
##### Set seed, number of iterations (warmup and total)                   #####
##### for sampling in model fits.                                         ####|
##### Needs to be the same in the runPoolingModel.R script.               ####|
##############################################################################|

tbPtntCstSeed <- 513
tbPtntCstIter <- 4000
tbPtntCstWarmup <- 500

###############################################################################
##### Read in model and other data setup 
###############################################################################
basicModel <- stan_model("code/inst/basicModel.stan")

### Data used to fit the model 
source(here("code/modelDataFormat.R"))


ggplot() +
    geom_point(data = modelData %>% 
            select(study.country, study.end.year, median.income), aes(x=study.country, y=median.income))  + 
    geom_point(data= modelData %>% select(study.country, study.end.year, median.income) %>% 
                   filter(study.country == "China"), aes(x=study.country, y=median.income), color = "red")  + 
    theme_minimal() + coord_flip()
