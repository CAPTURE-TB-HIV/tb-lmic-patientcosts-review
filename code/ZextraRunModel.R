###############################################################################
##### Call the stan model.                                                #####
###############################################################################
# totalModelYear <- stan(file = here("code/inst/tbPatientCostsPoolingYear.stan"),
#                    data = stanModelData,
#                    iter = 1000, chains = 3,
#                    control=list(max_treedepth=14))
# totalModel

###############################################################################
##### NOT UPDATED FOR ADDITIONAL POPULATION FILTERING SO DO NOT USE       #####
##### Test for subgroups                                                  #####
###############################################################################
modelDataHB <- modelData %>% filter(whohb == 1)
modelDataHB %>% select(covidence.id) %>% unique() %>% nrow()

stanModelDataHB <- list(
    N = length(modelDataHB$cost),
    cost = modelDataHB$cost + 0.01,
    se = rep(0, length(modelDataHB$cost)),
    # se = as.numeric(modelDataHB$se)+ 0.01,
    dme_prediag = modelDataHB$dme.prediag, 
    dnme_prediag =  modelDataHB$dnme.prediag, 
    ie_prediag =  modelDataHB$ie.prediag, 
    dme_intsvtx = modelDataHB$dme.intsvtx, 
    dnme_intsvtx =  modelDataHB$dnme.intsvtx, 
    ie_intsvtx =  modelDataHB$ie.intsvtx,
    dme_contntx = modelDataHB$dme.contntx, 
    dnme_contntx =  modelDataHB$dnme.contntx, 
    ie_contntx =  modelDataHB$ie.contntx, 
    N_country = max(modelDataHB$country.id),
    country_id = modelDataHB$country.id)

whoHbModel <- stan(file = here("code/inst/tbPatientCosts.stan"), 
                     data = stanModelDataHB, 
                     iter = 1000, chains = 3, 
                     control=list(max_treedepth=14))
whoHbModel

###############################################################################
modelDataUrb <- modelData %>% filter(urban == 1)
modelDataUrb %>% select(covidence.id) %>% unique() %>% nrow()
modelDataUrb %>% nrow()

stanModelDataUrb <- list(
    N = length(modelDataUrb$cost),
    cost = modelDataUrb$cost + 0.01,
    se = rep(0, length(modelDataUrb$cost)),
    # se = as.numeric(modelDataUrb$se)+ 0.01,
    dme_prediag = modelDataUrb$dme.prediag, 
    dnme_prediag =  modelDataUrb$dnme.prediag, 
    ie_prediag =  modelDataUrb$ie.prediag, 
    dme_intsvtx = modelDataUrb$dme.intsvtx, 
    dnme_intsvtx =  modelDataUrb$dnme.intsvtx, 
    ie_intsvtx =  modelDataUrb$ie.intsvtx,
    dme_contntx = modelDataUrb$dme.contntx, 
    dnme_contntx =  modelDataUrb$dnme.contntx, 
    ie_contntx =  modelDataUrb$ie.contntx, 
    drtb=modelDataUrb$drtb
)

whoUrbModel <- stan(file = here("code/inst/tbPatientCosts.stan"), 
                     data = stanModelDataUrb, 
                     iter = 1000, chains = 3, 
                     control=list(max_treedepth=14))
whoUrbModel

###############################################################################
modelDataPub <- modelData %>% filter(public == 1)
modelDataPub %>% select(covidence.id) %>% unique() %>% nrow()
modelDataPub %>% nrow()

stanModelDataPub <- list(
    N = length(modelDataPub$cost),
    cost = modelDataPub$cost + 0.01,
    se = rep(0, length(modelDataPub$cost)),
    # se = as.numeric(modelDataPub$se)+ 0.01,
    dme_prediag = modelDataPub$dme.prediag, 
    dnme_prediag =  modelDataPub$dnme.prediag, 
    ie_prediag =  modelDataPub$ie.prediag, 
    dme_intsvtx = modelDataPub$dme.intsvtx, 
    dnme_intsvtx =  modelDataPub$dnme.intsvtx, 
    ie_intsvtx =  modelDataPub$ie.intsvtx,
    dme_contntx = modelDataPub$dme.contntx, 
    dnme_contntx =  modelDataPub$dnme.contntx, 
    ie_contntx =  modelDataPub$ie.contntx, 
    drtb=modelDataPub$drtb
)

whoPubModel <- stan(file = here("code/inst/tbPatientCosts.stan"), 
                      data = stanModelDataPub, 
                      iter = 1000, chains = 3, 
                      control=list(max_treedepth=14))
whoPubModel

###############################################################################
### Test only means                                                         ###
###############################################################################
modelDataMeansOnly <- modelData %>% filter(mean == 1)
stanModelMeansOnlyData <- list(
    N = length(modelDataMeansOnly$cost),
    cost = modelDataMeansOnly$cost + 0.01,
    # se = rep(0, length(modelDataMeansOnly$cost)),
    se = as.numeric(modelDataMeansOnly$se)+ 0.01,
    dme_prediag = modelDataMeansOnly$dme.prediag, 
    dnme_prediag =  modelDataMeansOnly$dnme.prediag, 
    ie_prediag =  modelDataMeansOnly$ie.prediag, 
    dme_intsvtx = modelDataMeansOnly$dme.intsvtx, 
    dnme_intsvtx =  modelDataMeansOnly$dnme.intsvtx, 
    ie_intsvtx =  modelDataMeansOnly$ie.intsvtx,
    dme_contntx = modelDataMeansOnly$dme.contntx, 
    dnme_contntx =  modelDataMeansOnly$dnme.contntx, 
    ie_contntx =  modelDataMeansOnly$ie.contntx, 
    drtb=modelDataMeansOnly$drtb,
    urban=modelDataMeansOnly$urban, 
    pcGDP = modelDataMeansOnly$pcGDP
)

###############################################################################
##### Call the stan model.                                                #####
###############################################################################
totalModelMeansOnly <- stan(file = here("code/inst/tbPatientCosts.stan"),
                            data = stanModelMeansOnlyData,
                            iter = 1000, chains = 3,
                            control=list(max_treedepth=14))
totalModelMeansOnly
#### Empirical ################################################################


ggplot() + geom_point(data=modelDataMeans, aes(x=costs))

modelData <- modelData %>% filter(cost < 25000)
plot(modelData$cost)

sd(modelData$cost)
mean(as.numeric(fullData$dme.prediag.total.mean), na.rm = T)

mean(as.numeric(fullData$dmne.prediag.total.mean), na.rm = T)

mean(as.numeric(fullData$ie.prediag.lostincome.total.mean), na.rm = T)
