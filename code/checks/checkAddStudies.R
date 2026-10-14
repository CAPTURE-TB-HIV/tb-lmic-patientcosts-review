##### How many survey have all reported costs #####

fullDataSurveyID <-modelData %>% 
    # select(covidence.id, pdme.prediag , dnme.prediag , ie.prediag , 
    #            dme.intsvtx , dnme.intsvtx , ie.intsvtx , 
    #            dme.contntx , dnme.contntx , ie.contntx) %>% 
    ungroup() %>%
    reframe(numbCosts = sum(dme.prediag, dnme.prediag, ie.prediag, 
                        dme.intsvtx, dnme.intsvtx, ie.intsvtx,
                        dme.contntx, dnme.contntx, ie.contntx), .by=c(covidence.id, pop.id)) %>% 
    filter(numbCosts == 9) %>% 
    select(covidence.id) %>% unlist()
            
length(fullDataSurveyID)

modelData %>% filter(covidence.id %in% fullDataSurveyID) %>% 
    ungroup() %>% 
    summarize("Total costs" = sum(cost), .by=c(covidence.id, pop.id)) %>% View()

incCovIds <- modelData %>% select(covidence.id) %>% unique() %>% unlist() %>% as.vector()
length(incCovIds)

dataList$descriptive %>% filter(!covidence.id %in% incCovIds) %>% View() #write.csv("~/Desktop/addtnlStudies.csv")
