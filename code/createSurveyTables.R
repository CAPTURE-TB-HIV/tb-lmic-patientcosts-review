survey_tables <- function(modelDataMeans, 
                          modelDataMedians, 
                          dataDesc){ 
    
    #### Create a single dataframe from the above objects
    
    allModelData <- rbind(modelDataMeans, 
                          modelDataMedians)
    
    allModelData <- left_join(allModelData, dataDesc)
    
    
    ### Used in the pre-diag dme 
    dmePreDiagData <- 
    allModelData %>% 
        filter(dme.prediag==1) %>% 
        mutate("Reported cost type" = case_when((dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 6 ~ "Total TB care direct costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 3 ~ "Total TB care DM costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 2 ~ "Total pre-diagnostic direct costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 1 ~ "Only pre-diagnostic DM costs")
              ) %>%
        select(covidence.id, 
               study.country, 
               study.years,
               `Reported cost type`,
               pop.id,
               cost,
               study.doi)
    
    write.csv(dmePreDiagData, here("output/descriptive/prediag_dme_data.csv"))
    
    }