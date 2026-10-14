mk_frst_plot <- function(modelFit, 
                         emprclData, 
                         filename) {
    library(forcats)
    library(rstan)
    library(dplyr)
    library(ggplot2)
    ### Extract the mean model estimates from all chains as this is the 
    ### value we are going to plot 
    modelEstimates <- get_posterior_mean(modelFit)[,"mean-all chains"]

    ##########################################################################|
    #### Diagnostic Costs ##################################################### 
    ##########################################################################|
    #### Direct Medical Costs

    dme.prediag.index <- which(emprclData$dme.prediag == 1)
    prediagCosts <- emprclData %>% filter(dme.prediag == 1)
    prediagEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dme.prediag.index]     

    ### Create a dataframe of modeled and reported costs
    dmePrediagCompare <- data.frame(prediagCosts, 
                                   modelEst = prediagEstimate) %>%
                   ungroup() %>%
                  mutate(id = 1:n(), 
                         id = as.factor(id)) %>% 
    mutate(label = paste(id, study.country), 
           label= fct_reorder(label, modelEst)) %>%
    left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
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
                                                 dme.contntx + dnme.contntx) == 1 ~ "Only pre-diagnostic DM costs")) 

    dmePreDiagPlot <- ggplot(data=dmePrediagCompare) + 
                      geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
                      geom_point(aes(y=label, x=modelEst)) + 
                      theme_minimal() + 
                      ggtitle("Pre-diagnostic direct medical costs") +
                      scale_x_log10() + 
        theme(legend.position = "bottom"); dmePreDiagPlot
    
    ### Print the median of the model estimated mean cost
    round(median(dmePrediagCompare$modelEst))

    ##########################################################################|
    #### Direct Non-Medical Costs
    dnme.prediag.index <- which(emprclData$dnme.prediag == 1)
    prediagCosts <- emprclData %>% filter(dnme.prediag == 1)
    prediagEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dnme.prediag.index]     
    
    dnmePrediagCompare <- data.frame(prediagCosts, 
                                      modelEst = prediagEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>% 
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEst)) %>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year))  %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 6 ~ "Total TB care direct costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 3 ~ "Total TB care DNM costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 2 ~ "Total pre-diagnostic direct costs", 
                                                (dme.prediag + dnme.prediag + 
                                                     dme.intsvtx + dnme.intsvtx +
                                                     dme.contntx + dnme.contntx) == 1 ~ "Only pre-diagnostic DNM costs")) 
    
    
    round(median(dnmePrediagCompare$modelEst))
    
    dnmePreDiagPlot <- ggplot(data=dnmePrediagCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEst)) + 
        theme_minimal() + 
        ggtitle("Pre-diagnostic direct non-medical costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    ##########################################################################|
    ### Indirect Costs
    ie.prediag.index <- which(emprclData$ie.prediag == 1)
    prediagCosts <- emprclData %>% filter(ie.prediag == 1)
    prediagEstimate <- modelEstimates[grep("mu", names(modelEstimates))][ie.prediag.index]     
    
    iePrediagCompare <- data.frame(prediagCosts, 
                                   modelEst = prediagEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>% 
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEst))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((ie.prediag + ie.intsvtx + ie.contntx) == 3 ~ "Total TB care indirect costs", 
                                                (ie.prediag + ie.intsvtx + ie.contntx) == 1 ~ "Only pre-diagnostic indirect costs")) 
    
    
    round(median(iePrediagCompare$modelEst))
    
    iePreDiagPlot <- ggplot(data=iePrediagCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEst)) +
        theme_minimal() + 
        ggtitle("Pre-diagnostic indirect costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")

    ##########################################################################|
    #### Intensive Treatment Costs ############################################
    ##########################################################################|
    dme.intsvtx.index <- which(emprclData$dme.intsvtx == 1)
    dmeIntsvTxCosts <- emprclData %>% filter(dme.intsvtx == 1)
    dmeIntsvTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dme.intsvtx.index]     
    
    ### Create a dataframe of modeled and reported costs
    dmeIntsvTxCompare <-data.frame(dmeIntsvTxCosts, 
                                   modelEstDME = dmeIntsvTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>%
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstDME))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((dme.prediag + dme.intsvtx + dme.contntx) == 3 ~ "Total TB care DM costs", 
                                                (dme.prediag + dme.intsvtx + dme.contntx) == 2 ~ "Total TB treatment DM costs", 
                                                (dme.prediag + dme.intsvtx + dme.contntx) == 1 ~ "Only intensive treatment DM costs")) 
    
    dmeIntsvTxPlot <- ggplot(data=dmeIntsvTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstDME)) +
        theme_minimal() + 
        ggtitle("Intensive treatment direct non-medical costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    round(median(dmeIntsvTxCompare$modelEstDME))
    
    ##########################################################################|
    #### Direct Non-Medical Costs
    dnme.intsvtx.index <- which(emprclData$dnme.intsvtx == 1)
    dnmeIntsvTxCosts <- emprclData %>% filter(dnme.intsvtx == 1)
    dnmeIntsvTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dnme.intsvtx.index]     
    
    dnmeIntsvTxCompare <- data.frame(dnmeIntsvTxCosts, 
                                     modelEstDNME = dnmeIntsvTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id))%>%
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstDNME))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((dnme.prediag + dnme.intsvtx + dnme.contntx) == 3 ~ "Total TB care DNM costs", 
                                                (dnme.prediag + dnme.intsvtx + dnme.contntx) == 2 ~ "Total TB treatment DNM costs", 
                                                (dnme.prediag + dnme.intsvtx + dnme.contntx) == 1 ~ "Only intensive treatment DNM costs")) 
    
    
    dnmeIntsvTxPlot <- ggplot(data=dnmeIntsvTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstDNME)) +
        theme_minimal() + 
        ggtitle("Intensive treatment direct non-medical costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    
    
    round(median(dnmeIntsvTxCompare$modelEstDNME))
    ##########################################################################|
    ### Indirect Costs
    ie.intsvtx.index <- which(emprclData$ie.intsvtx == 1)
    ieIntsvTxCosts <- emprclData %>% filter(ie.intsvtx == 1)
    ieIntsvTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][ie.intsvtx.index]     
    
    ieIntsvTxCompare <- data.frame(ieIntsvTxCosts, 
                                   modelEstIE = ieIntsvTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>% 
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstIE))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((ie.prediag + ie.intsvtx + ie.contntx) == 3 ~ "Total TB care indirect costs", 
                                                (ie.prediag + ie.intsvtx + ie.contntx) == 2 ~ "Total TB treatment indirect costs", 
                                                (ie.prediag + ie.intsvtx + ie.contntx) == 1 ~ "Only intensive treatment indirect costs")) 
    
    
    ieIntsvTxPlot <- ggplot(data=ieIntsvTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstIE)) +
        theme_minimal() + 
        ggtitle("Intensive treatment indirect costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    
    
    round(median(ieIntsvTxCompare$modelEstIE))
    
    ##########################################################################|
    #### Continuation Treatment Costs #########################################
    ##########################################################################|
    #### Direct Medical Costs
    dme.contntx.index <- which(emprclData$dme.contntx == 1)
    dmeContnTxCosts <- emprclData %>% filter(dme.contntx == 1)
    dmeContnTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dme.contntx.index]     
    
    dmeContnTxCompare <- data.frame(dmeContnTxCosts, 
                                    modelEstDME = dmeContnTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>%
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstDME))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((dme.prediag + dme.intsvtx + dme.contntx) == 3 ~ "Total TB care DM costs", 
                                                (dme.prediag + dme.intsvtx + dme.contntx) == 2 ~ "Total TB treatment DM costs", 
                                                (dme.prediag + dme.intsvtx + dme.contntx) == 1 ~ "Only continuation treatment DM costs")) 
    
    dmeContnTxPlot <- ggplot(data=dmeContnTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstDME)) +
        theme_minimal() + 
        ggtitle("Continuation treatment direct non-medical costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    round(median(dmeContnTxCompare$modelEstDME))
    
    ##########################################################################|
    #### Direct Non-Medical Costs
    dnme.contntx.index <- which(emprclData$dnme.contntx == 1)
    dnmeContnTxCosts <- emprclData %>% filter(dnme.contntx == 1)
    dnmeContnTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][dnme.contntx.index]     
    
    dnmeContnTxCompare <- data.frame(dnmeContnTxCosts, 
                                     modelEstDNME = dnmeContnTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id))%>%
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstDNME))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((dnme.prediag + dnme.intsvtx + dnme.contntx) == 3 ~ "Total TB care DNM costs", 
                                                (dnme.prediag + dnme.intsvtx + dnme.contntx) == 2 ~ "Total TB treatment DNM costs", 
                                                (dnme.prediag + dnme.intsvtx + dnme.contntx) == 1 ~ "Only continuation treatment DNM costs")) 
    
    
    dnmeContnTxPlot <- ggplot(data=dnmeContnTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstDNME)) +
        theme_minimal() + 
        ggtitle("Continuation treatment direct non-medical costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    
    round(median(dnmeContnTxCompare$modelEstDNME))
    
    #### Indirect Non-Medical Costs
    ie.contntx.index <- which(emprclData$ie.contntx == 1)
    ieContnTxCosts <- emprclData %>% filter(ie.contntx == 1)
    ieContnTxEstimate <- modelEstimates[grep("mu", names(modelEstimates))][ie.contntx.index]     
    
    ieContnTxCompare <- data.frame(ieContnTxCosts, 
                                   modelEstIE = ieContnTxEstimate) %>%
        ungroup() %>%
        mutate(id = 1:n(), 
               id = as.factor(id)) %>% 
        mutate(label = paste(id, study.country), 
               label= fct_reorder(label, modelEstIE))%>%
        left_join(dataList$descriptive %>% select(covidence.id, study.end.year)) %>% 
        ## Label empirical cost types 
        mutate("Reported cost type" = case_when((ie.prediag + ie.intsvtx + ie.contntx) == 3 ~ "Total TB care indirect costs", 
                                                (ie.prediag + ie.intsvtx + ie.contntx) == 2 ~ "Total TB treatment indirect costs", 
                                                (ie.prediag + ie.intsvtx + ie.contntx) == 1 ~ "Only continuation treatment indirect costs")) 
               
    
    ieContnTxPlot <- ggplot(data=ieContnTxCompare) + 
        geom_point(aes(y=label, x=cost, color = `Reported cost type`), alpha=0.7, size = 3) + 
        geom_point(aes(y=label, x=modelEstIE)) +
        theme_minimal() + 
        ggtitle("Continuation treatment indirect costs") +
        scale_x_log10() + 
        theme(legend.position = "bottom")
    
    
    round(median(ieContnTxCompare$modelEstIE))
    
    ##########################################################################|
    #### Save to PDF for review  ##############################################
    ##########################################################################|
    saveName <- here(filename)
    pdf(file = saveName, height = 11.5,  width= 8.5)
    print(dmePreDiagPlot)
    print(dnmePreDiagPlot)
    print(iePreDiagPlot)
    print(dmeIntsvTxPlot)
    print(dnmeIntsvTxPlot)
    print(ieIntsvTxPlot)
    print(dmeContnTxPlot)
    print(dnmeContnTxPlot)
    print(ieContnTxPlot)
    dev.off()
}

