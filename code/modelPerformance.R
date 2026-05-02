library(ggplot2)

# samples <- as.data.frame(rstan::extract(totalModel)) %>% 
#            mutate(slope = )
# 
# ggplot(samples) +
#     geom_abline(aes(intercept = 0, slope = slope), color = "cadetblue4", alpha = 0.20) +
#     geom_abline(aes(intercept = 0, slope = mean(slope)), color = "black", size = 2) +
#     scale_x_continuous(limits = c(-1, 1)) 

modelDataPlot <- modelData %>% 
                 ungroup() %>%
                 mutate(id = 1:n())

# modelDataMeansOnlyPlot <- modelDataMeansOnly %>% 
#                  ungroup() %>%
#                  mutate(id = 1:n())
###############################################################################
posteriorMeans <- get_posterior_mean(totalModel)[,4]
# posteriorMeansX <- get_posterior_mean(totalModelMeansOnly)[,4]

# modelDataPred <- modelDataPlot %>% 
#                  ungroup() %>% 
#                  mutate(predCost = posteriorMeans["coef_dme_prediag"] * dme.prediag +  
#                                    posteriorMeans["coef_dnme_prediag"] * dnme.prediag +
#                                    posteriorMeans["coef_ie_prediag"] * ie.prediag +
#                                    posteriorMeans["coef_dme_intsvtx"] * dme.intsvtx +  
#                                    posteriorMeans["coef_dnme_intsvtx"] * dnme.intsvtx +
#                                    posteriorMeans["coef_ie_intsvtx"] * ie.intsvtx +
#                                    posteriorMeans["coef_dme_contntx"] * dme.contntx +  
#                                    posteriorMeans["coef_dnme_contntx"] * dnme.contntx +
#                                    posteriorMeans["coef_ie_contntx"] * ie.contntx +
#                                    exp(posteriorMeans["coef_drtb"] * drtb)
#                         )
# 
# modelDataPred$error <- posteriorMeans[(which(names(posteriorMeans) == "add_var[1]")):
#                                       (which(names(posteriorMeans) == "add_var[1]")+nrow(modelDataPred)-1)]

modelDataPlot$stanEst <- posteriorMeans[(which(names(posteriorMeans) == "mu[1]")):
                                        (which(names(posteriorMeans) == "mu[1]")+nrow(modelDataPlot)-1)]
# modelDataMeansOnlyPlot$stanEst <- posteriorMeansX[(which(names(posteriorMeansX) == "mu[1]")):
#                                             (which(names(posteriorMeansX) == "mu[1]")+nrow(modelDataMeansOnlyPlot)-1)]

# modelDataPred$predCost <- modelDataPred$predCost + modelDataPred$error

# ggplot() + geom_point(data=modelDataPlot, 
#                       aes(x=id, y=cost)) + 
#            geom_point(data=modelDataPred, 
#                       aes(x=id, y=predCost), alpha = .5, color = "salmon", size=2) + 
#            geom_point(data=modelDataPred, 
#                aes(x=id, y=predCost), alpha = .5, color = "blue", size=1) + 
#            theme_minimal()

# ggplot() + geom_point(data=modelDataPlot, 
#                       aes(x=id, y=cost)) + 
    # geom_point(data=modelDataPred, 
    #            aes(x=id, y=predCost), alpha = .5, color = "salmon", size=2) + 
    # geom_point(data=modelDataPlot, 
    #            aes(x=id, y=stanEst), alpha = .5, color = "blue", size=1) + 
    # theme_minimal()

ggplot() + 
    # geom_point(data=modelDataPlot, 
    #                   aes(x=id, y=cost)) + 
    # geom_point(data=modelDataPred, 
    #            aes(x=id, y=predCost), alpha = .5, color = "salmon", size=2) + 
    geom_point(data=modelDataPlot, 
               aes(x=id, y=cost-stanEst, color=as.factor(mean)), alpha = .5, size=1) + 
    theme_minimal()

# ggplot() + 
    # geom_point(data=modelDataPlot, 
    #                   aes(x=id, y=cost)) + 
    # geom_point(data=modelDataPred, 
    #            aes(x=id, y=predCost), alpha = .5, color = "salmon", size=2) + 
    # geom_point(data=modelDataMeansOnlyPlot, 
    #            aes(x=id, y=cost-stanEst), alpha = .5, size=1) + 
    # theme_minimal()

PPcheck <- as.data.frame(rstan::extract(totalModel)) %>%
           select(contains("y_rep")) %>%
           mutate(id=1:n())

colnames(PPcheck) <- c(paste0("cost", 2:(ncol(PPcheck))-1), "id")

ggplot(data=PPcheck) + geom_point(aes(x=id, y=cost1)) + theme_minimal() + 
     geom_hline(aes(yintercept = as.numeric(modelData[1,"cost"])), color="red") + 
    scale_y_log10()

ggplot(data=PPcheck) + geom_point(aes(x=id, y=cost2)) + theme_minimal() + 
    geom_hline(aes(yintercept = as.numeric(modelData[2,"cost"])), color="red")+ 
    scale_y_log10()

ggplot(data=PPcheck) + geom_point(aes(x=id, y=cost3)) + theme_minimal() + 
    geom_hline(aes(yintercept = as.numeric(modelData[3,"cost"])), color="red")+ 
    scale_y_log10()

ggplot(data=PPcheck) + geom_point(aes(x=id, y=cost4)) + theme_minimal() + 
    geom_hline(aes(yintercept = as.numeric(modelData[4,"cost"])), color="red")+ 
    scale_y_log10()
