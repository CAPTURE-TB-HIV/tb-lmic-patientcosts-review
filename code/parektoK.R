pk_ind <- which(loo::pareto_k_values(loo(model7)) > 1) 

modelData[pk_ind,] 

modelData %>% filter(covidence.id %in% unlist(modelData[pk_ind,"covidence.id"])) %>% View()
