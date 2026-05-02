
ggplot(data=dmeContnTxCompare,
       aes(x=study.start.year, 
           y=modelEstDME)) + geom_point() + 
    # scale_y_log10() + 
    theme_minimal(base_size = 16) +
    geom_smooth(method="glm") +
    ylab("Direct medical costs") + 
    xlab("Study start year") + 
    ggtitle("Continuation treatment")

ggplot(data=dmeIntsvTxCompare,
       aes(x=study.start.year, 
           y=modelEstDME)) + geom_point() + 
    # scale_y_log10() + 
    theme_minimal(base_size = 16) +
    geom_smooth(method="glm") +
    ylab("Direct medical costs") + 
    xlab("Study start year") + 
    ggtitle("Intensive treatment")

ggplot(data=dmePrediagCompare,
       aes(x=study.start.year, 
           y=modelEst)) + geom_point() + 
    # scale_y_log10() + 
    theme_minimal(base_size = 16) +
    geom_smooth(method="gam")+
    ylab("Direct medical costs") + 
    xlab("Study start year") + 
    ggtitle("Diagnosis")

ggplot() + 
    geom_point(data=dmeIntsvTxCompare,
               aes(x=study.start.year, 
                   y=modelEstDME)) + 
    geom_point(data=dmeContnTxCompare,
               aes(x=study.start.year, 
                   y=modelEstDME), color="red") 

ggplot() + geom_point(data = dataFull, 
                      aes(x=publication.year, 
                          y=cost, 
                          color = category)) + 
    theme_minimal() + 
    facet_wrap(facets = vars(study.country), scales = "free")

ggplot() + geom_point(data = dataFull, 
                      aes(x=publication.year, 
                          y=cost, 
                          color = study.country)) + 
    theme_minimal() + 
    facet_wrap(facets = vars(category), scales = "free")
