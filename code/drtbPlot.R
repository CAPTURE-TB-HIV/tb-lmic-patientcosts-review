drTBData <- 
fullData %>% 
    group_by(covidence.id) %>% 
    mutate("Resistance reporting" = 
               case_when(length(unique(pop.id))>1 & 
                        ("DRTB" %in% unique(pop.id) & 
                         "DSTB" %in% unique(pop.id)) ~ "Reports both stratified costs", 
                         
                        ("DRTB" %in% unique(pop.id) | 
                         "DSTB" %in% unique(pop.id) | 
                         grepl("all DS-TB|all DSTB|DSTB only|DS-TB only", drtb.desc)) ~ "Reports one of stratified costs",
                        dstb == 1 ~ "Study excluded DRTB", 
                        drtb == 1 ~ "Study excluded DSTB", 
                        .default = "No detailed reporting")) 

library(ggplot2)

ggplot() + 
    geom_tile(data = drTBData, aes(x=covidence.id, y=`Resistance reporting`, fill=natRep)) + 
    theme_minimal() + coord_flip() + 
    theme(legend.title = element_blank()) +
    ylab("") + xlab("Study")


drTBData %>% 
    filter(`Resistance reporting` == "No detailed reporting") %>%
    select(covidence.id) %>% unique() %>% nrow()

drTBData %>% 
    filter(`Resistance reporting` == "Reports both stratified costs") %>%
    select(covidence.id) %>% unique() %>% nrow()

drTBData %>% 
    filter(`Resistance reporting` == "Reports one of stratified costs") %>%
    select(covidence.id) %>% unique() %>% nrow()

drTBData %>% 
    filter(`Resistance reporting` == "Study excluded DRTB") %>%
    select(covidence.id) %>% unique() %>% nrow()

drTBData %>% 
    filter(`Resistance reporting` == "Study excluded DSTB") %>%
    select(covidence.id) %>% unique() %>% nrow()



