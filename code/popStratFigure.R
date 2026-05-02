### counts of population stratifications over time 
library(ggplot2)
library(dplyr)
desc <- dataList$descriptive

broadPopID <- dataList$data %>% 
              mutate(pop.id.broad = case_when(grepl("HIV", pop.id) ~ "HIV", 
                                              grepl("DRTB", pop.id) ~ "Drug-resistance", 
                                              grepl("DSTB", pop.id) ~ "Drug-resistance",
                                              grepl("Priv", pop.id) ~ "Public/Private",
                                              grepl("Pub", pop.id) ~ "Public/Private",
                                              grepl("Urb", pop.id) ~ "Urbanicity",
                                              grepl("Rural", pop.id) ~ "Urbanicity", 
                                              grepl("EP", pop.id) ~ "Location of TB",
                                              grepl("PTB", pop.id) ~ "Location of TB",
                                              grepl("^Male", pop.id) ~ "Sex",
                                              grepl("Female", pop.id) ~ "Sex", 
                                              grepl("SAT", pop.id) ~ "SAT/DOT",
                                              grepl("DOT", pop.id) ~ "SAT/DOT", 
                                              grepl("dot", pop.id) ~ "SAT/DOT", 
                                              grepl("ACF", pop.id) ~ "ACF/PCF",
                                              grepl("PCF", pop.id) ~ "ACF/PCF", 
                                              grepl("TxNaive", pop.id) ~ "TB history",
                                              grepl("ReTx", pop.id) ~ "TB history", 
                                              grepl("riorTB", pop.id) ~ "TB history", 
                                              grepl("nten", pop.id) ~ "Treatment\nstage",
                                              grepl("Cont", pop.id) ~ "Treatment\nstage", 
                                              grepl("Month", pop.id) ~ "Treatment\nstage", 
                                              grepl("osp", pop.id) ~ "Hospital stay\nrequired", 
                                              grepl("DIAB", pop.id) ~ "Diabetes status", 
                                              grepl("HR", pop.id) ~ "High risk\npopulation", 
                                              grepl("GenPop", pop.id) ~ "High risk\npopulation"
                    ))

broadPopID <- broadPopID %>% filter(pop.id !="Total") %>%
    select(pop.id, pop.id.broad, covidence.id) %>% 
    left_join(desc) %>% 
    select(pop.id, pop.id.broad, study.start.year, covidence.id) 

# View(broadPopID)

 ## Need to scale by the number of studies published that year
nPopID <- broadPopID %>% 
          select(!pop.id) %>% 
          group_by(study.start.year, covidence.id) %>%
          unique() %>% 
          count(pop.id.broad) %>% 
          rename(popN = n)

nTotalSurveysYear <-  desc %>% ungroup() %>%
                       reframe(TotalSurveys = n(), 
                       .by = c(study.start.year))

fullGrid <- expand.grid(pop.id.broad = unique(nPopID$pop.id.broad), 
                        study.start.year = 2010:2023)

nPopID <- nPopID %>% left_join(nTotalSurveysYear) %>% 
          full_join(fullGrid) %>% 
          mutate(`Fraction of\nsurveys` = ifelse(is.na(TotalSurveys),
                                                 0,
                                                 popN/TotalSurveys)) 
            


ggplot(nPopID %>% filter(!is.na(pop.id.broad)), 
       aes(x = study.start.year, y = pop.id.broad,  
                        fill = `Fraction of\nsurveys`)) + 
    geom_tile() + ylab("")+ xlab("Survey start year") + 
    theme_minimal(base_size = 16) + 
    scale_x_continuous(breaks = seq(2010, 2023, 1)) + 
    scale_y_discrete(limits = rev) + 
    theme(legend.position = "bottom",
          legend.key.width = unit(3, "line")) + 
    scale_fill_gradient(high="lightblue",low= "dodgerblue4")
    
