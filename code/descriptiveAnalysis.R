library(ggplot2)
modelData %>% 
    ungroup() %>% 
    filter(drtb == 1, 
           dme.prediag==1) %>% 
    select(cost) %>%
    range()

modelData %>% 
    ungroup() %>% 
    filter(drtb != 1, 
           dme.prediag==1) %>% 
    select(cost) %>%
    range()

countryYearNat <- cleanDF %>% 
           mutate(natRep = ifelse(grepl("ational", study.setting), 
                                  "National survey", "Subnational survey")) %>%
           group_by(study.country, study.start.year, natRep) %>% 
           count() %>% 
           rename(`Number of surveys` = n)
           
    
ggplot(countryYearNat %>% 
       mutate(study.country=case_when(study.country=="Vietnam" ~ "Viet Nam", 
                                      study.country=="Democratic Republic of Congo" ~ "DR Congo", 
                                      TRUE ~ study.country
              ))) + geom_point(aes(x=study.start.year, 
                                 y=study.country, 
                                 color = natRep, 
                                 size=`Number of surveys`)
                                 ) + 
    
    # position_dodge(width=1) +
    labs(color=NULL)+
    theme_minimal(base_size = 16) + 
    scale_y_discrete(limits = rev) + 
    scale_size_continuous(range = c(3,7)) + 
    xlab("Survey start year") + 
    ylab("") + 
    theme(
          # legend.position = "right",
          legend.box="vertical", 
          legend.position = c(0.925, 0.2),
          legend.background = element_rect(fill = "white", color="black"),
          # legend.title = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(2010, 2023, 1)) + 
    scale_color_discrete(type = myPal)
    
