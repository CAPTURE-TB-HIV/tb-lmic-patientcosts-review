a <- spread_draws(totalModel,
                  dstb_dme_prediag_cost_country[country.id.new],
                  dstb_dnme_prediag_cost_country[country.id.new],
                  dstb_ie_prediag_cost_country[country.id.new],
                  dstb_dme_intsvtx_cost_country[country.id.new],
                  dstb_dnme_intsvtx_cost_country[country.id.new],
                  dstb_ie_intsvtx_cost_country[country.id.new],
                  dstb_dme_contntx_cost_country[country.id.new],
                  dstb_dnme_contntx_cost_country[country.id.new],
                  dstb_ie_contntx_cost_country[country.id.new],
                  dstb_total_cost_country[country.id.new],
                  ndraws = 500, seed = tbPtntCstSeed) %>% 
    filter(!country.id.new %in% which(modelData$cost == 0)) %>% 
    group_by(country.id.new) %>% 
    median_qi(dstb_dme_prediag_cost_country, 
              dstb_dnme_prediag_cost_country, 
              dstb_ie_prediag_cost_country,
              dstb_dme_intsvtx_cost_country, 
              dstb_dnme_intsvtx_cost_country, 
              dstb_ie_intsvtx_cost_country,
              dstb_dme_contntx_cost_country, 
              dstb_dnme_contntx_cost_country, 
              dstb_ie_contntx_cost_country,
              dstb_total_cost_country) %>% select(! c(.width, .point, .interval)) %>% 
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country.upper, names_to="parameter") %>% 
    group_by(country.id.new) %>% 
    mutate(model = "Zeroes included",
           # country.id.old = country.id.new,
           country.id.new = cur_group_id()) 

b <- spread_draws(totalModelNoZero,
                  dstb_dme_prediag_cost_country[country.id.new],
                  dstb_dnme_prediag_cost_country[country.id.new],
                  dstb_ie_prediag_cost_country[country.id.new],
                  dstb_dme_intsvtx_cost_country[country.id.new],
                  dstb_dnme_intsvtx_cost_country[country.id.new],
                  dstb_ie_intsvtx_cost_country[country.id.new],
                  dstb_dme_contntx_cost_country[country.id.new],
                  dstb_dnme_contntx_cost_country[country.id.new],
                  dstb_ie_contntx_cost_country[country.id.new],
                  dstb_total_cost_country[country.id.new],
                  ndraws = 500, seed = tbPtntCstSeed) %>% 
    group_by(country.id.new) %>% 
    median_qi(dstb_dme_prediag_cost_country, 
              dstb_dnme_prediag_cost_country, 
              dstb_ie_prediag_cost_country,
              dstb_dme_intsvtx_cost_country, 
              dstb_dnme_intsvtx_cost_country, 
              dstb_ie_intsvtx_cost_country,
              dstb_dme_contntx_cost_country, 
              dstb_dnme_contntx_cost_country, 
              dstb_ie_contntx_cost_country,
              dstb_total_cost_country) %>% select(! c(.width, .point, .interval)) %>% 
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country.upper, names_to="parameter") %>% 
    mutate(model = "No zeroes") 

c <- spread_draws(totalModelHurdle,
                  dstb_dme_prediag_cost_country[country.id.new],
                  dstb_dnme_prediag_cost_country[country.id.new],
                  dstb_ie_prediag_cost_country[country.id.new],
                  dstb_dme_intsvtx_cost_country[country.id.new],
                  dstb_dnme_intsvtx_cost_country[country.id.new],
                  dstb_ie_intsvtx_cost_country[country.id.new],
                  dstb_dme_contntx_cost_country[country.id.new],
                  dstb_dnme_contntx_cost_country[country.id.new],
                  dstb_ie_contntx_cost_country[country.id.new],
                  dstb_total_cost_country[country.id.new],
                  ndraws = 500, seed = tbPtntCstSeed) %>% 
    filter(!country.id.new %in% which(modelData$cost == 0)) %>% 
    group_by(country.id.new) %>% 
    median_qi(dstb_dme_prediag_cost_country, 
              dstb_dnme_prediag_cost_country, 
              dstb_ie_prediag_cost_country,
              dstb_dme_intsvtx_cost_country, 
              dstb_dnme_intsvtx_cost_country, 
              dstb_ie_intsvtx_cost_country,
              dstb_dme_contntx_cost_country, 
              dstb_dnme_contntx_cost_country, 
              dstb_ie_contntx_cost_country,
              dstb_total_cost_country) %>% select(! c(.width, .point, .interval)) %>% 
    pivot_longer(dstb_dme_prediag_cost_country:dstb_total_cost_country.upper, names_to="parameter") %>%
    group_by(country.id.new) %>% 
    mutate(model = "Hurdle",
           # country.id.old = country.id.new,
           country.id.new = cur_group_id()) 

aWide <- data.frame(a[seq(1,nrow(a)-2, 3), c(2, 4)], 
                    median = unlist(a[seq(1,nrow(a)-2, 3), 3]),
                    lower = unlist(a[seq(2,nrow(a)-1, 3), 3]), 
                    upper = unlist(a[seq(3,nrow(a), 3), 3])) 

# aWide$covidence.id <- rep(modelData$covidence.id[-which(modelData$cost == 0)], each=10)

aWide$figure.id <- rep(paste(modelData$study.country, modelData$study.end.year)[-which(modelData$cost == 0)], each=10)
         
aWide <- aWide %>% distinct()

bWide <- data.frame(b[seq(1,nrow(b)-2, 3), c(2, 4)], 
                    median = unlist(b[seq(1,nrow(b)-2, 3), 3]),
                    lower = unlist(b[seq(2,nrow(b)-1, 3), 3]), 
                    upper = unlist(b[seq(3,nrow(b), 3), 3])) 

bWide$figure.id <- rep(paste(modelDataNoZeros$study.country, modelDataNoZeros$study.end.year), each=10)

bWide <- bWide %>% distinct()

cWide <- data.frame(c[seq(1,nrow(c)-2, 3), c(2, 4)], 
                    median = unlist(c[seq(1,nrow(c)-2, 3), 3]),
                    lower = unlist(c[seq(2,nrow(c)-1, 3), 3]), 
                    upper = unlist(c[seq(3,nrow(c), 3), 3])) 
cWide$figure.id <- rep(paste(modelData$study.country, modelData$study.end.year)[-which(modelData$cost == 0)], each=10)
cWide <- cWide %>% distinct()


all <- rbind(aWide, bWide, cWide) %>% 
        mutate(parameter = gsub("_cost_country", "", parameter)) %>% 
       arrange(figure.id) 

# saveRDS(all, "~/Desktop/allHurdleTests.rds", version =2)

# all <- readRDS("~/Desktop/allHurdleTests.rds")

all$pageNo <- c(rep(1:6, each=360),
                rep(7, length.out=180))

# all$pageNo <- rep(1:12, each=360,)

for(i in 1:7){

    p <- ggplot(data=all %>% filter(pageNo==i)) + 
    geom_point(aes(y=parameter, x=median, group=model, color=model), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(y=parameter, xmin=lower, xmax=upper, 
                      group=model, color=model), position = position_dodge(width = 1)) + 
    theme_minimal() + 
    facet_wrap(facets=vars(figure.id)) + 
    theme(legend.position = "bottom") + 
    scale_x_log10()
    
    ggsave(plot = p, filename = here(paste0('output/hurdleModelComp-page_', i, '.pdf')), 
           width=11, height=8.5, units = "in")
    
}
