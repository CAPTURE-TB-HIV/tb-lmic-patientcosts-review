# bleh <- newDF %>% 
#     group_by(`Covidence..`) %>% 
#     summarise(N.pops.reported = n()) %>% 
#     filter(N.pops.reported ==1) %>% select(Covidence..)
# 
# newDF <- newDF %>%  
#     mutate(pop.id = ifelse(`Covidence..` %in% as.vector(unlist(bleh)), 
#                            "Total", 
#                            pop.id)) 
#     
#     
# View(newDF %>% select(`Covidence..`, pop.id))
# 
# newDF %>% filter(pop.id == "Total") %>% 
#     # select(`Covidence..`, Male.Number.of.persons) %>% 
#     summarise(N.males = sum(Male.Number.of.persons, na.rm=TRUE), 
#               N.females =  sum(Female.Number.of.persons, na.rm=TRUE), 
#               N.DSTB = sum(DSTB.Number.of.people, na.rm=TRUE),
#               N.DRTB = sum(DRTB.Number.of.people, na.rm=TRUE),
#               N.EPTB = sum(Extra.pulmonary.TB.Number.of.persons, na.rm = TRUE),
#               N.SAT = sum(SAT.All.treatment, na.rm = TRUE),
#               N.DOT = sum(DOT.All.treatment, na.rm = TRUE),
#               N.HIV = sum(HIV.positive.Number.of.people, na.rm = TRUE))
# 
# saveRDS(newDF, here("data/tempLineListData.rds"), version = 2)
# 
# ### Total direct expenses 
# sum(!(is.na(rawData$Total.TB.Care.Mean) & is.na(rawData$Total.TB.Care.Median)))
# sum(!(is.na(rawData$Pre.diagnosis.Mean) & is.na(rawData$Pre.diagnosis.Median)))
# sum(!(is.na(rawData$Post.diagnosis.Mean) & is.na(rawData$Post.diagnosis.Median)))
# sum(!(is.na(rawData$Intensive.treatment.Mean) & is.na(rawData$Intensive.treatment.Median)))
# sum(!(is.na(rawData$Continuation.treatment.Mean) & is.na(rawData$Continuation.treatment.Median)))
# sum(!(is.na(rawData$Post.treatment.completion.Mean) & is.na(rawData$Post.treatment.completion.Median)))
# sum(!(is.na(rawData$Full.TB.episode.Mean) & is.na(rawData$Full.TB.episode.Median)))
# 
# 
# ### Total DME
# sum(!(is.na(rawData$Total.Mean) & is.na(rawData$Total.Median)))
# sum(!(is.na(rawData$Total.Mean.1) & is.na(rawData$Total.Median.1)))
# sum(!(is.na(rawData$Total.Mean.2) & is.na(rawData$Total.Median.2)))
# sum(!(is.na(rawData$Total.Mean.3) & is.na(rawData$Total.Median.3)))
# sum(!(is.na(rawData$Total.Mean.4) & is.na(rawData$Total.Median.4)))
# sum(!(is.na(rawData$Total.Mean.5) & is.na(rawData$Total.Median.5)))
# # sum(!(is.na(rawData$Total.Mean.6) & is.na(rawData$Total.Median.6)))
# 
# ### Total DMNE
# sum(!(is.na(rawData$Total.Mean.7) & is.na(rawData$Total.Median.7)))
# sum(!(is.na(rawData$Total.Mean.8) & is.na(rawData$Total.Median.8)))
# sum(!(is.na(rawData$Total.Mean.9) & is.na(rawData$Total.Median.9)))
# sum(!(is.na(rawData$Total.Mean.10) & is.na(rawData$Total.Median.10)))
# sum(!(is.na(rawData$Total.Mean.11) & is.na(rawData$Total.Median.11)))
# sum(!(is.na(rawData$Total.Mean.12) & is.na(rawData$Total.Median.12)))
# # sum(!(is.na(rawData$Total.Mean.13) & is.na(rawData$Total.Median.13)))
# 
# ### Total Indirect
# sum(!(is.na(rawData$Total.income.lost.Mean) & is.na(rawData$Total.income.lost.Median)))
# sum(!(is.na(rawData$Total.income.lost.Mean.1) & is.na(rawData$Total.income.lost.Median.1)))
# sum(!(is.na(rawData$Total.income.lost.Mean.2) & is.na(rawData$Total.income.lost.Median.2)))
# sum(!(is.na(rawData$Total.income.lost.Mean.3) & is.na(rawData$Total.income.lost.Median.3)))
# sum(!(is.na(rawData$Total.income.lost.Mean.4) & is.na(rawData$Total.income.lost.Median.4)))
# sum(!(is.na(rawData$Total.income.lost.Mean.5) & is.na(rawData$Total.income.lost.Median.5)))
