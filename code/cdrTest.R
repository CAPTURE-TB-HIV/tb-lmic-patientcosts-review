

cdrData <- read_csv("~/Downloads/TB_burden_countries_2026-07-23.csv") %>% 
    select(country, iso3, g_whoregion, year, 
           c_cdr) %>% 
    rename(country.code = iso3,
           study.end.year = year)

ggplot(cdrData %>% filter(country.code %in% LMIC$country.code)) + geom_line(aes(study.end.year, c_cdr)) + theme_minimal() +
    facet_wrap(facets=vars(country.code))


# modelDatacdr <- left_join(modelData, cdrData) %>% filter(!is.na(cdrd_mdr_prct)) %>% 
#                  mutate(cdr.rate = ((drtb.wt * cdrd_mdr_prct) + ((1-drtb.wt) * cdrd_dstb_prct)))
#                             
# dim(modelDatacdr %>% filter(is.na(cdr.rate)))

modelDatacdr <- left_join(modelData, cdrData) %>% 
    filter(!is.na(c_cdr)) %>% 
    rename(cdr.rate = c_cdr) %>%
    ungroup() %>%
    group_by(covidence.id) %>%
    mutate(survey.id = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(country.code) %>% 
    mutate(country.id = cur_group_id()) %>% ungroup()

covariatescdr <- left_join(covariateFilt24, cdrData)  %>% filter(!is.na(c_cdr))  %>% 
    rename(cdr.rate = c_cdr) %>%     
    mutate(country.id.new = cur_group_id(), 
           in.review = ifelse(country.code %in% modelDatacdr$country.code,1, 0)) %>% 
    left_join(modelDatacdr %>% ungroup() %>% 
                  select(country.code, country.id) %>% unique()) %>% 
    mutate(in.review.id = ifelse(is.na(country.id), 99, country.id))

stanDatacdr <- list(
    N = length(modelDatacdr$cost),
    cost = modelDatacdr$cost,
    se = rep(0, length(modelDatacdr$cost)),
    # se = as.numeric(modelDatacdr$se)+ 0.01,
    dme_prediag = modelDatacdr$dme.prediag, 
    dnme_prediag =  modelDatacdr$dnme.prediag, 
    ie_prediag =  modelDatacdr$ie.prediag, 
    dme_intsvtx = modelDatacdr$dme.intsvtx, 
    dnme_intsvtx =  modelDatacdr$dnme.intsvtx, 
    ie_intsvtx =  modelDatacdr$ie.intsvtx,
    dme_contntx = modelDatacdr$dme.contntx, 
    dnme_contntx =  modelDatacdr$dnme.contntx, 
    ie_contntx =  modelDatacdr$ie.contntx, 
    N_survey = max(modelDatacdr$survey.id),
    survey_id = modelDatacdr$survey.id, 
    N_country = max(modelDatacdr$country.id),
    country_id = modelDatacdr$country.id, 
    id_matrix = modelDatacdr %>% select(survey.id, country.id) %>% distinct(), 
    
    ### Internal covariate
    drtb_ind= modelDatacdr$drtb.wt,
    survey_year = ((modelDatacdr$study.end.year - mean(modelDatacdr$study.end.year)) / sd(modelDatacdr$study.end.year)),
    
    prior_sigma = 2, 
    prob_cost_zero = sum(modelDatacdr$cost == 0) / nrow(modelDatacdr), 
    
    
    ### External covariateFilt
    median_income = ((modelDatacdr$log.median.income - mean(modelDatacdr$log.median.income)) / sd(modelDatacdr$log.median.income)),
    oop_perc_che =  (modelDatacdr$oop.perc.che - mean(modelDatacdr$oop.perc.che))/sd(modelDatacdr$oop.perc.che), 
    uhc_index =     (modelDatacdr$UHCindex - mean(modelDatacdr$UHCindex)) / sd(modelDatacdr$UHCindex), 
    cdr_rate =     (modelDatacdr$cdr.rate - mean(modelDatacdr$cdr.rate)) / sd(modelDatacdr$cdr.rate), 
    ### Prediction data 
    N_country_new = max(covariatescdr$country.id.new),
    review_ind = covariatescdr$in.review, 
    in_review_id = covariatescdr$in.review.id, # note that this is different from modelDatacdr country.id
    pred_year = (2024 - mean(modelDatacdr$study.end.year)) / sd(modelDatacdr$study.end.year), 
    median_income_new = (covariatescdr$log.median.income.trunc - mean(modelDatacdr$log.median.income)) / sd(modelDatacdr$log.median.income), 
    oop_perc_che_new = (covariatescdr$oop.perc.che.trunc - mean(modelDatacdr$oop.perc.che)) / sd(modelDatacdr$oop.perc.che), 
    uhc_index_new = (covariatescdr$uhc.index.trunc - mean(modelDatacdr$UHCindex)) / sd(modelDatacdr$UHCindex),
    cdr_rate_new =     (covariatescdr$cdr.rate - mean(modelDatacdr$cdr.rate)) / sd(modelDatacdr$cdr.rate)
    
)

modelcdr <- stan(file = here("code/inst/cdrModel.stan"),
                  data = stanDatacdr,
                  iter = tbPtntCstIter, chains = 2,
                  warmup = tbPtntCstWarmup,
                  control=list(max_treedepth=14), 
                  seed = tbPtntCstSeed)

sampscdr <- tidybayes::spread_draws(modelcdr,
                                     dstb_total_cost_country[country.id.new],
                                     ndraws = 1000, seed = tbPtntCstSeed);

estcdr<- left_join(sampscdr  %>%
                        group_by(country.id.new) %>%
                        mean_qi(dstb_total_cost_country) %>%
                        select(! c(.width, .point, .interval)),
                    covariatescdr %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(estcdr$dstb_total_cost_country)

ggplot(data=estcdr) + 
    geom_point(aes(x=study.country, y = dstb_total_cost_country)) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper)) + 
    theme_minimal() + coord_flip()


coefcdr <- as.data.frame(modelcdr) %>% select(beta_dme_prediag,
                                                beta_dnme_prediag,
                                                beta_ie_prediag,
                                                beta_dme_intsvtx,
                                                beta_dnme_intsvtx,
                                                beta_ie_intsvtx,
                                                beta_dme_contntx,
                                                beta_dnme_contntx,
                                                beta_ie_contntx,
                                                contains("mult_drtb"),
                                                contains("mult_year"), 
                                                contains("mult_med_income"), 
                                                contains("mult_oop_perc_che"),
                                                contains("mult_uhc"),
                                                contains("mult_cdr")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coefcdr) <- gsub("mult_year", "yearMult", colnames(coefcdr), fixed=TRUE)
colnames(coefcdr) <- gsub("mult_drtb", "drtbMult", colnames(coefcdr), fixed=TRUE)
colnames(coefcdr) <- gsub("mult_med_income", "medIncMult", colnames(coefcdr), fixed=TRUE)
colnames(coefcdr) <- gsub("mult_oop_perc_che", "oopMult", colnames(coefcdr), fixed=TRUE)
colnames(coefcdr) <- gsub("mult_uhc", "uhcMult", colnames(coefcdr), fixed=TRUE)
colnames(coefcdr) <- gsub("mult_cdr", "cdrMult", colnames(coefcdr), fixed=TRUE)

colnames(coefcdr) <- gsub(".", "_", colnames(coefcdr), fixed=TRUE)

coefcdr <- coefcdr %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

View(coefcdr)
