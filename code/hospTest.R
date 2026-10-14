

hospData <- read_csv("~/Downloads/TB_expenditure_utilisation_2026-07-21.csv") %>% 
            select(country, iso3, g_whoregion, year, 
                   hcfvisit_dstb, hcfvisit_mdr,
                   hospd_dstb_prct, hospd_mdr_prct) %>% 
            rename(country.code = iso3,
                   study.end.year = year)

ggplot(hospData %>% filter(country.code %in% LMIC$country.code)) + geom_line(aes(study.end.year, hospd_dstb_prct)) + theme_minimal() +
    facet_wrap(facets=vars(country.code))

ggplot(hospData %>% filter(country.code %in% LMIC$country.code)) + geom_line(aes(study.end.year, hcfvisit_dstb)) + theme_minimal() +
    facet_wrap(facets=vars(country.code))


dim(modelData %>% filter(study.end.year > 2016))

# modelDataHosp <- left_join(modelData, hospData) %>% filter(!is.na(hospd_mdr_prct)) %>% 
#                  mutate(hosp.rate = ((drtb.wt * hospd_mdr_prct) + ((1-drtb.wt) * hospd_dstb_prct)))
#                             
# dim(modelDataHosp %>% filter(is.na(hosp.rate)))

modelDataHosp <- left_join(modelData %>% 
                           mutate(true.year = study.end.year,
                                  study.end.year = ifelse(study.end.year > 2016, 
                                                          study.end.year, 2017)), hospData) %>% 
                filter(!is.na(hospd_dstb_prct)) %>% 
                select(! study.end.year) %>% 
                rename(study.end.year = true.year, 
                       hosp.rate = hospd_dstb_prct) %>%
                ungroup() %>%
                group_by(covidence.id) %>%
                mutate(survey.id = cur_group_id()) %>% 
                ungroup() %>% 
                group_by(country.code) %>% 
                mutate(country.id = cur_group_id()) %>% ungroup()

covariatesHosp <- left_join(covariateFilt24, hospData)  %>% filter(!is.na(hospd_dstb_prct))  %>% 
                  rename(hosp.rate = hospd_dstb_prct) %>%     
                  mutate(country.id.new = cur_group_id(), 
                  in.review = ifelse(country.code %in% modelDataHosp$country.code,1, 0)) %>% 
                  left_join(modelDataHosp %>% ungroup() %>% 
                                  select(country.code, country.id) %>% unique()) %>% 
                  mutate(in.review.id = ifelse(is.na(country.id), 99, country.id))

stanDataHosp <- list(
    N = length(modelDataHosp$cost),
    cost = modelDataHosp$cost,
    se = rep(0, length(modelDataHosp$cost)),
    # se = as.numeric(modelDataHosp$se)+ 0.01,
    dme_prediag = modelDataHosp$dme.prediag, 
    dnme_prediag =  modelDataHosp$dnme.prediag, 
    ie_prediag =  modelDataHosp$ie.prediag, 
    dme_intsvtx = modelDataHosp$dme.intsvtx, 
    dnme_intsvtx =  modelDataHosp$dnme.intsvtx, 
    ie_intsvtx =  modelDataHosp$ie.intsvtx,
    dme_contntx = modelDataHosp$dme.contntx, 
    dnme_contntx =  modelDataHosp$dnme.contntx, 
    ie_contntx =  modelDataHosp$ie.contntx, 
    N_survey = max(modelDataHosp$survey.id),
    survey_id = modelDataHosp$survey.id, 
    N_country = max(modelDataHosp$country.id),
    country_id = modelDataHosp$country.id, 
    id_matrix = modelDataHosp %>% select(survey.id, country.id) %>% distinct(), 
    
    ### Internal covariate
    drtb_ind= modelDataHosp$drtb.wt,
    survey_year = ((modelDataHosp$study.end.year - mean(modelDataHosp$study.end.year)) / sd(modelDataHosp$study.end.year)),
    
    prior_sigma = 2, 
    prob_cost_zero = sum(modelDataHosp$cost == 0) / nrow(modelDataHosp), 


    ### External covariateFilt
    median_income = ((modelDataHosp$log.median.income - mean(modelDataHosp$log.median.income)) / sd(modelDataHosp$log.median.income)),
    oop_perc_che =  (modelDataHosp$oop.perc.che - mean(modelDataHosp$oop.perc.che))/sd(modelDataHosp$oop.perc.che), 
    uhc_index =     (modelDataHosp$UHCindex - mean(modelDataHosp$UHCindex)) / sd(modelDataHosp$UHCindex), 
    hosp_rate =     (modelDataHosp$hosp.rate - mean(modelDataHosp$hosp.rate)) / sd(modelDataHosp$hosp.rate), 
    ### Prediction data 
    N_country_new = max(covariatesHosp$country.id.new),
    review_ind = covariatesHosp$in.review, 
    in_review_id = covariatesHosp$in.review.id, # note that this is different from modelDataHosp country.id
    pred_year = (2024 - mean(modelDataHosp$study.end.year)) / sd(modelDataHosp$study.end.year), 
    median_income_new = (covariatesHosp$log.median.income.trunc - mean(modelDataHosp$log.median.income)) / sd(modelDataHosp$log.median.income), 
    oop_perc_che_new = (covariatesHosp$oop.perc.che.trunc - mean(modelDataHosp$oop.perc.che)) / sd(modelDataHosp$oop.perc.che), 
    uhc_index_new = (covariatesHosp$uhc.index.trunc - mean(modelDataHosp$UHCindex)) / sd(modelDataHosp$UHCindex),
    hosp_rate_new =     (covariatesHosp$hosp.rate - mean(modelDataHosp$hosp.rate)) / sd(modelDataHosp$hosp.rate)
    
)

modelHosp <- stan(file = here("code/inst/hospModel.stan"),
               data = stanDataHosp,
               iter = tbPtntCstIter, chains = 2,
               warmup = tbPtntCstWarmup,
               control=list(max_treedepth=14), 
               seed = tbPtntCstSeed)

sampsHosp <- tidybayes::spread_draws(modelHosp,
                                  dstb_total_cost_country[country.id.new],
                                  ndraws = 1000, seed = tbPtntCstSeed);

estHosp<- left_join(sampsHosp  %>%
                     group_by(country.id.new) %>%
                     mean_qi(dstb_total_cost_country) %>%
                     select(! c(.width, .point, .interval)),
                 covariatesHosp %>% select(study.country, country.id.new, in.review) %>% distinct()) %>% 
    arrange(dstb_total_cost_country) %>% 
    mutate(study.country=factor(study.country, levels=study.country)) 

range(estHosp$dstb_total_cost_country)
ggplot(data=estHosp) + 
    geom_point(aes(x=study.country, y = dstb_total_cost_country)) + 
    geom_errorbar(aes(x=study.country, ymin = .lower, ymax = .upper)) + 
    theme_minimal() + coord_flip()


coefHosp <- as.data.frame(modelHosp) %>% select(beta_dme_prediag,
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
                                          contains("mult_hosp")) %>% 
    mean_qi() %>% select(!c(.width, .point, .interval)) 


colnames(coefHosp) <- gsub("mult_year", "yearMult", colnames(coefHosp), fixed=TRUE)
colnames(coefHosp) <- gsub("mult_drtb", "drtbMult", colnames(coefHosp), fixed=TRUE)
colnames(coefHosp) <- gsub("mult_med_income", "medIncMult", colnames(coefHosp), fixed=TRUE)
colnames(coefHosp) <- gsub("mult_oop_perc_che", "oopMult", colnames(coefHosp), fixed=TRUE)
colnames(coefHosp) <- gsub("mult_uhc", "uhcMult", colnames(coefHosp), fixed=TRUE)
colnames(coefHosp) <- gsub("mult_hosp", "hospMult", colnames(coefHosp), fixed=TRUE)

colnames(coefHosp) <- gsub(".", "_", colnames(coefHosp), fixed=TRUE)

coefHosp <- coefHosp %>% 
    pivot_longer(cols = everything(), names_to = c("type", "category", "phase", "desc"), names_sep = "_") %>%
    mutate(desc = ifelse(is.na(desc), "mean", desc), 
           value = round(value,2)) %>% 
    pivot_wider(id_cols = c(type, category, phase), values_from = value, names_from = desc) %>%
    mutate(formatted = paste0(mean, " (", lower, ", ", upper, ")"))

View(coefHosp)
