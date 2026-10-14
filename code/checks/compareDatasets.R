medianIncomeDFLong <- medianIncomeDF %>% 
    select(study.country, year, median.income.calc.usd)
    pivot_longer()

pdf(file = "~/Desktop/compareMI.pdf", width = 11.5,  height= 8.5)

print(
    ggplot() +
        geom_point(data=medianIncomeDFOld[1:1008,], aes(x=study.end.year, y=median.income.calc.usd), color="red") +
        geom_point(data=medianIncomeDF0[1:1008,], aes(x=study.end.year, y=median.income.calc.usd), color="darkgreen") +
        theme_minimal() +
        facet_wrap(facets=vars(study.country))
)
print(
    ggplot() +
        geom_point(data=medianIncomeDFOld[1009:2016,], aes(x=study.end.year, y=median.income.calc.usd), color="red") +
        geom_point(data=medianIncomeDF0[1009:2016,], aes(x=study.end.year, y=median.income.calc.usd), color="darkgreen") +
        theme_minimal() +
        facet_wrap(facets=vars(study.country))
)
print(
    ggplot() +
        geom_point(data=medianIncomeDFOld[2017:3024,], aes(x=study.end.year, y=median.income.calc.usd), color="red") +
        geom_point(data=medianIncomeDF0[2017:3024,], aes(x=study.end.year, y=median.income.calc.usd), color="darkgreen") +
        theme_minimal() +
        facet_wrap(facets=vars(study.country))
)
print(
    ggplot() +
        geom_point(data=medianIncomeDFOld[3025:nrow(medianIncomeDFOld),], aes(x=study.end.year, y=median.income.calc.usd), color="red") +
        geom_point(data=medianIncomeDF0[3025:nrow(medianIncomeDFOld),], aes(x=study.end.year, y=median.income.calc.usd), color="darkgreen") +
        theme_minimal() +
        facet_wrap(facets=vars(study.country))
)
dev.off()

# giniOldDF <- read.csv(here("data/covariateData/calc-median-income/GINI-old.csv")) %>% 
#     select(Country.Name, 
#            Country.Code,
#            X2000:X2023) %>%
#     filter(Country.Code %in% LMIC$country.code) %>% 
#     rename(study.country = Country.Name) %>%
#     pivot_longer(cols = X2000:X2023, names_to = "study.end.year", values_to = "GINI index") %>%
#     mutate(study.end.year = as.double(gsub("X", "", study.end.year, fixed = TRUE))) 

# pdf(file = "~/Desktop/compareGINI.pdf", width = 11.5,  height= 8.5)
# 
# print(
#     ggplot() + 
#         geom_point(data=giniDF[1:1008,], aes(x=study.end.year, y=`GINI index`), color="red") + 
#         geom_point(data=giniOldDF[1:1008,], aes(x=study.end.year, y=`GINI index`), color="darkgreen") + 
#         theme_minimal() + 
#         facet_wrap(facets=vars(study.country))
# )
# print(
#     ggplot() + 
#         geom_point(data=giniDF[1009:2016,], aes(x=study.end.year, y=`GINI index`), color="red") + 
#         geom_point(data=giniOldDF[1009:2016,], aes(x=study.end.year, y=`GINI index`), color="darkgreen") + 
#         theme_minimal() + 
#         facet_wrap(facets=vars(study.country))
# )
# print(
#     ggplot() + 
#         geom_point(data=giniDF[2017:3024,], aes(x=study.end.year, y=`GINI index`), color="red") + 
#         geom_point(data=giniOldDF[2017:3024,], aes(x=study.end.year, y=`GINI index`), color="darkgreen") + 
#         theme_minimal() + 
#         facet_wrap(facets=vars(study.country))
# )
# print(
#     ggplot() + 
#         geom_point(data=giniDF[3025:nrow(giniDF),], aes(x=study.end.year, y=`GINI index`), color="red") + 
#         geom_point(data=giniOldDF[3025:nrow(giniDF),], aes(x=study.end.year, y=`GINI index`), color="darkgreen") + 
#         theme_minimal() + 
#         facet_wrap(facets=vars(study.country))
# )
# print(
# ggplot(medianIncomeDF0[1:1008,]) + 
#     geom_line(aes(x=study.end.year, y=GINI.interpolated), color="purple") + 
#     # geom_line(aes(x=study.end.year, y=`GINI index`), color="green") + 
#     geom_point(aes(x=study.end.year, y=`GINI index`), color="green", size=2) + 
#     
#     geom_line(aes(x=study.end.year, y=GINI.imputed), color="orange") +
#     facet_wrap(facets=vars(study.country)) + theme_minimal()
# )
# print(
# ggplot(medianIncomeDF0[1009:2016,]) +  
#     geom_line(aes(x=study.end.year, y=GINI.interpolated), color="purple") + 
#     # geom_line(aes(x=study.end.year, y=`GINI index`), color="green") + 
#     geom_point(aes(x=study.end.year, y=`GINI index`), color="green", size=2) +
#     
#     geom_line(aes(x=study.end.year, y=GINI.imputed), color="orange") +
#     facet_wrap(facets=vars(study.country)) + theme_minimal()
# )
# print(
# ggplot(medianIncomeDF0[2017:3024,]) + 
#     geom_line(aes(x=study.end.year, y=GINI.interpolated), color="purple") + 
#     # geom_line(aes(x=study.end.year, y=`GINI index`), color="green") + 
#     geom_point(aes(x=study.end.year, y=`GINI index`), color="green", size=2) +
#     
#     geom_line(aes(x=study.end.year, y=GINI.imputed), color="orange") +
#     facet_wrap(facets=vars(study.country)) + theme_minimal()
# )
# print(
# ggplot(medianIncomeDF0[2035:nrow(medianIncomeDF0),]) + 
#     geom_line(aes(x=study.end.year, y=GINI.interpolated), color="purple") + 
#     # geom_line(aes(x=study.end.year, y=`GINI index`), color="green") + 
#     geom_point(aes(x=study.end.year, y=`GINI index`), color="green", size=2) +
#     
#     geom_line(aes(x=study.end.year, y=GINI.imputed), color="orange") +
#     facet_wrap(facets=vars(study.country)) + theme_minimal()
# ) 
# 
# dev.off()