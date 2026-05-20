#### Code to impute missing GINI coefficients using country and year matched 
#### GDP, HDI, and world region. 
#### Written by JS Su on 5/19/26 and adapted by NA Swartwood. 

library(dplyr)
library(tidyr)
library(readr)
library(here)

### Read in the LMICs 
lmic <- read.csv(here("data/LMIC2025.csv"))
lmic <- lmic |> 
  rename(Code = country.code) %>% 
  arrange(Code)

gini <- read.csv(here("data/giniImputation/world bank gini index.csv"), check.names = FALSE)
gini <- gini |> 
  slice(-c(218:271)) |> 
  filter(`Country Code`%in% (lmic$Code)) |> 
  pivot_longer(
    cols = contains("[YR"),
    names_to = "year",
    values_to = "gini"
  ) |>
  mutate(
    year = parse_number(year),
    gini = na_if(as.character(gini), ".."),
    gini = as.numeric(gini)
  ) |> 
  rename(iso3 = "Country Code") |> 
  select(iso3, year, gini) |> 
  arrange(iso3)
hdi <- read.csv(here("data/giniImputation/undp_hdi.csv"), check.names = FALSE)
hdi <- hdi |> 
  filter(iso3 %in% (lmic$Code)) |> 
  pivot_longer(
    cols = hdi_1990:hdi_2023,
    names_to = "year",
    values_to = "hdi"
  ) |>
  select(iso3, year, hdi) |> 
  mutate(
    year = gsub("hdi_", "", year),
    year = as.integer(year)
  ) |> 
  arrange(iso3) |> 
  group_by(iso3) |> # linear interpolation
  mutate(
    hdi = if (sum(!is.na(hdi)) >= 2) { # NA: PRK, MCO, no XKX
      ifelse(is.na(hdi), predict(lm(hdi ~ year)), hdi)
    } else {
      hdi
    }
  ) |>
  ungroup()

gdp_pc <- read.csv(here("data/giniImputation/GDP_per_cap.csv")) # NA: PRK
gdp_pc <- gdp_pc |> 
  rename(iso3 = "ISO3", year = "Year") |> 
  mutate(
    year = as.integer(year),
    pc_GDP = as.numeric(pc_GDP)
  ) |> 
  select(-Country_Name) |> 
  arrange(iso3)
world_region <- read_csv(here("data/giniImputation/acs_census_crosswalk_8-26-2021_V5.csv"))
world_region <- world_region %>% 
  select(-c(1, 2, (4:8), 10)) %>% 
  filter(iso3 %in%(lmic$Code)) |> 
  arrange(iso3) |> 
  distinct(iso3, WorldBank)
gini_model_data <- gini |>
  left_join(hdi, by = c("iso3", "year")) |>
  left_join(gdp_pc, by = c("iso3", "year")) |>
  filter(year %in% 2000:2023) |> 
  left_join(world_region, by = "iso3") |> 
  mutate(WorldBank = if_else(iso3 == "XKX", "ECA", WorldBank)) |> 
  # filter(!iso3 %in% "PRK") |> # remove PRK, no data
  arrange(iso3, year) |> 
  mutate(
    gini = as.numeric(gini),
    hdi = as.numeric(hdi),
    pc_GDP = as.numeric(pc_GDP),
    year = as.integer(year),
    WorldBank = as.factor(WorldBank),
    iso3 = as.factor(iso3),
    log_pc_GDP = ifelse(pc_GDP > 0, log(pc_GDP), NA_real_)
  )
gini_complete <- gini_model_data |>
  filter(
    !is.na(gini),
    !is.na(hdi),
    !is.na(log_pc_GDP),
    !is.na(year),
    !is.na(WorldBank)
  )
library(mgcv)
gini_model <- gam(gini ~ s(hdi, k = 4) + s(log(pc_GDP), k = 4) + s(year) + WorldBank + s(iso3, bs = "re"), 
                 data = gini_complete, family = gaussian(), method = "REML")
summary(gini_model)
gam.check(gini_model)
plot(gini_model, pages = 1, shade = TRUE)
gini_complete <- gini_complete |>
  mutate(
    gini_fitted = fitted(gini_model),
    residual = gini - gini_fitted
  )
sqrt(mean(gini_complete$residual^2, na.rm = TRUE))  # RMSE
mean(abs(gini_complete$residual), na.rm = TRUE)     # MAE
gini_model_data <- gini_model_data |>
  mutate(
    gini_pred = predict(gini_model, newdata = gini_model_data),
    gini_pred = pmin(pmax(gini_pred, 0), 100),
    gini_imputed = ifelse(is.na(gini), gini_pred, gini)
  )
write.csv(gini_model_data, file = here("data/covariateData/calc-median-income/GINI-imputed.csv"), row.names = FALSE)
