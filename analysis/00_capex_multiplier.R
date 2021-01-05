# Joshua Karas
# 11/13/2020
# Renewable Energy

# This script is calculating the capital cost multiplier to use for the washington agent pkl

library(tidyverse)
library(rstanarm)

data <- readxl::read_excel("//securefs/jlarc_power$/Documentation/Materials/Agencies/WSUSolardata2020-05-29_Cleaning.xlsx")

# Remove commercial solar projects and anything over 15 kW capacity
data_clean <- data %>% 
  filter(ProjectType != "Commercial-scale" & NamePlateCapacity <= 15) %>% 
  mutate(county = tolower(SiteCounty),
         year = substring(CertificationDate, 1, 4)) %>%
  select(year, county, `$/kW`) %>%
  rename(cost = `$/kW`)

# Regression of the form total_capex = county*base_capex
# Which is also equivalent to log(total_capex) = log(county) + log(base_capex)
glm <- stan_glm(log(cost) ~ county, family = gaussian, data_clean)

print(glm)

prior_summary(glm)

plot(glm, plotfun = "areas", prob = 0.9, 
     pars = c("(Intercept)", paste0("county", unlist(glm$xlevels)[-1])))

pp_check(glm, plotfun = "error_binned")

# Build capex multipliers by county
# Adams was baseline, set to 1.00 (isntead of intercept value)
# Include pend oreille since it wasn't present
capex_multiplier <- as_tibble(exp(glm$coefficients)) %>% 
  bind_cols(county = unlist(glm$xlevels)) %>% 
  select(county, value)

capex_multiplier[1,2] <- 1.00

capex_multiplier <- rbind(capex_multiplier, c(county = "pend oreille", value = 1.00)) %>% 
  mutate(value = as.numeric(value))

print(capex_multiplier)

# Adjust capex multiplier for base cost of solar systems in dgen model
system_capex_per_kw <- 2704.2738

capex_multiplier_adjusted <- capex_multiplier %>% 
  mutate(value = value * (exp(glm$coefficients)[[1]] / system_capex_per_kw))

# merge in dgen_county_id to capex_multiplier_adjusted
wa_meta <- readr::read_csv("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen_data/OS_dGen_Beta_Load_Meta_Data/res/state_data/state_meta/WA_meta.csv")

crosslink <- wa_meta %>% 
  select(dgen_county_id, build_existing_model.county_name) %>% 
  rename(county_id = dgen_county_id) %>%
  distinct() %>% 
  mutate(county = tolower(str_remove(build_existing_model.county_name, " County, WA")))

capex_merge <- capex_multiplier_adjusted %>% 
  left_join(crosslink, by = "county") %>%
  select(county_id, value) %>%
  rename(capex_multiplier = value)

readr::write_csv(capex_merge, "//securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen/analysis/capex_merge.csv")

