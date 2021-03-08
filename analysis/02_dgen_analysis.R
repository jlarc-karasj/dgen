# Joshua Karas
# 11/30/2020
# Renewable Energy

# This script imports the dgen agent outputs for two scenarios (preferences on  and preferences off)
# Preferences on = control, preferences off = counterfactual
# Then it does some basic analysis

library(tidyverse)
library(magrittr)


# Load and prepare data ---------------------------------------------------

# Cross-link for county name  
wa_meta <- readr::read_csv("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen_data/OS_dGen_Beta_Load_Meta_Data/res/state_data/state_meta/WA_meta.csv")

crosslink <- wa_meta %>% 
  select(dgen_county_id, build_existing_model.county_name) %>% 
  rename(county_id = dgen_county_id, county_name = build_existing_model.county_name) %>%
  distinct()

# Load a model run
input_paths <- paste0("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen/analysis/model_runs/agent_outputs_", c("incentives_on_2", "incentives_off_2"), ".csv")

# Flag model runs with 2018 preference turned on as control
incentives_on <- readr::read_csv(input_paths[1])
incentives_on$treatment <- "incentives_on"

# Flag model runs with 2018 preference turned off as counterfactual
incentives_off <- readr::read_csv(input_paths[2])
incentives_off$treatment <- "incentives_off"

data <- bind_rows(incentives_on, incentives_off) %>%
  left_join(crosslink)

rm(incentives_on, incentives_off, wa_meta, crosslink)


# Histograms --------------------------------------------------------------

graphs  <- data %>% 
  filter(treatment == "incentives_on" & year == 2018 & new_adopters > 0)

hist(graphs$developable_roof_sqft)

hist(graphs$max_demand_kw)

hist(graphs$avg_monthly_kwh)

hist(graphs$first_year_elec_bill_savings)

graphs %>% 
  count(tilt) %$% 
  barplot(n, main = "Tilt Distribution", names.arg = tilt)

graphs %>%
  count(azimuth) %$%
  barplot(n, main = "Azimuth Distribution", names.arg = azimuth)

rm(graphs)

# County-level Aggregation ------------------------------------------------

county <- data %>%
  group_by(county_name, year, treatment) %>% 
  summarise(total_new_adopters = sum(new_adopters), 
            total_adopters = sum(number_of_adopters),
            total_customers = sum(customers_in_bin),
            total_new_system_kw = sum(new_system_kw),
            total_system_kw = sum(system_kw_cum),
            mean_system_kw = total_new_system_kw / total_new_adopters,
            mean_system_capex_per_kw = sum(system_capex_per_kw * cap_cost_multiplier * new_adopters)/total_new_adopters) %>%
  ungroup()


# Graphs of New Adopters --------------------------------------------------

# Total new adopters by year and treatment
adopters <- county %>% 
  group_by(year, treatment) %>% 
  summarise(adopters = sum(total_new_adopters)) %>%
  ungroup()

adopters %>%
  ggplot(aes(x = year, y = adopters, color = treatment)) + 
  geom_line(size = 1.0) + 
  labs(title = "Yearly New Adopters", x = "Year", y = "New Adopters", color = "Treatment") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) + 
  theme_minimal()


# Difference of New Adopters between Control and Counterfactual by year
adopters_diff <- adopters %>%
  spread(treatment, adopters) %>%
  mutate(difference = incentives_on - incentives_off)

adopters_diff %>%
  ggplot(aes(x = year, y = difference)) + 
  geom_line(size = 1.0) + 
  labs(title = "Yearly Difference in New Adopters", subtitle = "(Tax Pref ON - Tax Pref OFF)", x = "Year", y = "Difference in New Adopters") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) + 
  theme_minimal()
  

# Cumulative difference of new adopters by year
adopters_cum <- adopters_diff %>% 
    mutate(cumulative = cumsum(difference),
           cumulative_off = cumsum(incentives_off),
           cumulative_on = cumsum(incentives_on),
           perc_diff = (cumulative_on / cumulative_off) - 1)

adopters_cum %>%
  ggplot(aes(x = year, y = perc_diff)) + 
  geom_line(size = 1.0) + 
  labs(title = "Percent Difference in New Adopters", subtitle = "(Tax Pref ON / Tax Pref OFF)", x = "Year", y = "Difference in New Adotpers") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) + 
  theme_minimal()

rm(adopters, adopters_cum, adopters_diff)


# Graphs of New System KW -------------------------------------------------

# Total new system kw by year and treatment
systems <- county %>% 
  group_by(year, treatment) %>% 
  summarise(system_kw = sum(total_new_system_kw)) %>%
  ungroup()

systems %>%
  ggplot(aes(x = year, y = system_kw, color = treatment)) + 
  geom_line(size = 1.0) + 
  labs(title = "Yearly New System KW", x = "Year", y = "New System KW", color = "Treatment") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) + 
  theme_minimal()


# Difference of New Adopters between Control and Counterfactual by year
systems_diff <- systems %>%
  spread(treatment, system_kw) %>%
  mutate(difference = incentives_on - incentives_off)

systems_diff %>%
  ggplot(aes(x = year, y = difference)) + 
  geom_line(size = 1.0) + 
  labs(title = "Yearly Difference in New System KW", subtitle = "(Tax Pref ON - Tax Pref OFF)", x = "Year", y = "Difference in New System KW") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) +  
  theme_minimal()


# Cumulative difference of new adopters by year
systems_cum <- systems_diff %>% 
  mutate(cumulative = cumsum(difference),
         cumulative_off = cumsum(incentives_off),
         cumulative_on = cumsum(incentives_on),
         perc_diff = (cumulative_on / cumulative_off) - 1)

systems_cum %>%
  ggplot(aes(x = year, y = perc_diff)) + 
  geom_line(size = 1.0) + 
  labs(title = "Cumulative Difference in New System KW", subtitle = "(Tax Pref ON - Tax Pref OFF)", x = "Year", y = "Difference in New System KW") + 
  scale_x_continuous(limits = c(2014, 2024), breaks = seq(2014, 2024, 2)) + 
  theme_minimal()

rm(systems, systems_diff, systems_cum)


# System Size -------------------------------------------------------------

county %>% 
  group_by(year, treatment) %>% 
  summarise(size = round(sum(total_new_system_kw) / sum(total_new_adopters), 1)) %>% 
  ggplot(aes(x = year, y = size, color = treatment)) + 
  geom_line(size = 1.0) + 
  labs(title = "Average System Size", x = "Year", y = "Average System KW") + 
  scale_x_discrete(limits = seq(2014, 2030, 2)) + 
  theme_minimal()

county %>% 
  group_by(year, treatment) %>% 
  summarise(adopters = sum(total_adopters), customers = sum(total_customers)) %>%
  ungroup() %>%
  spread(treatment, adopters) %>%
  mutate(incentives_on_percent = incentives_on / customers, incentives_off_percent = incentives_off / customers)


#output_path <- paste0("//Securefs/jlarc_power$/Documentation/Analysis/Workpapers/git_repo/dgen_analysis/model_runs/county_totals_run", run_num, ".csv")

#readr::write_csv(county_data, output_path)
