## -----------------------------------------------------------------------------
## Script name: correlation_category_table.R
##
## Purpose of script: Calculate number of elements in correlation categories
##
## Author: George Dewey
##
## Date Created: 2024-10-09
##
## Last Updated: 2024-10-09
## -----------------------------------------------------------------------------

library(tidyverse)

corrdata = xlsx::read.xlsx(file = "data/all_state_ba_lag_corr_w_mortality_hospt_cases.xlsx",
                           sheetName = "Sheet1")

cor_tbl = corrdata %>% as_tibble()

names(cor_tbl) = c("behavior", "severity_type", "state", "lag_-2", "lag_-1", "lag0", "lag_+1", "lag_+2")

cor_tbl = cor_tbl %>%
  mutate(behavior_cat = ifelse(behavior %in% c("Avoiding contact with other people",
                                                  "Avoiding public or crowded places",
                                                  "Frequently washing hands",
                                                  "Wearing a face mask when outside of your home"),
                               "Risk-exposing",
                               "Risk-averting"))

# Create the lag 0 only dataset

cor_tbl_lag0 = cor_tbl %>% select(behavior, severity_type, state, lag0, behavior_cat)

cor_tbl_lag0_2cat = cor_tbl_lag0 %>%
  select(-behavior_cat) %>%
  pivot_wider(names_from = behavior, values_from = lag0) %>%
  mutate(
    risk_averting = rowMeans(select(.,
                                    `Avoiding contact with other people`, `Avoiding public or crowded places`,
                                    `Frequently washing hands`, `Wearing a face mask when outside of your home`), na.rm = TRUE),
    risk_exposing = rowMeans(select(.,
                                    `Go to work`, `Go to the gym`, `Go visit a friend`,
                                    `Go to a cafe, bar, or restaurant`, `Go to a doctor or visit a hospital`,
                                    `Go to church or another place of worship`), na.rm = TRUE)
  ) %>%
  select(state, severity_type, risk_averting, risk_exposing)

cor_tbl_lag0_2cat_mortality_only = cor_tbl_lag0_2cat %>% filter(severity_type == "mortality") %>% select(-severity_type)

write_csv(cor_tbl_lag0_2cat_mortality_only, file = "data/statewise_lag0_correlations_mortality.csv")

median(cor_tbl_lag0_2cat_mortality_only$risk_averting)
median(cor_tbl_lag0_2cat_mortality_only$risk_exposing)
