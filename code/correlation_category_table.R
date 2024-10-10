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
                               "Risk-averting",
                               "Risk-exposing"))

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

cor_tbl_lag0_medians = cor_tbl_lag0 %>%
  select(-behavior_cat) %>%
  pivot_wider(names_from = behavior, values_from = lag0) %>%
  mutate(
    risk_averting = apply(select(.,
                                 `Avoiding contact with other people`, `Avoiding public or crowded places`,
                                 `Frequently washing hands`, `Wearing a face mask when outside of your home`), 1, median, na.rm = TRUE),
    risk_exposing = apply(select(.,
                                 `Go to work`, `Go to the gym`, `Go visit a friend`,
                                 `Go to a cafe, bar, or restaurant`, `Go to a doctor or visit a hospital`,
                                 `Go to church or another place of worship`), 1, median, na.rm = TRUE)
  ) %>%
  select(state, severity_type, risk_averting, risk_exposing)


cor_tbl_lag0_2cat_mortality_only = cor_tbl_lag0_2cat %>% filter(severity_type == "mortality") %>% select(-severity_type)
cor_tbl_lag0_2cat_medians_mortality = cor_tbl_lag0_medians %>% filter(severity_type == "mortality") %>% select(-severity_type)

cor_tbl_lag0_2cat_medians_mortality
cor_tbl_lag0_2cat_mortality_only %>%
  filter(state != "National") %>%
  ggplot(aes(x = risk_averting)) +
  geom_histogram(binwidth = 0.05)

write_csv(cor_tbl_lag0_2cat_mortality_only, file = "data/statewise_lag0_correlations_mortality.csv")

mean(cor_tbl_lag0_2cat_mortality_only$risk_averting)
median(cor_tbl_lag0_2cat_mortality_only$risk_exposing)

state_corr_mortality_avgs = cor_tbl_lag0_2cat_mortality_only %>%
  filter(state != "National")

state_corr_mortality_avgs$risk_averting %>% mean()
state_corr_mortality_avgs$risk_exposing %>% mean()

## Do the same for each lag
cor_tbl

lag_cor_tbl = cor_tbl %>%
  filter(severity_type == "mortality") %>%
  group_by(state, behavior_cat) %>%
  summarize(
    avg_lag_2 = mean(`lag_-2`, na.rm = TRUE),
    avg_lag_1 = mean(`lag_-1`, na.rm = TRUE),
    avg_lag0  = mean(`lag0`, na.rm = TRUE),
    avg_lag1  = mean(`lag_+1`, na.rm = TRUE),
    avg_lag2  = mean(`lag_+2`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = behavior_cat,
    values_from = c(avg_lag_2, avg_lag_1, avg_lag0, avg_lag1, avg_lag2),
    names_glue = "{behavior_cat}_{.value}")

lag_cor_tbl %>%
  select(state, contains("averting"))

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_exposing_lag = colnames(select(., starts_with("Risk-exposing_avg_")))[which.max(c_across(starts_with("Risk-exposing_avg_")))],
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_averting_lag)

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_exposing_lag = colnames(select(., starts_with("Risk-exposing_avg_")))[which.min(c_across(starts_with("Risk-exposing_avg_")))],
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_exposing_lag)

# Cases

lag_cor_tbl = cor_tbl %>%
  filter(severity_type == "cases") %>%
  group_by(state, behavior_cat) %>%
  summarize(
    avg_lag_2 = mean(`lag_-2`, na.rm = TRUE),
    avg_lag_1 = mean(`lag_-1`, na.rm = TRUE),
    avg_lag0  = mean(`lag0`, na.rm = TRUE),
    avg_lag1  = mean(`lag_+1`, na.rm = TRUE),
    avg_lag2  = mean(`lag_+2`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = behavior_cat,
    values_from = c(avg_lag_2, avg_lag_1, avg_lag0, avg_lag1, avg_lag2),
    names_glue = "{behavior_cat}_{.value}")

lag_cor_tbl %>%
  select(state, contains("averting"))

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_averting_lag)

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_exposing_lag = colnames(select(., starts_with("Risk-exposing_avg_")))[which.min(c_across(starts_with("Risk-exposing_avg_")))],
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_exposing_lag)

# Hospitalizations

cor_tbl %>% select(severity_type) %>% distinct()

lag_cor_tbl = cor_tbl %>%
  filter(severity_type == "hospitalization") %>%
  group_by(state, behavior_cat) %>%
  summarize(
    avg_lag_2 = mean(`lag_-2`, na.rm = TRUE),
    avg_lag_1 = mean(`lag_-1`, na.rm = TRUE),
    avg_lag0  = mean(`lag0`, na.rm = TRUE),
    avg_lag1  = mean(`lag_+1`, na.rm = TRUE),
    avg_lag2  = mean(`lag_+2`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = behavior_cat,
    values_from = c(avg_lag_2, avg_lag_1, avg_lag0, avg_lag1, avg_lag2),
    names_glue = "{behavior_cat}_{.value}")

lag_cor_tbl %>%
  select(state, contains("averting"))

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_averting_lag)

lag_cor_tbl %>%
  rowwise() %>%
  mutate(
    max_risk_exposing_lag = colnames(select(., starts_with("Risk-exposing_avg_")))[which.min(c_across(starts_with("Risk-exposing_avg_")))],
    max_risk_averting_lag = colnames(select(., starts_with("Risk-averting_avg_")))[which.max(c_across(starts_with("Risk-averting_avg_")))]
  ) %>%
  ungroup() %>%
  select(state, starts_with("max")) %>%
  count(max_risk_exposing_lag)



