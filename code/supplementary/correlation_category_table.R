## -----------------------------------------------------------------------------
## Script name: correlation_category_table.R
## Purpose: Calculate number of elements in correlation categories
## Author: George Dewey
## Date Created: 2024-10-09
## Last Updated: 2024-10-09
## -----------------------------------------------------------------------------

# Load required packages
library(tidyverse)

# Read data
corrdata <- xlsx::read.xlsx(
  file = "data/all_state_ba_lag_corr_w_mortality_hospt_cases.xlsx",
  sheetName = "Sheet1"
)

# Initial data preparation
cor_tbl <- corrdata %>% 
  as_tibble() %>%
  setNames(c("behavior", "severity_type", "state", "lag_-2", "lag_-1", "lag0", "lag_+1", "lag_+2"))

# Add behavior categories
cor_tbl <- cor_tbl %>%
  mutate(
    behavior_cat = ifelse(
      behavior %in% c(
        "Avoiding contact with other people",
        "Avoiding public or crowded places",
        "Frequently washing hands",
        "Wearing a face mask when outside of your home"
      ),
      "Risk-averting",
      "Risk-exposing"
    )
  )

# Function to analyze lag patterns
analyze_lag_patterns <- function(data, severity_type_filter, behavior_filter) {
  data %>%
    filter(
      severity_type == severity_type_filter,
      behavior == behavior_filter
    ) %>%
    select(-behavior_cat) %>%
    rowwise() %>%
    mutate(
      max_lag = colnames(select(., starts_with("lag")))[which.max(c_across(starts_with("lag")))]
    ) %>%
    ungroup() %>%
    count(max_lag)
}

# Create lag0 dataset
create_lag0_dataset <- function(data) {
  data %>%
    select(-behavior_cat) %>%
    pivot_wider(names_from = behavior, values_from = lag0) %>%
    mutate(
      risk_averting = rowMeans(select(.,
        `Avoiding contact with other people`, 
        `Avoiding public or crowded places`,
        `Frequently washing hands`, 
        `Wearing a face mask when outside of your home`
      ), na.rm = TRUE),
      risk_exposing = rowMeans(select(.,
        `Go to work`, `Go to the gym`, `Go visit a friend`,
        `Go to a cafe, bar, or restaurant`, 
        `Go to a doctor or visit a hospital`,
        `Go to church or another place of worship`
      ), na.rm = TRUE)
    ) %>%
    select(state, severity_type, risk_averting, risk_exposing)
}

# Create datasets
cor_tbl_lag0 <- cor_tbl %>% 
  select(behavior, severity_type, state, lag0, behavior_cat)

cor_tbl_lag0_2cat <- create_lag0_dataset(cor_tbl_lag0)
cor_tbl_lag0_2cat_mortality_only <- cor_tbl_lag0_2cat %>% 
  filter(severity_type == "mortality") %>% 
  select(-severity_type)

# Save results
write_csv(cor_tbl_lag0_2cat_mortality_only, 
          file = "data/statewise_lag0_correlations_mortality.csv")

# Calculate summary statistics
state_corr_mortality_avgs <- cor_tbl_lag0_2cat_mortality_only %>%
  filter(state != "National")

# Analysis by severity type
analyze_by_severity <- function(data, severity_type) {
  data %>%
    filter(severity_type == !!severity_type) %>%
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
      names_glue = "{behavior_cat}_{.value}"
    )
}

# Analyze different severity types
mortality_analysis <- analyze_by_severity(cor_tbl, "mortality")
cases_analysis <- analyze_by_severity(cor_tbl, "cases")
hospitalization_analysis <- analyze_by_severity(cor_tbl, "hospitalization")
