## -----------------------------------------------------------------------------
## Script name: disease_severity_correlations.R
##
## Purpose of script: Generate a heatmap for cross-correlations of COVID-19
## disease severity measures (hospitalizations, cases, and deaths)
##
## Author: George Dewey
##
## Date Created: 2024-09-30
##
## Last Updated: 2024-10-01
## ----------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(patchwork)

# Load data --------------------------------------------------------------------

# Monthly data
cases = read_csv("data/severity/monthly/all_state_official_case_cnt.csv", show_col_types = FALSE)
deaths = read_csv("data/severity/monthly/death_agg_all_state_prior7_14_&_30.csv", show_col_types = FALSE)
hosps = read_csv("data/severity/monthly/hospitalization_state_monthly.csv")

hosps[is.na(hosps) == T] = 0
deaths[is.na(deaths) == T] = 0
cases[is.na(cases) == T] = 0

# Create the national level dataset---------------------------------------------
us_monthly_cases = cases %>% select(month_yr, state_code, case_cnt) %>%
  group_by(month_yr) %>%
  summarize(natl_cases = sum(case_cnt))

us_monthly_deaths = deaths %>% filter(Wave == "mid_month") %>% select(End_Date, death_agg30, State) %>%
  group_by(End_Date) %>%
  summarize(natl_deaths = sum(death_agg30)) %>%
  mutate(month_yr = sprintf("%04d-%02d", year(End_Date), month(End_Date))) %>%
  select(month_yr, natl_deaths)

us_monthly_hosps = hosps %>% rename(month_yr = mnth_yr) %>%
  group_by(month_yr) %>%
  summarize(national_hosps = sum(monthly_hospitalization))

us_monthly_ts = us_monthly_cases %>%
  left_join(us_monthly_deaths, by = "month_yr") %>%
  left_join(us_monthly_hosps, by = "month_yr") %>%
  rename(Cases = natl_cases,
         Deaths = natl_deaths,
         `Hosps.` = national_hosps)

# Quick plot to check trajectories ---------------------------------------------
us_monthly_ts %>%
  ggplot(aes(x = ym(month_yr))) +
  geom_line(aes(y = Cases, color = "Cases")) +
  geom_line(aes(y = Deaths, color = "Deaths")) +
  geom_line(aes(y = `Hosps.`, color = "Hospitalizations")) +
  scale_color_manual(values = c("Cases" = "dodgerblue2",
                                "Deaths" = "gold2",
                                "Hospitalizations" = "violet"))
# We can still compute the correlations but we have missing data - make the first
# figure version with pairwise obs only

# Create the lag columns for each ----------------------------------------------
us_monthly_mat = us_monthly_ts %>%
  mutate(`Cases (-1)` = lag(Cases, 1),
         `Cases (-2)` = lag(Cases, 2),
         `Cases (+1)` = lead(Cases, 1),
         `Cases (+2)` = lead(Cases, 2),
         `Deaths (-1)` = lag(Deaths, 1),
         `Deaths (-2)` = lag(Deaths, 2),
         `Deaths (+1)` = lead(Deaths, 1),
         `Deaths (+2)` = lead(Deaths, 2),
         `Hosps. (-1)`= lag(`Hosps.`, 1),
         `Hosps. (-2)` = lag(`Hosps.`, 2),
         `Hosps. (+1)` = lead(`Hosps.`, 1),
         `Hosps. (+2)`= lead(`Hosps.`, 2)) %>%
  select(-month_yr)

us_monthly_cor_mat = cor(us_monthly_mat, use = "complete.obs")

var_order = c("Cases", "Deaths", "Hosps.", "Cases (-1)", "Deaths (-1)",
              "Hosps. (-1)", "Cases (-2)", "Deaths (-2)", "Hosps. (-2)",
              "Cases (+1)", "Deaths (+1)", "Hosps. (+1)", "Cases (+2)",
              "Deaths (+2)", "Hosps. (+2)")

us_monthly_cor_mat_ordered = us_monthly_cor_mat[var_order, var_order]
us_monthly_cor_mat_ordered[upper.tri(us_monthly_cor_mat_ordered)] = NA

melted_us_monthly_cor_mat = reshape2::melt(us_monthly_cor_mat_ordered, na.rm = TRUE)

# National plot ---------------------------------------------------------------
ggplot(melted_us_monthly_cor_mat,
       aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 1.75) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation\n") +
  theme_minimal() +
  coord_fixed() +
  labs(x = "",
       y = "",
       title = "National-level Correlations of Disease Severity Measures") +
   theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1, "lines"),
    legend.position="right")

ggsave(filename = "figures/for_manuscript/cor_heatmap_us.png", height = 4,
       width = 6.5, units = "in", bg = "white")

# National plot with cases as the reference signal -----------------------------
melted_us_monthly_cor_mat %>%
  filter(Var2 == "Cases", Var1 != "Cases") %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 1.75) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation\n") +
  theme_minimal() +
  coord_fixed() +
  labs(x = "",
       y = "",
       title = "National-level Correlations of Disease\nSeverity Measures with Case Counts") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1, "lines"),
    legend.position="right")

ggsave(filename = "figures/for_manuscript/cor_heatmap_us_cases_ref.png", height = 4,
       width = 6.5, units = "in", bg = "white")


# Plots for each state ---------------------------------------------------------

unique_states = cases %>% select(state_code) %>% unique() %>% pull()

state_lookup = c(
  Alabama = "AL",
  Alaska = "AK",
  Arizona = "AZ",
  Arkansas = "AR",
  California = "CA",
  Colorado = "CO",
  Connecticut = "CT",
  Delaware = "DE",
  Florida = "FL",
  Georgia = "GA",
  Hawaii = "HI",
  Idaho = "ID",
  Illinois = "IL",
  Indiana = "IN",
  Iowa = "IA",
  Kansas = "KS",
  Kentucky = "KY",
  Louisiana = "LA",
  Maine = "ME",
  Maryland = "MD",
  Massachusetts = "MA",
  Michigan = "MI",
  Minnesota = "MN",
  Mississippi = "MS",
  Missouri = "MO",
  Montana = "MT",
  Nebraska = "NE",
  Nevada = "NV",
  `New Hampshire` = "NH",
  `New Jersey` = "NJ",
  `New Mexico` = "NM",
  `New York` = "NY",
  `North Carolina` = "NC",
  `North Dakota` = "ND",
  Ohio = "OH",
  Oklahoma = "OK",
  Oregon = "OR",
  Pennsylvania = "PA",
  `Rhode Island` = "RI",
  `South Carolina` = "SC",
  `South Dakota` = "SD",
  Tennessee = "TN",
  Texas = "TX",
  Utah = "UT",
  Vermont = "VT",
  Virginia = "VA",
  Washington = "WA",
  `West Virginia` = "WV",
  Wisconsin = "WI",
  Wyoming = "WY"
)

state_lookup_from_abb = c(
  AL = "Alabama",
  AK = "Alaska",
  AZ = "Arizona",
  AR = "Arkansas",
  CA = "California",
  CO = "Colorado",
  CT = "Connecticut",
  DE = "Delaware",
  DC = "District of Columbia",
  FL = "Florida",
  GA = "Georgia",
  HI = "Hawaii",
  ID = "Idaho",
  IL = "Illinois",
  IN = "Indiana",
  IA = "Iowa",
  KS = "Kansas",
  KY = "Kentucky",
  LA = "Louisiana",
  ME = "Maine",
  MD = "Maryland",
  MA = "Massachusetts",
  MI = "Michigan",
  MN = "Minnesota",
  MS = "Mississippi",
  MO = "Missouri",
  MT = "Montana",
  NE = "Nebraska",
  NV = "Nevada",
  NH = "New Hampshire",
  NJ = "New Jersey",
  NM = "New Mexico",
  NY = "New York",
  NC = "North Carolina",
  ND = "North Dakota",
  OH = "Ohio",
  OK = "Oklahoma",
  OR = "Oregon",
  PA = "Pennsylvania",
  RI = "Rhode Island",
  SC = "South Carolina",
  SD = "South Dakota",
  TN = "Tennessee",
  TX = "Texas",
  UT = "Utah",
  VT = "Vermont",
  VA = "Virginia",
  WA = "Washington",
  WV = "West Virginia",
  WI = "Wisconsin",
  WY = "Wyoming"
)

deaths %>% filter(State == "North Dakota")

deaths = deaths %>% mutate(state_code = state_lookup[State])

for(state in unique_states[!unique_states == "DC"]){
    tmp_state_cases = cases %>% select(month_yr, state_code, case_cnt) %>%
    filter(state_code == state)

  tmp_state_deaths = deaths %>% filter(Wave == "mid_month") %>%
    select(End_Date, death_agg30, state_code) %>%
    filter(state_code == state) %>%
    mutate(month_yr = sprintf("%04d-%02d", year(End_Date), month(End_Date))) %>%
    select(month_yr, death_agg30)

  tmp_state_hosps = htmp_state_hosps = htmp_state_hosps = hosps %>%
    rename(month_yr = mnth_yr) %>%
    select(month_yr, state_code, monthly_hospitalization) %>%
    filter(state_code == state)

  tmp_state_merged = tmp_state_cases %>%
    left_join(tmp_state_deaths, by = "month_yr") %>%
    left_join(tmp_state_hosps, by = "month_yr") %>%
    mutate(month_yr = ym(month_yr)) %>%
    select(month_yr, case_cnt, death_agg30, monthly_hospitalization) %>%
    rename(Cases = case_cnt,
           Deaths = death_agg30,
           `Hosps.` = monthly_hospitalization)

  tmp_mat = tmp_state_merged %>%
    mutate(`Cases (-1)` = lag(Cases, 1),
           `Cases (-2)` = lag(Cases, 2),
           `Cases (+1)` = lead(Cases, 1),
           `Cases (+2)` = lead(Cases, 2),
           `Deaths (-1)` = lag(Deaths, 1),
           `Deaths (-2)` = lag(Deaths, 2),
           `Deaths (+1)` = lead(Deaths, 1),
           `Deaths (+2)` = lead(Deaths, 2),
           `Hosps. (-1)`= lag(`Hosps.`, 1),
           `Hosps. (-2)` = lag(`Hosps.`, 2),
           `Hosps. (+1)` = lead(`Hosps.`, 1),
           `Hosps. (+2)`= lead(`Hosps.`, 2)) %>%
    select(-month_yr)

  tmp_mat_cor = tryCatch({
    cor(tmp_mat, use = "complete.obs")
  }, error = function(e){
    cor(tmp_mat, use = "pairwise.complete.obs")
  })

  var_order = c("Cases", "Deaths", "Hosps.", "Cases (-1)", "Deaths (-1)",
                "Hosps. (-1)", "Cases (-2)", "Deaths (-2)", "Hosps. (-2)",
                "Cases (+1)", "Deaths (+1)", "Hosps. (+1)", "Cases (+2)",
                "Deaths (+2)", "Hosps. (+2)")

  tmp_mat_cor_ordered = tmp_mat_cor[var_order, var_order]
  tmp_mat_cor_ordered[upper.tri(tmp_mat_cor_ordered)] = NA
  melted_tmp_mat_cor = reshape2::melt(tmp_mat_cor_ordered, na.rm = TRUE)

  ggplot(melted_tmp_mat_cor,
         aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 1.75) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name="Correlation\n") +
    theme_minimal() +
    coord_fixed() +
    labs(x = "",
         y = "",
         title = paste0("Correlations of Disease Severity\nMeasures in ",state_lookup_from_abb[state])) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(1, "lines"),
      legend.position="right")

  ggsave(filename = paste0("figures/for_manuscript/state_severity_heatmaps/",state,".png"),
         height = 4, width = 6.5, units = "in", bg = "white")

  print(paste0("State ",state," completed"))

}

# Big heatmap with cases for each state ----------------------------------------

# First build the dataset - we need to combine national level row with the case
# row for each state

us_corplot_row_dat = melted_us_monthly_cor_mat %>% filter(Var2 == "Cases", Var1 != "Cases") %>%
  mutate(region = "US")

state_corplot_rows = NULL
for(state in unique_states[!unique_states == "DC"]){
  tmp_state_cases = cases %>% select(month_yr, state_code, case_cnt) %>%
    filter(state_code == state)

  tmp_state_deaths = deaths %>% filter(Wave == "mid_month") %>%
    select(End_Date, death_agg30, state_code) %>%
    filter(state_code == state) %>%
    mutate(month_yr = sprintf("%04d-%02d", year(End_Date), month(End_Date))) %>%
    select(month_yr, death_agg30)

  tmp_state_hosps = htmp_state_hosps = htmp_state_hosps = hosps %>%
    rename(month_yr = mnth_yr) %>%
    select(month_yr, state_code, monthly_hospitalization) %>%
    filter(state_code == state)

  tmp_state_merged = tmp_state_cases %>%
    left_join(tmp_state_deaths, by = "month_yr") %>%
    left_join(tmp_state_hosps, by = "month_yr") %>%
    mutate(month_yr = ym(month_yr)) %>%
    select(month_yr, case_cnt, death_agg30, monthly_hospitalization) %>%
    rename(Cases = case_cnt,
           Deaths = death_agg30,
           `Hosps.` = monthly_hospitalization)

  tmp_mat = tmp_state_merged %>%
    mutate(`Cases (-1)` = lag(Cases, 1),
           `Cases (-2)` = lag(Cases, 2),
           `Cases (+1)` = lead(Cases, 1),
           `Cases (+2)` = lead(Cases, 2),
           `Deaths (-1)` = lag(Deaths, 1),
           `Deaths (-2)` = lag(Deaths, 2),
           `Deaths (+1)` = lead(Deaths, 1),
           `Deaths (+2)` = lead(Deaths, 2),
           `Hosps. (-1)`= lag(`Hosps.`, 1),
           `Hosps. (-2)` = lag(`Hosps.`, 2),
           `Hosps. (+1)` = lead(`Hosps.`, 1),
           `Hosps. (+2)`= lead(`Hosps.`, 2)) %>%
    select(-month_yr)

  tmp_mat_cor = tryCatch({
    cor(tmp_mat, use = "complete.obs")
  }, error = function(e){
    cor(tmp_mat, use = "pairwise.complete.obs")
  })

  var_order = c("Cases", "Deaths", "Hosps.", "Cases (-1)", "Deaths (-1)",
                "Hosps. (-1)", "Cases (-2)", "Deaths (-2)", "Hosps. (-2)",
                "Cases (+1)", "Deaths (+1)", "Hosps. (+1)", "Cases (+2)",
                "Deaths (+2)", "Hosps. (+2)")

  tmp_mat_cor_ordered = tmp_mat_cor[var_order, var_order]
  tmp_mat_cor_ordered[upper.tri(tmp_mat_cor_ordered)] = NA
  melted_tmp_mat_cor = reshape2::melt(tmp_mat_cor_ordered, na.rm = TRUE)
  tmp_corplot_state_row = melted_tmp_mat_cor %>% filter(Var2 == "Cases", Var1 != "Cases") %>%
    mutate(region = state)

  state_corplot_rows = bind_rows(state_corplot_rows, tmp_corplot_state_row)

  print(paste0("State ",state," completed"))

}

corplot_rows_all = bind_rows(us_corplot_row_dat, state_corplot_rows)

corplot_rows_all$region = factor(corplot_rows_all$region, levels = rev(c("US", sort(unique(corplot_rows_all$region[corplot_rows_all$region != "US"])))))

all_var1_values = unique(corplot_rows_all$Var1)

all_var1_values = all_var1_values[-1]
all_var1_values

corplot_rows_all = corplot_rows_all %>%
  tidyr::complete(Var1 = all_var1_values, region = unique(corplot_rows_all$region), fill = list(value = NA))
corplot_rows_all = corplot_rows_all %>% filter(Var1 != "Cases")

# Heatmap ----------------------------------------------------------------------

corplot_rows_all %>%
  filter(!is.na(value)) %>%
  ggplot(aes(Var1, region, fill = value)) +
  geom_tile(color = "white", height = 1) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation\n") +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "National and State-Level Correlations of\nDisease Severity Metrics with Case Counts") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

ggsave("figures/for_manuscript/national_and_state_case_corplot.png", width = 10, height = 15, units = "in", bg = "white")
