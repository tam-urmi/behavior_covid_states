## -----------------------------------------------------------------------------
## Script name: decomposition_validation.R
##
## Purpose of script: Validation of behavior trend oscillations and computation
## of correlations
##
## Author: George Dewey
##
## Date Created: 2024-10-28
##
## Last Updated: 2024-10-28
## -----------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(reshape2)

# Load data

# Load the interpolated behavior data
behavior_dat = read_csv("data/processed/behavior_wave_interpolated_all_state.csv")

# National-level
behavior_national = behavior_dat %>%
  filter(state == "National")

# Filter data points to only use end-of-month (EoM points)
behavior_time_filtered = behavior_national %>%
  filter(End_Date  == ceiling_date(End_Date, "month") - days(1),
         End_Date <= "2022-07-01",
         End_Date > "2020-04-01") %>%
  select(-c(state, Total_N, new_ind))

behavior_time_filtered

# Do the decomposition
behavior_time_filtered = behavior_time_filtered %>%
  mutate(date_num = as.numeric(End_Date),
         date_index = 1:nrow(behavior_time_filtered))

names(behavior_time_filtered) = c("wave", "end_date", "avoid_contact", "avoid_crowds", "wash_hands", "wear_mask",
                                  "work", "gym", "friend", "cafe", "doctor", "church", "transit", "room", "room_5_10",
                                  "room_11_50", "room_50_plus", "date_num", "date_index")
behavior_cols = names(behavior_time_filtered)[3:(ncol(behavior_time_filtered)-2)]

# Also, do the regression on evenly-spaced x values (assuming the x-unit is months)
get_oscillation = function(data, col){
  model = lm(reformulate("date_num", response = col), data = data)
  residuals = residuals(model)
  return(residuals)
}

get_oscillation_from_index = function(data, col){
  model = lm(reformulate("date_index", response = col), data = data)
  residuals = residuals(model)
  return(residuals)
}

behavior_time_filtered

behavior_oscillations = behavior_time_filtered %>%
  mutate(across(all_of(behavior_cols),
                ~ get_oscillation(behavior_time_filtered, cur_column()),
                .names = "residual_{.col}")) %>%
  select(wave, end_date, starts_with("residual_"))

behavior_oscillations_index = behavior_time_filtered %>%
  mutate(across(all_of(behavior_cols),
                ~ get_oscillation_from_index(behavior_time_filtered, cur_column()),
                .names = "residual_{.col}")) %>%
  select(wave, end_date, starts_with("residual_"))

oscillations_severity_index %>%
  select(end_date, deaths, residual_avoid_contact)

# Merge severity and compare at the national level

# Mortality
data_mortality = read_csv("data/processed/death_monthly_state.csv", show_col_types = FALSE)
aggregated_deaths = data_mortality %>% filter(State == "National") %>%
  select(End_Date, death_agg30)

# Hospitalizations
data_hosp = readxl::read_xlsx("~/Downloads/Corrcheck (1).xlsx", sheet = "Sheet1", n_max = 25)

# Cases
data_cases = read_csv("data/all_state_official_case_cnt.csv", show_col_types = FALSE)

nat_deaths =
  aggregated_deaths %>% filter(End_Date  == ceiling_date(End_Date, "month") - days(1)) %>%
  rename(end_date = End_Date,
         deaths = death_agg30)

nat_hosps =
  data_hosp %>%
  select(Date, Hosp) %>%
  mutate(date = ymd(paste0(Date, "-01")),   # Parse to the first day of the month
         date = ceiling_date(date, "month") - days(1)) %>%
  select(date, Hosp) %>%
  rename(hosps = Hosp)

nat_cases = data_cases %>%
  select(month_yr, state_code, case_cnt) %>%
  group_by(month_yr) %>%
  summarize(cases = sum(case_cnt)) %>%
  mutate(date = ymd(paste0(month_yr, "-01")),   # Parse to the first day of the month
         date = ceiling_date(date, "month") - days(1)) %>%
  select(date, cases)

# Create a dataset that is the raw behavior + severity only
behavior_severity = behavior_time_filtered %>%
  left_join(nat_deaths, by = "end_date") %>%
  left_join(nat_hosps, by = c("end_date" = "date")) %>%
  left_join(nat_cases, by = c("end_date" = "date")) %>%
  select(wave, end_date, deaths, hosps, cases, everything())

write_csv(behavior_severity, file = "data/behavior_severity.csv")

# Merge the national-level behavior oscillations and all 3 severity measures
oscillations_severity = behavior_oscillations %>%
  left_join(nat_deaths, by = "end_date") %>%
  left_join(nat_hosps, by = c("end_date" = "date")) %>%
  left_join(nat_cases, by = c("end_date" = "date")) %>%
  select(wave, end_date, deaths, hosps, cases, everything())

# Confirm results?
oscillations_severity_index = behavior_oscillations_index %>%
  left_join(nat_deaths, by = "end_date") %>%
  left_join(nat_hosps, by = c("end_date" = "date")) %>%
  left_join(nat_cases, by = c("end_date" = "date")) %>%
  select(wave, end_date, deaths, hosps, cases, everything())


tmp = behavior_time_filtered %>%
  select(date_index, avoid_contact)
m = lm(avoid_contact ~ date_index, tmp)
summary(m)
m$residuals
reo## Figures --------------------------------------------------------------------

# Replicate figure 1
raw_behavior_deaths = behavior_time_filtered %>%
  left_join(nat_deaths, by = "end_date") %>%
  mutate(date_index = row_number()) %>%
  select(wave, end_date, date_index, deaths, everything())

# Panel A - Avoid contact vs mortality
p1 = raw_behavior_deaths %>%
  ggplot(aes(x = date_index)) +
  geom_line(aes(y = deaths, color = "Mortality")) +
  geom_line(aes(y = avoid_contact * (max(deaths)/max(avoid_contact)), color = "Risk-averting behavior")) +
  scale_y_continuous("Mortality", sec.axis = sec_axis(~./(max(raw_behavior_deaths$deaths)/max(raw_behavior_deaths$avoid_contact)), name = "% Adherence")) +
  scale_color_manual(NULL, values = c("Mortality" = "black", "Risk-averting behavior" = "orange")) +
  theme_minimal() +
  labs(x = "", title = "Avoiding contact with other people") +
  theme(axis.title.y.right = element_text(color = "orange"),
        axis.text.y.right = element_text(color = "orange"),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

# Panel B: Linear regression vs data
p2 = raw_behavior_deaths %>%
  ggplot(aes(x = date_index)) +
  geom_point(aes(y = avoid_contact, color = "Risk-averting behavior")) +
  geom_smooth(aes(y = avoid_contact, color = "Risk-averting behavior"), method = "lm", se = FALSE) +
  scale_color_manual(NULL, values = c("Risk-averting behavior" = "orange")) +
  theme_minimal() +
  labs(x = "", y = "% Adherence", title = "Linear Component") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none"
        )

# Panel C: Oscillation vs. mortality
oscillations_severity_index %>%
  mutate(date_index = row_number()) %>%
  ggplot(aes(x = date_index)) +
  # Scale residual_avoid_contact to match the range of deaths
  geom_line(aes(y = residual_avoid_contact * (max(deaths) / max(residual_avoid_contact)), color = "Risk-averting behavior")) +
  geom_line(aes(y = deaths, color = "Mortality")) +
  # Set up the y-axis and secondary axis for proper scaling
  scale_y_continuous(
    "Mortality",
    sec.axis = sec_axis(~ . / (max(oscillations_severity_index$deaths) / max(oscillations_severity_index$residual_avoid_contact)), name = "% Residual Adherence")
  ) +
  scale_color_manual(
    NULL,
    values = c("Mortality" = "black", "Risk-averting behavior" = "orange")
  ) +
  theme_minimal() +
  labs(x = "", title = "Oscillation") +
  theme(
    axis.title.y.right = element_text(color = "orange"),
    axis.text.y.right = element_text(color = "orange"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

# Panel C (version 2)
p3 = oscillations_severity_index %>%
  mutate(date_index = row_number()) %>%
  ggplot(aes(x = date_index)) +
  # Scale residual_avoid_contact to match the range of deaths
  geom_line(aes(y = residual_avoid_contact/max(residual_avoid_contact), color = "Risk-averting behavior")) +
  geom_line(aes(y = deaths/max(deaths) - 0.5, color = "Mortality")) +
  # Set up the y-axis and secondary axis for proper scaling
  # scale_y_continuous(
  #   "Mortality",
  #   sec.axis = sec_axis(~ . / (max(oscillations_severity_index$deaths) / max(oscillations_severity_index$residual_avoid_contact)), name = "% Residual Adherence")
  # ) +
  scale_color_manual(
    NULL,
    values = c("Mortality" = "black", "Risk-averting behavior" = "orange")
  ) +
  theme_minimal() +
  labs(x = "", y = "% of max signal", title = "Oscillation") +
  theme(
    axis.title.y.right = element_text(color = "orange"),
    axis.text.y.right = element_text(color = "orange"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

# Panel D: Go visit a friend (raw) vs. mortality
p4 = raw_behavior_deaths %>%
  ggplot(aes(x = date_index)) +
  geom_line(aes(y = deaths, color = "Mortality")) +
  geom_line(aes(y = friend * (max(deaths)/max(friend)), color = "Risk-exposing behavior")) +
  scale_y_continuous("Mortality", sec.axis = sec_axis(~./(max(raw_behavior_deaths$deaths)/max(raw_behavior_deaths$friend)), name = "% Adherence")) +
  scale_color_manual(NULL, values = c("Mortality" = "black", "Risk-exposing behavior" = "darkgreen")) +
  theme_minimal() +
  labs(x = "", title = "Go visit a friend") +
  theme(axis.title.y.right = element_text(color = "darkgreen"),
        axis.text.y.right = element_text(color = "darkgreen"),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# Panel E: Go visit a friend linear component
p5 = raw_behavior_deaths %>%
  ggplot(aes(x = date_index)) +
  geom_point(aes(y = friend, color = "Risk-averting behavior")) +
  geom_smooth(aes(y = friend, color = "Risk-averting behavior"), method = "lm", se = FALSE) +
  scale_color_manual(NULL, values = c("Risk-averting behavior" = "darkgreen")) +
  theme_minimal() +
  labs(x = "", y = "% Adherence", title = "Linear Component") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none"
  )

# Panel F: Go visit a friend oscillatory component
oscillations_severity_index %>%
  mutate(date_index = row_number()) %>%
  ggplot(aes(x = date_index)) +
  # Scale residual_avoid_contact to match the range of deaths
  geom_line(aes(y = residual_friend * (max(deaths) / max(residual_friend)), color = "Risk-exposing behavior")) +
  geom_line(aes(y = deaths, color = "Mortality")) +
  # Set up the y-axis and secondary axis for proper scaling
  scale_y_continuous(
    "Mortality",
    sec.axis = sec_axis(~ . / (max(oscillations_severity_index$deaths) / max(oscillations_severity_index$residual_friend)), name = "% Residual Adherence")
  ) +
  scale_color_manual(
    NULL,
    values = c("Mortality" = "black", "Risk-exposing behavior" = "darkgreen")
  ) +
  theme_minimal() +
  labs(x = "", title = "Oscillation") +
  theme(
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

# Panel F (version 2)
p6 = oscillations_severity_index %>%
  mutate(date_index = row_number()) %>%
  ggplot(aes(x = date_index)) +
  # Scale residual_avoid_contact to match the range of deaths
  geom_line(aes(y = residual_friend / max(residual_friend), color = "Risk-exposing behavior")) +
  geom_line(aes(y = deaths / max(deaths) - 0.5, color = "Mortality")) +
  # Set up the y-axis and secondary axis for proper scaling
  # scale_y_continuous(
  #   "Mortality",
  #   sec.axis = sec_axis(~ . / (max(oscillations_severity_index$deaths) / max(oscillations_severity_index$residual_friend)), name = "% Residual Adherence")
  # ) +
  scale_color_manual(
    NULL,
    values = c("Mortality" = "black", "Risk-exposing behavior" = "darkgreen")
  ) +
  theme_minimal() +
  labs(x = "", y = "% of max signal", title = "Oscillation") +
  theme(
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

# Combine
(p1 + p2 + p3) / (p4 + p5 + p6) +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "none")

# Create the big correlation matrix

behavior_columns <- c(
  "Avoiding contact with other people",
  "Avoiding public or crowded places",
  "Frequently washing hands",
  "Wearing a face mask when outside of your home",
  "Go to work",
  "Go to the gym",
  "Go visit a friend",
  "Go to a cafe, bar, or restaurant",
  "Go to a doctor or visit a hospital",
  "Go to church or another place of worship",
  "Take mass transit (e.g. subway, bus, or train)",
  "Been in a room with someone outside of household in the past 24 hours",
  "Been in a room with 5-10 people outside of household in the past 24 hours",
  "Been in a room with 11-50 people outside of household in the past 24 hours",
  "Been in a room with over 50 people outside of household in the past 24 hours"
)

# Generate lags and leads for each behavior column
data2 <- oscillations_severity_index %>%
  arrange(end_date) %>%
  mutate(across(starts_with("residual"),
                .fns = list(lag2 = ~lag(.x, 2),
                            lag1 = ~lag(.x, 1),
                            lead1 = ~lead(.x, 1),
                            lead2 = ~lead(.x, 2)),
                .names = "{col}_{fn}"))

# Define the new order of base behaviors
ordered_behaviors <- c(
  "avoid_contact", "avoid_crowds", "wash_hands", "wear_mask", "work",
  "gym", "friend", "cafe", "doctor", "church", "transit",
  "room", "room_5_10", "room_11_50", "room_50_plus"
)

# Define nicer labels for the behaviors
nicer_labels <- c(
  "Avoid contact",
  "Avoid crowds",
  "Wash hands frequently",
  "Wear mask outside of your home",
  "Go to work",
  "Go to gym",
  "Go visit a friend",
  "Go to a cafe, bar, or restaurant",
  "Go to a doctor or hospital",
  "Go to a church",
  "Take mass transit",
  "Be in a room with another person from outside of your household",
  "Be in a room with 5-10 people from outside of your household",
  "Be in a room with 11-50 people from outside of your household",
  "Be in a room with more than 50 people from outside of your household"
)

# Generate all behavior column names with lags and leads
behavior_columns <- c()
for (behavior in ordered_behaviors) {
  behavior_columns <- c(behavior_columns,
                        paste0("residual_", behavior, "_lag2"),
                        paste0("residual_", behavior, "_lag1"),
                        paste0("residual_", behavior),
                        paste0("residual_", behavior, "_lead1"),
                        paste0("residual_", behavior, "_lead2"))
}

# Initialize an empty list to store correlation data frames for each severity measure
correlation_list <- list()

# Define severity measures in the required order
severity_measures <- c("deaths", "hosps", "cases")
for (severity in severity_measures) {
  # Data frame to store correlation results for this severity measure
  severity_df <- data.frame(
    Behavior = rep(ordered_behaviors, each = 5),
    LagLead = rep(c("lag2", "lag1", "lag0", "lead1", "lead2"), times = length(ordered_behaviors)),
    Correlation = NA,
    Severity = severity
  )

  # Calculate correlations for each behavior across lags and leads
  for (i in seq_along(ordered_behaviors)) {
    behavior_name <- ordered_behaviors[i]
    lag_lead_cols <- paste0("residual_", behavior_name, c("_lag2", "_lag1", "", "_lead1", "_lead2"))
    correlations <- sapply(lag_lead_cols, function(col) cor(data2[[severity]], data2[[col]], use = "pairwise.complete.obs"))
    severity_df$Correlation[(i - 1) * 5 + 1:5] <- correlations
  }

  # Add the data frame for this severity to the list
  correlation_list[[severity]] <- severity_df
}

# Combine all severity data into a single data frame
correlation_df <- do.call(rbind, correlation_list)

# Create a Row index for plotting, ensuring correct ordering and grouping
correlation_df <- correlation_df %>%
  mutate(
    Row = rep(1:length(ordered_behaviors), each = 5, times = length(severity_measures)) +
      rep((0:(length(severity_measures) - 1)) * length(ordered_behaviors), each = length(ordered_behaviors) * 5),
    Behavior_Label = rep(nicer_labels, each = 5, times = length(severity_measures))
  )

# Ensure correct ordering of LagLead levels and Row for plotting
correlation_df$LagLead <- factor(correlation_df$LagLead, levels = c("lag2", "lag1", "lag0", "lead1", "lead2"))
correlation_df$Severity = factor(correlation_df$Severity, levels = c("deaths", "hosps", "cases"))
correlation_df$Row = rep(sort(rep(15:1, times = 5)), 3)

# Plot the heatmap with custom row and facet labels
ggplot(correlation_df, aes(x = LagLead, y = factor(Row, levels = 15:1), fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text.y = element_text(size = 10, face = "bold")) +  # Ensure facet labels are visible
  labs(title = "Correlation Heatmap of Severity Measures and Behaviors",
       x = "Lag/Lead", y = "Behavior by Severity Measure") +
  facet_grid(Severity ~ ., scales = "free_y", space = "free_y",
             labeller = as_labeller(c(deaths = "Deaths", hosps = "Hospitalizations", cases = "Cases"))) +
  scale_y_discrete(labels = rev(correlation_df$Behavior_Label[!duplicated(correlation_df$Row)]))

tmp = oscillations_severity_index %>%
  mutate(case_cnt_norm = (cases*100/346,037,975))
cor(tmp$residual_avoid_contact, tmp$case_cnt_norm)

a = c(11.89723487,  11.30981993,  11.13580896,  23.63798827,
      20.36661695,  20.57275038,  38.51808303,  83.49743138,
      94.03042388,  82.797839  ,  32.91148872,  27.26280394,
      27.94178672,  14.41456024,   6.28489446,  18.24183386,
      62.37090277,  71.19732654,  49.23228426,  49.37231213,
      91.63315107, 304.18169499,  69.32291033)
b = c(-1.93787681, -4.46736957, -3.9661608 , -3.11502182,  0.49205156,
      4.83645925,  6.165322  ,  5.36449725,  3.98429718,  1.62969551,
      -2.89572501, -7.33760995, -6.6881605 , -0.88006998,  2.17170386,
      -0.11225005, -1.7684836 ,  2.68815753,  6.07707605,  3.40714978,
      -1.83123527, -3.17454547, -0.92379614)
cor(a, b)
