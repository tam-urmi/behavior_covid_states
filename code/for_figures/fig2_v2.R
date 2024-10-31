## -----------------------------------------------------------------------------
## Script name: fig2_v2.R
##
## Purpose of script: Generate corrplots for severity and behaviors
##
## Author: George Dewey
##
## Date Created: 2024-10-25
##
## Last Updated: 2024-10-25
## -----------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(patchwork)

# Generate three separate figures and combine
behavior = read_csv("data/processed/behavior_wave_interpolated_all_state.csv")
behavior %>% filter(state == "National") %>%
  left_join(data_mortality %>% filter(State == "National"), by = "End_Date") %>% view()

# Mortality
data_mortality = read_csv("data/processed/death_monthly_state.csv")
aggregated_deaths = data_mortality %>% filter(State == "National") %>%
  select(End_Date, death_agg30)

# Merge for correlation plot
behavior_n = behavior %>% filter(state == "National")

behavior_corr = behavior_n %>% left_join(aggregated_deaths, by = "End_Date") %>%
  select(End_Date, death_agg30, `Avoiding contact with other people`:`Been in a room with over 50 people outside of household in the past 24 hours`)

behavior_corr = behavior_corr %>% filter(End_Date  == ceiling_date(End_Date, "month") - days(1),
                         End_Date > as_date("2020-03-31"), End_Date <= as_date("2022-07-01"))

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

cor(behavior_corr$death_agg30, lag(behavior_corr$`Avoiding contact with other people`, 1), use = "pairwise.complete.obs")
cor(behavior_corr$death_agg30, behavior_corr$`Avoiding contact with other people`)

# Generate lags and leads for each behavior column
data2 = behavior_corr %>%
  mutate(across(all_of(behavior_columns),
                .fns = list(lag2 = ~lag(.x, 2),
                            lag1 = ~lag(.x, 1),
                            lead1 = ~lead(.x, 1),
                            lead2 = ~lead(.x, 2)),
                .names = "{col}_{fn}"))

data2
# Select relevant columns for correlation
correlation_data <- data2 %>%
  select(death_agg30,
         contains("lag2"), contains("lag1"),
         `Avoiding contact with other people`,
         `Avoiding public or crowded places`,
         `Frequently washing hands`,
         `Wearing a face mask when outside of your home`,
         `Go to work`,
         `Go to the gym`,
         `Go visit a friend`,
         `Go to a cafe, bar, or restaurant`,
         `Go to a doctor or visit a hospital`,
         `Go to church or another place of worship`,
         `Take mass transit (e.g. subway, bus, or train)`,
         `Been in a room with someone outside of household in the past 24 hours`,
         `Been in a room with 5-10 people outside of household in the past 24 hours`,
         `Been in a room with 11-50 people outside of household in the past 24 hours`,
         `Been in a room with over 50 people outside of household in the past 24 hours`,
         contains("lead1"), contains("lead2"))

correlation_data

# Compute the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Melt the correlation matrix and add custom columns for easier plotting
melted_correlation_matrix <- melt(correlation_matrix)

# Split the column names for behaviors and time shifts
melted_correlation_matrix <- melted_correlation_matrix %>%
  mutate(
    Behavior = case_when(
      grepl("Avoiding contact", Var2) ~ "Avoiding contact",
      grepl("Avoiding public", Var2) ~ "Avoiding public places",
      grepl("Frequently washing hands", Var2) ~ "Washing hands",
      grepl("Wearing a face mask", Var2) ~ "Wearing a face mask",
      grepl("Go to work", Var2) ~ "Go to work",
      grepl("Go to the gym", Var2) ~ "Go to the gym",
      grepl("Go visit a friend", Var2) ~ "Go visit a friend",
      grepl("Go to a cafe", Var2) ~ "Go to a cafe, bar, or restaurant",
      grepl("Go to a doctor", Var2) ~ "Go to doctor/hospital",
      grepl("Go to church", Var2) ~ "Go to church",
      grepl("Take mass transit", Var2) ~ "Take mass transit",
      grepl("Been in a room with someone outside", Var2) ~ "Room with someone",
      grepl("Been in a room with 5-10 people", Var2) ~ "Room with 5-10 people",
      grepl("Been in a room with 11-50 people", Var2) ~ "Room with 11-50 people",
      grepl("Been in a room with over 50 people", Var2) ~ "Room with over 50 people"
    ),
    Time_shift = case_when(
      grepl("lag2", Var2) ~ "Lag-2",
      grepl("lag1", Var2) ~ "Lag-1",
      grepl("lead1", Var2) ~ "Lead+1",
      grepl("lead2", Var2) ~ "Lead+2",
      grepl("death", Var1) ~ "Lag0"  # Current value
    )
  )


# Ensure Time_shift is ordered correctly
melted_correlation_matrix$Time_shift <- factor(melted_correlation_matrix$Time_shift,
                                               levels = c("Lag-2", "Lag-1", "Lag0", "Lead+1", "Lead+2"))

# Filter the correlation matrix to exclude unnecessary comparisons
filtered_correlation_matrix <- melted_correlation_matrix %>%
  filter(Var1 == "death_agg30") %>%
  select(Behavior, Time_shift, value)

desired_order <- rev(c(
  "Avoiding contact",
  "Avoiding public places",
  "Washing hands",
  "Wearing a face mask",
  "Go to work",
  "Go to the gym",
  "Go visit a friend",
  "Go to a cafe, bar, or restaurant",
  "Go to doctor/hospital",
  "Go to church",
  "Take mass transit",
  "Room with someone",
  "Room with 5-10 people",
  "Room with 11-50 people",
  "Room with over 50 people"
))
# Ensure the Behavior column follows the desired order
filtered_correlation_matrix$Behavior <- factor(filtered_correlation_matrix$Behavior,
                                               levels = desired_order)

# Plot the heatmap with behaviors as rows and time shifts as columns, adding correlation values as text
ggplot(filtered_correlation_matrix %>% drop_na(), aes(x = Time_shift, y = Behavior, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +  # Add correlation values on tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1)) +
  labs(title = "Heatmap of Correlations between Mortality and Behaviors (Lag -2 to +2)",
       x = "Time Shift", y = "Behavior")

# Hospitalizations
data1 = readxl::read_xlsx("~/Downloads/Corrcheck (1).xlsx", sheet = "Sheet1", n_max = 25)

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
data2 <- data1 %>%
  arrange(Date) %>%
  mutate(across(all_of(behavior_columns),
                .fns = list(lag2 = ~lag(.x, 2),
                            lag1 = ~lag(.x, 1),
                            lead1 = ~lead(.x, 1),
                            lead2 = ~lead(.x, 2)),
                .names = "{col}_{fn}"))

data2

names(data2)

# Select relevant columns for correlation
correlation_data <- data2 %>%
  select(Hosp,
         contains("lag2"), contains("lag1"),
         `Avoiding contact with other people`,
         `Avoiding public or crowded places`,
         `Frequently washing hands`,
         `Wearing a face mask when outside of your home`,
         `Go to work`,
         `Go to the gym`,
         `Go visit a friend`,
         `Go to a cafe, bar, or restaurant`,
         `Go to a doctor or visit a hospital`,
         `Go to church or another place of worship`,
         `Take mass transit (e.g. subway, bus, or train)`,
         `Been in a room with someone outside of household in the past 24 hours`,
         `Been in a room with 5-10 people outside of household in the past 24 hours`,
         `Been in a room with 11-50 people outside of household in the past 24 hours`,
         `Been in a room with over 50 people outside of household in the past 24 hours`,
         contains("lead1"), contains("lead2"))

correlation_data

# Compute the correlation matrix
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

correlation_matrix[,"Go to a cafe, bar, or restaurant"]

# Melt the correlation matrix and add custom columns for easier plotting
melted_correlation_matrix <- melt(correlation_matrix)

# Split the column names for behaviors and time shifts
melted_correlation_matrix <- melted_correlation_matrix %>%
  mutate(
    Behavior = case_when(
      grepl("Avoiding contact", Var2) ~ "Avoiding contact",
      grepl("Avoiding public", Var2) ~ "Avoiding public places",
      grepl("Frequently washing hands", Var2) ~ "Washing hands",
      grepl("Wearing a face mask", Var2) ~ "Wearing a face mask",
      grepl("Go to work", Var2) ~ "Go to work",
      grepl("Go to the gym", Var2) ~ "Go to the gym",
      grepl("Go visit a friend", Var2) ~ "Go visit a friend",
      grepl("Go to a cafe", Var2) ~ "Go to a cafe, bar, or restaurant",
      grepl("Go to a doctor", Var2) ~ "Go to doctor/hospital",
      grepl("Go to church", Var2) ~ "Go to church",
      grepl("Take mass transit", Var2) ~ "Take mass transit",
      grepl("Been in a room with someone outside", Var2) ~ "Room with someone",
      grepl("Been in a room with 5-10 people", Var2) ~ "Room with 5-10 people",
      grepl("Been in a room with 11-50 people", Var2) ~ "Room with 11-50 people",
      grepl("Been in a room with over 50 people", Var2) ~ "Room with over 50 people"
    ),
    Time_shift = case_when(
      grepl("lag2", Var2) ~ "Lag-2",
      grepl("lag1", Var2) ~ "Lag-1",
      grepl("lead1", Var2) ~ "Lead+1",
      grepl("lead2", Var2) ~ "Lead+2",
      grepl("Hosp", Var1) ~ "Lag0"  # Current value
    )
  )

melted_correlation_matrix

# Ensure Time_shift is ordered correctly
melted_correlation_matrix$Time_shift <- factor(melted_correlation_matrix$Time_shift,
                                               levels = c("Lag-2", "Lag-1", "Lag0", "Lead+1", "Lead+2"))

# Filter the correlation matrix to exclude unnecessary comparisons
filtered_correlation_matrix <- melted_correlation_matrix %>%
  filter(Var1 == "Hosp") %>%
  select(Behavior, Time_shift, value)

filtered_correlation_matrix

desired_order <- rev(c(
  "Avoiding contact",
  "Avoiding public places",
  "Washing hands",
  "Wearing a face mask",
  "Go to work",
  "Go to the gym",
  "Go visit a friend",
  "Go to a cafe, bar, or restaurant",
  "Go to doctor/hospital",
  "Go to church",
  "Take mass transit",
  "Room with someone",
  "Room with 5-10 people",
  "Room with 11-50 people",
  "Room with over 50 people"
))
# Ensure the Behavior column follows the desired order
filtered_correlation_matrix$Behavior <- factor(filtered_correlation_matrix$Behavior,
                                               levels = desired_order)


# Plot the heatmap with behaviors as rows and time shifts as columns, adding correlation values as text
ggplot(filtered_correlation_matrix %>% drop_na(), aes(x = Time_shift, y = Behavior, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +  # Add correlation values on tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1)) +
  labs(title = "Heatmap of Correlations between Hosp and Behaviors (Lag -2 to +2)",
       x = "Time Shift", y = "Behavior")