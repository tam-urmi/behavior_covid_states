## -----------------------------------------------------------------------------
## Script name: check_interpolation.R
##
## Purpose of script: To validate the interpolation for aggregated
## CSP behavior data at the national and state levels
##
## Author: George Dewey
##
## Date Created: 2024-10-24
##
## Last Updated: 2024-10-24
## -----------------------------------------------------------------------------

# Setup ---------------------------------------------
# Load packages
library(tidyverse)
library(lubridate)
library(patchwork)
library(reshape2)

# Load data
dat = read_csv("~/Documents/Projects/covid_states_behavior1/data/behaviors_perc_till_wv27_sept30.csv")

# Filter data to only the study period and define average wave data as the data
# from the midpoint of each wave
dat_n = dat %>% filter(state_code == "National", Wave <= 23) %>%
  mutate(wave_midpoint = as_date(as_date(Start_Date) + as.numeric(difftime(as_date(End_Date), as_date(Start_Date), units = "days")/ 2))) %>%
  select(Wave, wave_midpoint, !contains("ave"))

# Record the start and end dates of the study period
start_date = min(dat_n$wave_midpoint)
end_date = as_date(max(dat_n$wave_midpoint))

# Choose the dates we want to interpolate
dates <- seq(from = start_date, to = end_date, by = "day") %>%
  .[day(.) == 15 | day(.) == days_in_month(.)]

dat_n$wave_midpoint_num <- as.numeric(dat_n$wave_midpoint)
dates_num <- as.numeric(dates)

# Interpolation function -------------------------------------------------------

interpolate_data <- function(data, dates_num, column_name, method = "linear") {
  # Convert dates to numeric for interpolation
  x <- data$wave_midpoint_num
  y <- data[[column_name]]

  if (method == "linear") {
    # Linear interpolation using approx
    return(approx(x, y, xout = dates_num)$y)

  } else if (method == "spline") {
    # Cubic spline interpolation using spline function
    return(spline(x, y, xout = dates_num, method = "natural")$y)

  } else if (method == "quadratic") {
    # Quadratic interpolation using a second-degree polynomial fit (doesn't work)
    # Fit the quadratic model
    quadratic_model <- lm(y ~ poly(x, 2))

    # Predict the values at the desired dates using the quadratic model
    predicted_values <- predict(quadratic_model, newdata = data.frame(x = dates_num))

    return(predicted_values)
  }

    else if (method == "lagrangian") {
      # Lagrangian interpolation using a 2nd-degree polynomial (3 points)

      # Create an empty vector for interpolated values
      interpolated_values <- numeric(length(dates_num))

      # Loop over each date to interpolate
      for (i in seq_along(dates_num)) {
        x_new <- dates_num[i]

        # Find the three closest points to the current date for interpolation
        idx <- order(abs(x - x_new))[1:3]
        x1 <- x[idx[1]]
        x2 <- x[idx[2]]
        x3 <- x[idx[3]]
        y1 <- y[idx[1]]
        y2 <- y[idx[2]]
        y3 <- y[idx[3]]

        # Lagrange 2nd-order interpolation formula
        L1 <- y1 * ((x_new - x2) * (x_new - x3)) / ((x1 - x2) * (x1 - x3))
        L2 <- y2 * ((x_new - x1) * (x_new - x3)) / ((x2 - x1) * (x2 - x3))
        L3 <- y3 * ((x_new - x1) * (x_new - x2)) / ((x3 - x1) * (x3 - x2))

        # Interpolated value at x_new
        interpolated_values[i] <- L1 + L2 + L3
      }

      return(interpolated_values)

  } else {
    stop("Invalid method. Choose between 'linear', 'quadratic', 'lagrangian`, or 'spline'.")
  }
}

# Plots ------------------------------------------------------------------------
# Linear interpolation --------------------------------------------------------

dat_n$data = "Original"

interpolated_df_l <- data.frame(
  wave_midpoint = dates,
  state_code = "National",  # Assuming the state_code stays constant for interpolated data
  data = "Interpolated"
)

# Loop over each behavior column and interpolate
for (col in names(dat_n)[4:18]) {
  interpolated_df_l[[col]] <- interpolate_data(dat_n, dates_num, col,
                                               method = "linear")
}

# Combine original data with interpolated data
int_linear = dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%  # Remove 'Wave' and numeric date if not needed
  bind_rows(interpolated_df_l) %>%
  arrange(wave_midpoint) %>%
  relocate(data) %>%
  distinct() %>%
  ggplot(aes(x = wave_midpoint)) +
  geom_line(aes(y = `Avoiding contact with other people`)) +
  labs(x = "Date", y = "% Adherence (Avoid contact)", title = "Linear interpolation") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Quadratic interpolation (wrong version) -------------------------------------
interpolated_df_q <- data.frame(
  wave_midpoint = dates,
  state_code = "National",
  data = "Interpolated"
)

for (col in names(dat_n)[4:18]) {
  interpolated_df_q[[col]] <- interpolate_data(dat_n, dates_num, col, method = "quadratic")
}

dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%
  bind_rows(interpolated_df_q) %>%
  arrange(wave_midpoint) %>%
  relocate(data) %>%
  distinct() %>%
  ggplot(aes(x = wave_midpoint)) +
  geom_line(aes(y = `Avoiding contact with other people`)) +
  labs(x = "Date", y = "% Adherence (Avoid contact)", title = "(Wrong) quadratic interpolation") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Spline interpolation ---------------------------------------------------------

interpolated_df_c <- data.frame(
  wave_midpoint = dates,
  state_code = "National"
)

for (col in names(dat_n)[4:18]) {
  interpolated_df_c[[col]] <- interpolate_data(dat_n, dates_num, col, method = "spline")
}

int_splines = dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%  # Remove 'Wave' and numeric date if not needed
  bind_rows(interpolated_df_c) %>%
  arrange(wave_midpoint) %>%
  distinct() %>%
  ggplot(aes(x = wave_midpoint)) +
  geom_line(aes(y = `Avoiding contact with other people`)) +
  labs(x = "Date", y = "% Adherence (Avoid contact)", title = "Cubic splines interpolation") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Lagrangian 2nd-order interpolation ------------------------------------------

interpolated_df_lg <- data.frame(
  wave_midpoint = dates,
  state_code = "National"
)

for (col in names(dat_n)[4:18]) {
  interpolated_df_lg[[col]] <- interpolate_data(dat_n, dates_num, col, method = "lagrangian")
}

int_lagrangian = dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%
  bind_rows(interpolated_df_lg) %>%
  arrange(wave_midpoint) %>%
  distinct() %>%
  ggplot(aes(x = wave_midpoint)) +
  geom_line(aes(y = `Avoiding contact with other people`)) +
  labs(x = "Date", y = "% Adherence (Avoid contact)", title = "Lagrangian 2nd-order interpolation") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

int_linear / int_splines / int_lagrangian

dat_n_eom = dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%
  bind_rows(interpolated_df_lg) %>%
  arrange(wave_midpoint) %>%
  distinct() %>%
  filter(wave_midpoint == ceiling_date(wave_midpoint, "month") - days(1)) %>%
  select(wave_midpoint, `Avoiding contact with other people`, `Go visit a friend`) %>%
  rename(date = wave_midpoint,
         avoid_contact_gd = `Avoiding contact with other people`,
         visit_friend_gd = `Go visit a friend`)

dat_gd_examples = dat_n %>%
  select(-Wave, -wave_midpoint_num) %>%
  bind_rows(interpolated_df_lg) %>%
  arrange(wave_midpoint) %>%
  distinct() %>%
  select(wave_midpoint, `Avoiding contact with other people`, `Go visit a friend`) %>%
  rename(date = wave_midpoint,
         avoid_contact_gd = `Avoiding contact with other people`,
         visit_friend_gd = `Go visit a friend`)

## Import the current dataset and compare
current = read_csv("data/processed/behavior_wave_interpolated_all_state.csv")

current %>% filter(state == "National", End_Date == ceiling_date(End_Date, "month") - days(1)) %>%
  select(End_Date, `Avoiding contact with other people`, `Go visit a friend`) %>%
  rename(date = End_Date,
         avoid_contact_current = `Avoiding contact with other people`,
         visit_friend_current = `Go visit a friend`) %>%
  left_join(dat_n_eom, by = "date") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = avoid_contact_current, color = "Current")) +
  geom_line(aes(y = avoid_contact_gd, color = "Check")) +
  scale_color_manual(values = c("Current" = "dodgerblue3", "Check" = "gold2"))


current %>% filter(state == "National", Wave == "interpolation") %>%
  select(End_Date, `Avoiding contact with other people`, `Go visit a friend`) %>%
  rename(date = End_Date,
         avoid_contact_current = `Avoiding contact with other people`,
         visit_friend_current = `Go visit a friend`) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = avoid_contact_current, color = "Current")) +
  geom_line(data = dat_original_interpolated, aes(x = date, y = avoid_contact_gd, color = "Check")) +
  scale_color_manual(values = c("Current" = "dodgerblue3", "Check" = "gold2"))

current %>%
  filter(state == "National") %>%
  select(End_Date, `Avoiding contact with other people`, `Go visit a friend`) %>%
  rename(date = End_Date,
         avoid_contact_current = `Avoiding contact with other people`,
         visit_friend_current = `Go visit a friend`) %>%
  left_join(dat_gd_examples, by = "date") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = avoid_contact_current, color = "Current"))+
  geom_line(aes(y = avoid_contact_gd, color = "Check")) +
  geom_line(aes(y = lag(avoid_contact_gd, 3)), color = "tan3") + # shift my values 6 weeks..
  scale_color_manual(values = c("Current" = "dodgerblue3", "Check" = "gold2"))


