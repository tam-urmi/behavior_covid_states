## -----------------------------------------------------------------------------
## Script name: check_aggregation.R
##
## Purpose of script: To check existing aggregation measures for disease severity
## measures
##
## Author: George Dewey
##
## Date Created: 2024-10-24
##
## Last Updated: 2024-10-24
## -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)

## load data
deaths = read_csv("data/death_daily_us_state.csv")
deaths
deaths %>%
  select(Province_State) %>%
  distinct() %>%
  print(n = 51)

cases = read_csv("data/time_series_covid19_confirmed_US.csv")

## Import the current set for validation
current_deaths = read_csv("data/processed/death_monthly_state.csv")

monthly_current_deaths = current_deaths %>% filter(State == "National") %>% select(End_Date, death_agg30)

current_cases = read_csv("data/processed/")

## Comparing aggregate deaths
deaths %>%
  mutate(Month = floor_date(Last_Update, "month")) %>%
  group_by(Month) %>%
  summarise(total_deaths = sum(death_daily, na.rm = TRUE)) %>%
  arrange(Month) %>%
  mutate(Month = ceiling_date(as_date(Month), "month")) %>%
  ggplot(aes(x = Month))+
  geom_point(aes(y = total_deaths, color = "Validation")) +
  geom_point(data = monthly_current_deaths, aes(x = End_Date, y = death_agg30, color = "Current")) +
  scale_color_manual(values = c("Validation" = "darkgreen", "Current" = "gold2"))
# Looks good

cases = cases %>% select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Lat, Long_, Combined_Key))

cases_long <- cases %>%
  pivot_longer(cols = -c("Province_State", "Country_Region"),  # All columns with dates
               names_to = "date",         # Name the new 'date' column
               values_to = "case_count")  # Values go into 'case_count' column

# Convert 'date' column to a proper date format
cases_long <- cases_long %>%
  mutate(date = mdy(date))  # Convert to date using lubridate's mdy()

# Aggregate by month and sum cases for the entire country
cases_monthly <- cases_long %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(monthly_cases = sum(case_count, na.rm = TRUE)) %>%
  arrange(month)

cases_monthly %>%
  mutate(month = ceiling_date(month, "month") - 1)

hosps = read_csv("data/owid_US_COVID-19_Hospitalization_Metrics_by_Jurisdiction__Timeseries_20240509.csv")

hosps %>%
  select(`Collection Date`, Jurisdiction, `New COVID-19 Hospital Admissions`) %>%
  filter(Jurisdiction == "USA") %>%
  mutate(month = ceiling_date(`Collection Date`, "month") -1) %>%  # Extract the month from 'Collection Date'
  group_by(month) %>%
  summarise(monthly_hospital_admissions = sum(`New COVID-19 Hospital Admissions`, na.rm = TRUE)) %>%
  arrange(month)
