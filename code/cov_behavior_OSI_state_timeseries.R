## -----------------------------------------------------------------------------
## Script name: cov_behavior_OSI_state_timeseries.R
##
## Purpose of script: Generate visualizations of OSI and behavior adherence
## over time for selected states
##
## Author: George Dewey
##
## Date Created: 2024-09-23
##
## Last Updated: 2024-10-09
## -----------------------------------------------------------------------------
# Load packages
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)

# Load and configure data

# OSI dataset
osi = read_csv(file = "../covid_states_behavior/data/OxCGRT_compact_subnational_v1.csv", show_col_types = FALSE)

# Behavior data
dat = read_csv(file = "../covid_states_behavior1/data/processed/behavior_wave_interpolated_all_state.csv", show_col_types = FALSE)

# Data management
osi_us = osi %>%
  filter(CountryName == "United States") %>%
  select(CountryName, CountryCode, RegionName, RegionCode, Date, StringencyIndex_Average)

osi_national = osi_us %>% filter(is.na(RegionName) == TRUE)

osi_states = osi_us %>% filter(is.na(RegionName) == FALSE)

dat = select(dat, -1)

names(dat) = c("end_date", "state", "avoid_contact", "avoid_crowds", "wash_hands",
               "wear_mask", "work", "gym", "friend", "cafe", "doctor", "church",
               "transit", "room", "room_5_10", "room_11_50", "room_50_plus",
               "N", "new_ind")

osi_states_date_updated = osi_states %>%
  filter(RegionName %in% c("Massachusetts", "Virginia", "Louisiana", "Nebraska",
                           "Alaska")) %>%
  mutate(state = ifelse(is.na(RegionCode) == FALSE,
                        str_sub(RegionCode, 4, 5), "National"),
         date_true = ymd(Date)) %>%
  select(state, date_true, StringencyIndex_Average) %>%
  filter(date_true >= as_date("2020-03-31"))

dat_MA = dat %>% filter(state == "MA", end_date <= as_date("2022-04-01"))
dat_VA = dat %>% filter(state == "VA", end_date <= as_date("2022-04-01"))
dat_LA = dat %>% filter(state == "LA", end_date <= as_date("2022-04-01"))
dat_NE = dat %>% filter(state == "NE", end_date <= as_date("2022-04-01"))
dat_AK = dat %>% filter(state == "AK", end_date <= as_date("2022-04-01"))

# Plots for state behavior vs. OSI for the example set -------------------------

dat_MA %>%
  ggplot(aes(x = as_date(end_date))) +
  geom_line(aes(y = avoid_contact, color = "Behavior"), linewidth = 0.75) +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "MA"),
            aes(x = date_true,
                y = StringencyIndex_Average,
                color = "OSI"),
            linewidth = 0.75,
            show.legend = TRUE) +
  scale_color_manual(NULL, values = c("OSI" = "blue", "Behavior" = "orange")) +
  labs(title = "Massachusetts / Avoid Contact", x = "", y = "% Adherence / OSI") +
  scale_y_continuous(limits = c(20, 80), breaks = c(20, 40, 60, 80)) +
  scale_x_date(limits = c(as_date("2020-04-01"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = -0.25),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.text = element_text(size = 12),
    legend.position="bottom")


p2 =  dat_VA %>%
  ggplot(aes(x = as_date(end_date))) +
  geom_line(aes(y = avoid_contact, color = "Behavior"), linewidth = 0.75) +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "VA"),
            aes(x = date_true,
                y = StringencyIndex_Average,
                color = "OSI"),
            linewidth = 0.75,
            show.legend = TRUE) +
  scale_color_manual(values = c("OSI" = "blue", "Behavior" = "orange")) +
  labs(title = "Virginia / Avoid Contact", x = "", y = "% Adherence / OSI") +
  scale_y_continuous(limits = c(18, 80), breaks = c(20, 40, 60, 80)) +
  scale_x_date(limits = c(as_date("2020-04-01"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = -0.25),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.position="none")

p3 =  dat_LA %>%
  ggplot(aes(x = as_date(end_date), color = state, group = state)) +
  geom_line(aes(y = avoid_contact, color = "Behavior", linewidth = 0.75) +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "LA"),
            aes(x = date_true,
                y = StringencyIndex_Average,
                group = state),
            linewidth = 0.75,
            show.legend = TRUE) +
  scale_color_manual(values = c("LA" = "red")) +
  labs(title = "Louisiana / Avoid Contact", x = "", y = "% Adherence / OSI") +
  scale_y_continuous(limits = c(20, 80), breaks = c(20, 40, 60, 80)) +
  scale_x_date(limits = c(as_date("2020-04-01"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = -0.25),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.position="none")

p4 =  dat_NE %>%
  ggplot(aes(x = as_date(end_date), color = state, group = state)) +
  geom_line(aes(y = avoid_contact), color = "orange", linewidth = 0.75) +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "NE"),
            aes(x = date_true,
                y = StringencyIndex_Average,
                group = state),
            linewidth = 0.75,
            show.legend = TRUE) +
  scale_color_manual(values = c("NE" = "red")) +
  labs(title = "Nebraska / Avoid Contact", x = "", y = "% Adherence / OSI") +
  scale_y_continuous(limits = c(15, 80), breaks = c(20, 40, 60, 80)) +
  scale_x_date(limits = c(as_date("2020-04-01"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = -0.25),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.position="none")

p5 =  dat_AK %>%
  ggplot(aes(x = as_date(end_date), color = state, group = state)) +
  geom_line(aes(y = avoid_contact), color = "orange", linewidth = 0.75) +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "AK"),
            aes(x = date_true,
                y = StringencyIndex_Average,
                group = state),
            linewidth = 0.75,
            show.legend = TRUE) +
  scale_color_manual(values = c("AK" = "red")) +
  labs(title = "Alaska / Avoid Contact", x = "", y = "% Adherence / OSI") +
  scale_y_continuous(limits = c(18, 80), breaks = c(20, 40, 60, 80)) +
  scale_x_date(limits = c(as_date("2020-04-01"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = -0.25),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.position="none")

(p1 + p2) / (p3 + p4) / (p5 + plot_spacer())
ggsave(filename = "../covid_states_behavior1/figures/supplementary/OSI_avoid_contact_timeseries_v4.png", width = 8, height = 7, units = "in", bg = "white")

## Plotting OSI trajectories over time for each state
osi_states_date_updated = osi_states_date_updated %>%
  mutate(party = case_when(state %in% c("AK", "LA", "NE") ~ "Republican",
                           state %in% c("MA", "VA") ~ "Democratic"))

dat_s = dat %>% filter(state %in% c("MA", "VA", "NE", "AK", "LA")) %>%
  mutate(midpoint = as_date(start_date) + (as_date(end_date) - as_date(start_date))/2) %>%
  select(state, midpoint, avoid_contact)

osi_dat_s = dat_s %>% left_join(osi_states_date_updated, by = c("state", c("midpoint" = "date_true")))

osi_dat_s = osi_dat_s %>%
  mutate(squared_error = (StringencyIndex_Average - avoid_contact)^2)

# Calculate MAE for each state
osi_dat_LA = osi_dat_s %>% filter(state == "LA") %>% drop_na()
osi_dat_AK = osi_dat_s %>% filter(state == "AK") %>% drop_na()
osi_dat_NE = osi_dat_s %>% filter(state == "NE") %>% drop_na()
osi_dat_VA = osi_dat_s %>% filter(state == "VA") %>% drop_na()
osi_dat_MA = osi_dat_s %>% filter(state == "MA") %>% drop_na()

Metrics::mae(osi_dat_LA$StringencyIndex_Average, osi_dat_LA$avoid_contact)
Metrics::mae(osi_dat_AK$StringencyIndex_Average, osi_dat_AK$avoid_contact)
Metrics::mae(osi_dat_NE$StringencyIndex_Average, osi_dat_NE$avoid_contact)
Metrics::mae(osi_dat_VA$StringencyIndex_Average, osi_dat_VA$avoid_contact)
Metrics::mae(osi_dat_MA$StringencyIndex_Average, osi_dat_MA$avoid_contact)


osi_dat_s %>%
  ggplot(aes(x = midpoint, y = squared_error, group = state, color = state)) +
  geom_line(linewidth = 1.) +
  scale_color_manual(NULL, values = c("#8eace6", "#219ebc", "#023047", "#F6D55C", "#ED553B")) +
  scale_x_date(limits = c(as_date("2020-04-23"), as_date("2022-10-23")))+
  labs(title = "Deviation between adherence and OSI\n(Avoiding contact with others)", x = "", y = "Deviation (squared error)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="bottom")

sse = NULL
for(i in nrow(dat)){
  sse = sse + squared_error
}

dat %>%
  filter(state %in% c("LA")) %>%
  ggplot(aes(x = as_date(start_date), color = state, group = state)) +
  geom_line(aes(y = avoid_contact), color = "blue") +
  geom_line(data = osi_states_date_updated %>%
              filter(state == "LA"), aes(x = date_true,
                                         y = StringencyIndex_Average,
                                         group = state)) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## OSI vs. behavior over time for each state

# Build a new dataset: date, avoid_contact, osi, party

dat_nr = dat %>% select(end_date, state, avoid_contact) %>% rename(date = end_date)

osi_states_nr = osi_states %>%
  mutate(state = ifelse(is.na(RegionCode) == FALSE, str_sub(RegionCode, 4, 5), "National"),
         date = ymd(Date)) %>%
  select(state, date, StringencyIndex_Average) %>%
  filter(date >= as_date("2020-03-31"))

names(dat_nr)
names(osi_states_nr)

pol_map = c("AL" = "Republican", "AK" = "Republican", "AZ" = "Swing", "AR" = "Republican", "CA" = "Democrat", "CO" = "Democrat", "CT" = "Democrat", "DE" = "Democrat", "DC" = "Democrat", "FL" = "Republican", "GA" = "Swing", "HI" = "Democrat", "IA" = "Republican", "ID" = "Republican", "IN" = "Republican", "IL" = "Democrat", "KS" = "Republican", "KY" = "Republican", "LA" = "Republican", "MA" = "Democrat",  "MD" = "Democrat", "ME" = "Democrat", "MI" = "Swing", "MN" = "Democrat", "MO" = "Republican",  "MS" = "Republican", "MT" = "Republican", "NC" = "Swing", "ND" = "Republican", "NE" = "Republican",  "NH" = "Democrat", "NJ" = "Democrat", "NM" = "Democrat", "NV" = "Swing", "NY" = "Democrat",  "OH" = "Republican", "OK" = "Republican", "OR" = "Democrat", "PA" = "Swing", "RI" = "Democrat", "SC" = "Republican", "SD" = "Republican", "TN" = "Republican", "TX" = "Republican", "UT" = "Republican",  "VA" = "Democrat", "VT" = "Democrat", "WA" = "Democrat", "WI" = "Democrat", "WV" = "Republican",  "WY" = "Republican", "DC" = "Democrat")

dat_osi_nr = dat_nr %>% left_join(osi_states_nr, by = c("state", "date")) %>%
  mutate(party = pol_map[state])

behav_osi_means = dat_osi_nr %>% group_by(party, date) %>%
  summarize(mean_ac = mean(avoid_contact, na.rm = T),
            mean_OSI = mean(StringencyIndex_Average, na.rm = T))

## OSI vs. behavior by party ---------------------------------------------------

## Subplot 1 - OSI over time

comp_f1 = dat_osi_nr %>%
  filter(is.na(party) == FALSE) %>%
  ggplot(aes(x = date, group = state, color = party)) +
  geom_line(aes(y = StringencyIndex_Average), linetype = "dashed") +
  scale_color_manual(NULL, values = c("Democrat" = "blue", "Swing" = "black", "Republican" = "red")) +
  scale_y_continuous(limits = c(15, 80), breaks = c(20, 40, 60, 80)) +
  labs(x = "", y = "OSI") +
  scale_x_date(limits = c(as_date("2020-04-30"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="none")

# Subplot 2 - Behavior over time
comp_f2 = dat_osi_nr %>%
  filter(is.na(party) == FALSE) %>%
  ggplot(aes(x = date, group = state, color = party)) +
  geom_line(aes(y = avoid_contact)) +
  scale_color_manual(NULL, values = c("Democrat" = "blue", "Swing" = "black", "Republican" = "red")) +
  scale_y_continuous(limits = c(10, 80), breaks = c(20, 40, 60, 80)) +
  labs(x = "", y = "% Adherence") +
  scale_x_date(limits = c(as_date("2020-04-30"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="none")

comp_f3 = dat_osi_nr %>%
  filter(is.na(party) == FALSE) %>%
  ggplot(aes(x = date, group = state, color = party)) +
  geom_line(data = behav_osi_means %>% drop_na(), aes(x = date, y = mean_ac, group = party, color = party), linewidth = 1.25) +
  geom_line(data = behav_osi_means %>% drop_na(), aes(x = date, y = mean_OSI, group = party, color = party), linewidth = 1.25, linetype = "dashed") +
  scale_color_manual(NULL, values = c("Democrat" = "blue", "Swing" = "black", "Republican" = "red")) +
  scale_linetype_manual(NULL, values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(10, 80), breaks = c(20, 40, 60, 80)) +
  labs(x = "", y = "% Adherence / OSI") +
  scale_x_date(limits = c(as_date("2020-04-30"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="none")


## OSI vs. behavior by party (aggregate) ---------------------------------------

dat_osi_nr %>%
  filter(is.na(party) == FALSE) %>%
  ggplot(aes(x = date, group = state, color = party)) +
  geom_line(aes(y = avoid_contact, linetype = "Behavior"), alpha = 0.1) +
  geom_line(aes(y = StringencyIndex_Average, linetype = "OSI"), alpha = 0.1) +
  geom_line(data = behav_osi_means %>% drop_na(), aes(x = date, y = mean_ac, group = party, color = party), linewidth = 1.25) +
  geom_line(data = behav_osi_means %>% drop_na(), aes(x = date, y = mean_OSI, group = party, color = party), linewidth = 1.25, linetype = "twodash") +
  scale_color_manual(NULL, values = c("Democrat" = "blue", "Swing" = "black", "Republican" = "red")) +
  scale_linetype_manual(values = c("Behavior" = "solid", "OSI" = "dotted")) +
  scale_y_continuous(limits = c(10, 80), breaks = c(20, 40, 60, 80)) +
  labs(x = "", y = "% Adherence / OSI", title = "Avoid Contact / OSI by Political Leaning") +
  scale_x_date(limits = c(as_date("2020-04-30"), as_date("2022-03-31")),
               date_breaks = "5 months",
               date_labels = " %b \n%Y",
               expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.text = element_text(size = 14),
    legend.position="bottom")
# ggsave(filename = "figures/supplementary/avoid_contact_party_OSI_means.png", height = 5, width = 7, units = "in", bg = "white")
