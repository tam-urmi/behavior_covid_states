## -----------------------------------------------------------------------------
## Script name: table_2_behavior_stats.R
##
## Purpose of script: Calcualate percent change for behaviors at the state level
## and calculate statistics
##
## Author: George Dewey
##
## Date Created: 2024-10-16
##
## Last Updated: 2024-10-17
## -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)

dat = read_csv("data/processed/behavior_wave_interpolated_all_state.csv")

names(dat) = c("wave", "end_date", "state", "avoid_contact", "avoid_crowds",
               "freq_handwash", "wear_mask", "work", "gym", "visit_friend", "cafe",
               "doctor", "church", "transit", "room_1", "room_5_10", "room_11_50",
               "room_50_plus", "n", "new_ind")

dat = dat %>% select(-c("n", "new_ind"))

# Make some convenience functions
sd1 = function(x){sd(x, na.rm = TRUE)}
se_mean = function(x){sd(x, na.rm = TRUE)/sqrt((length(x)))}
max1 = function(x){max(x, na.rm = TRUE)}
mean1 = function(x){mean(x, na.rm = TRUE)}

dat$wave


dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI")) %>%
  group_by(state, tp) %>%
  summarize(avoid_contact = mean1(avoid_contact),
            visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(avoid_contact, visit_friend)) %>%
  ungroup() %>%
  summarize(mean_ac_start = mean(avoid_contact_start),
            mean_ac_final = mean(avoid_contact_final),
            sd_ac_start = sd(avoid_contact_start),
            sd_ac_final = sd(avoid_contact_final),
            )

dat_by_state = dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI")) %>%
  group_by(state, tp) %>%
  summarize(mean_avoid_contact = mean1(avoid_contact),
            mean_visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(mean_avoid_contact, mean_visit_friend)) %>%
  ungroup()

dat_by_state_50 = dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  # filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI", "NE")) %>%
  group_by(state, tp) %>%
  summarize(mean_avoid_contact = mean1(avoid_contact),
            mean_visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(mean_avoid_contact, mean_visit_friend)) %>%
  ungroup()


quantile(dat_by_state_50$mean_avoid_contact_start, c(0.025, 0.5, 0.975))
quantile(dat_by_state_50$mean_avoid_contact_final, c(0.025, 0.5, 0.975))
quantile(dat_by_state_50$mean_visit_friend_start, c(0.025, 0.5, 0.975))
quantile(dat_by_state_50$mean_visit_friend_final, c(0.025, 0.5, 0.975))

# Statewise stats- ------------------------------------------------------------
quantile(dat_by_state$mean_avoid_contact_start, c(0.025, 0.5, 0.975))
quantile(dat_by_state$mean_avoid_contact_final, c(0.025, 0.5, 0.975))
quantile(dat_by_state$mean_visit_friend_start, c(0.025, 0.5, 0.975))
quantile(dat_by_state$mean_visit_friend_final, c(0.025, 0.5, 0.975))

dat_by_state %>% view()


dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI")) %>%
  group_by(state, tp) %>%
  summarize(mean_avoid_contact = mean1(avoid_contact),
            mean_visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(mean_avoid_contact, mean_visit_friend)) %>%
  ungroup() %>%
  mutate(avoid_contact_perc_dec = abs((mean_avoid_contact_start - mean_avoid_contact_final)/(mean_avoid_contact_start)),
         visit_friend_perc_chg = abs((mean_visit_friend_start - mean_visit_friend_final)/(mean_visit_friend_start))) %>%
  summarize(mean_ac_perc_dec = mean(avoid_contact_perc_dec),
            min_ac_perc_dec = min(avoid_contact_perc_dec),
            max_ac_perc_dec = max(avoid_contact_perc_dec),
            se_ac_perc_dec = se_mean(avoid_contact_perc_dec),
            mean_vf_perc_dec = mean(visit_friend_perc_chg),
            min_vf_perc_dec = min(visit_friend_perc_chg),
            max_vf_perc_dec = max(visit_friend_perc_chg),
            se_vf_perc_dec = se_mean(visit_friend_perc_chg))

# Histogram for avoid contact -------------------------------------------------

h1 = dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI")) %>%
  group_by(state, tp) %>%
  summarize(mean_avoid_contact = mean1(avoid_contact),
            mean_visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(mean_avoid_contact, mean_visit_friend)) %>%
  ungroup() %>%
  ggplot()+
  geom_histogram(aes(x = mean_avoid_contact_start, fill = "April 2020 Values"), bins = 15) +
  geom_histogram(aes(x = mean_avoid_contact_final, fill = "May 2022 Values"), bins = 15) +
  labs(x = "% Adherence", y = "Count", title = "Start and end distributions for\navoid contact with other people") +
  scale_fill_manual(NULL, values = c("April 2020 Values" = "#ff6f00", "May 2022 Values" = "#c45f00")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 12),
    legend.position="bottom")
ggsave(file = "figures/avoid_contact_dists.png", width = 6, height = 6, units = "in", bg = "white")


# Histogram for visit a friend -------------------------------------------------

h2 = dat %>%
  filter(end_date %in% c(as_date("2020-04-30"), as_date("2022-05-31"))) %>%
  mutate(tp = ifelse(wave == 1, "start", "final")) %>%
  filter(!state %in% c("AK", "WY", "ND", "SD", "NM", "VT", "DC", "RI", "MT", "HI")) %>%
  group_by(state, tp) %>%
  summarize(mean_avoid_contact = mean1(avoid_contact),
            mean_visit_friend = mean1(visit_friend)) %>%
  pivot_wider(names_from = tp,
              values_from = c(mean_avoid_contact, mean_visit_friend)) %>%
  ungroup() %>%
  ggplot()+
  geom_histogram(aes(x = mean_visit_friend_start, fill = "April 2020 Values"), bins = 15) +
  geom_histogram(aes(x = mean_visit_friend_final, fill = "May 2022 Values"), bins = 15) +
  labs(x = "% Adherence", y = "Count", title = "Start and end distributions for\ngoing to visit a friend") +
  scale_fill_manual(NULL, values = c("April 2020 Values" = "#0eff00", "May 2022 Values" = "#089000")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 12),
    legend.position="bottom")
ggsave(file = "figures/visit_friend_dists.png", width = 6, height = 6, units = "in", bg = "white")

h1 + h2 + plot_annotation(tag_levels = "A")
ggsave(file = "figures/state_level_distributions.png", width = 8, height = 6, unit = "in", bg = "white")
