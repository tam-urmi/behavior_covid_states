## -----------------------------------------------------------------------------
## Script name: cov_behavior_OSI_fig.R
##
## Purpose of script: Generate figures comparing OSI to policy adherence
##
## Author: George Dewey
##
## Date Created: 2024-09-10
##
## Last Updated: 2024-09-12
## -----------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)

# Load and configure data

# OSI dataset
osi = read_csv(file = "data/OxCGRT_compact_subnational_v1.csv", show_col_types = FALSE)

# Get raw values for each state
dat = read_csv(file = "data/health_behaviors_W27 (1).csv", show_col_types = FALSE)

osi_us = osi %>%
  filter(CountryName == "United States") %>%
  select(CountryName, CountryCode, RegionName, RegionCode, Date, StringencyIndex_Average)

osi_national = osi_us %>% filter(is.na(RegionName) == TRUE)

osi_states = osi_us %>% filter(is.na(RegionName) == FALSE)

# Use the adherence measure from the earliest time point
dat = select(dat, -1)

names(dat)

names(dat) = c("state", "work", "gym", "friend", "cafe", "hospital", "church", "transit", "avoid_contact", "avoid_crowd", "wash_hands", "mask_public", "room_share_1", "room_share_5_10", "room_share_11_50", "room_share_over_50", "wave", "start_date", "end_date", "n", "total_n")

dat_wave1 = dat %>% filter(wave == 1)

dat %>% filter(wave == 1) %>%
  select(start_date, end_date) # Wave 1 begins 04/16/2020 and ends 04/30/2020

# For analysis, choose the midpoint: 04/24/2020 and compare OSI at that time

osi_us_w1 = osi_us %>% filter(Date == 20200424) %>%
  mutate(state = ifelse(is.na(RegionCode) == FALSE, str_sub(RegionCode, 4, 5), "National")) %>%
  select(state, StringencyIndex_Average)

dat_osi = dat_wave1 %>% left_join(osi_us_w1, by = "state")

pol_map = c("AL" = "Republican", "AK" = "Republican", "AZ" = "Swing", "AR" = "Republican", "CA" = "Democrat", "CO" = "Democrat", "CT" = "Democrat", "DE" = "Democrat", "DC" = "Democrat", "FL" = "Republican", "GA" = "Swing", "HI" = "Democrat", "IA" = "Republican", "ID" = "Republican", "IN" = "Republican", "IL" = "Democrat", "KS" = "Republican", "KY" = "Republican", "LA" = "Republican", "MA" = "Democrat",  "MD" = "Democrat", "ME" = "Democrat", "MI" = "Swing", "MN" = "Democrat", "MO" = "Republican",  "MS" = "Republican", "MT" = "Republican", "NC" = "Republican", "ND" = "Republican", "NE" = "Republican",  "NH" = "Democrat", "NJ" = "Democrat", "NM" = "Democrat", "NV" = "Swing", "NY" = "Democrat",  "OH" = "Republican", "OK" = "Republican", "OR" = "Democrat", "PA" = "Swing", "RI" = "Democrat", "SC" = "Republican", "SD" = "Republican", "TN" = "Republican", "TX" = "Republican", "UT" = "Republican",  "VA" = "Democrat", "VT" = "Democrat", "WA" = "Democrat", "WI" = "Swing", "WV" = "Republican",  "WY" = "Republican", "DC" = "Democrat")

dat_osi$state_party = pol_map[dat_osi$state]

dat_osi_ = dat_osi %>% filter(is.na(state_party) == FALSE)

# Generate figure comparing OSI and % Response
## Going to work
p1 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = work)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go to work") +
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
p1

## Going to the gym
p2 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = gym)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go to the gym") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")


## Visiting a friend
p3 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = friend)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go visit a friend") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Visiting a cafe/bar/restaurant
p4 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = cafe)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go to a cafe, bar,\nor restaurant") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Visiting a doctor/hospital
p5 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = hospital)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go to a doctor or\nvisit a hospital") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Visiting a church of another place of worship
p6 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = church)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Go to church or another\nplace of worship") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Avoid contact in public
p8 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = avoid_contact)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Avoiding contact with\nother people") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Using public transit
p7 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = transit)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Take mass transit (e.g.\nsubway, bus, or train)") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Avoid crowds
p9 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = avoid_crowd)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Avoiding public\nor crowded places") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Mask wearing
p10 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = mask_public)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Wearing a face mask when\noutside your home") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Washing hands
p11 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = wash_hands)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Frequently washing hands") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Sharing a room with one person
p12 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = room_share_1)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Been in a room with someone outside\nof household in the past 24 hours") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Sharing a room with 5-10 people
p13 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = room_share_5_10)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Been in a room with 5-10 people outside\nof household in the past 24 hours") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

## Sharing a room with 11-50 people
p14 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = room_share_11_50)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Been in a room with 11-50 people\noutside of household in the past 24 hours") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

# Sharing room with over 50 people
p15 = dat_osi_ %>%
  ggplot(aes(x = StringencyIndex_Average, y = room_share_over_50)) +
  geom_point(aes(color = state_party), size = 2) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 2,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="% Response",
       title="Been in a room with over 50 people\noutside of household in the past 24 hours") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="bottom")

# Make versions with no axis labels
pb1 = p1 + theme(axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(),
           plot.title = element_blank(),
           legend.position = "none")
pb2 = p2 + theme(axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(),
           plot.title = element_blank(),
           legend.position = "none")
pb3 = p3 + theme(axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(),
           plot.title = element_blank(),
           legend.position = "none")
pb4 = p4 + theme(axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_blank(),
                 legend.position = "none")
pb5 = p5 + theme(axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_blank(),
                 legend.position = "none")

pb6 = p6 + theme(axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_blank(),
                 legend.position = "none")

pb7 = p7 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")

pb8 = p8 + theme(axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_blank(),
                 legend.position = "none")

pb9 = p9 + theme(axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_blank(),
                 legend.position = "none")

pb10 = p10 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")

pb11 = p11 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")

pb12 = p12 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")

pb13 = p13 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")


pb14 = p14+ theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  plot.title = element_blank(),
                  legend.position = "none")

pb15 = p15 + theme(axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   plot.title = element_blank(),
                   legend.position = "none")
# Big dot theme---------------------------------------------------------------
# geom_point(aes(color = state_party), size = 4) +
#   geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
#   scale_color_manual(NULL, values=c("blue", "red", "black", "grey"),
#                      na.translate = FALSE) +
#   geom_label_repel(aes(label = state),
#                    size = 6,
#                    box.padding = 0.25,
#                    point.padding = 0.2,
#                    segment.color = 'grey40',
#                    max.overlaps = 10) +
# theme(
#   axis.text = element_text(size = 12),
#   axis.title = element_text(size = 16),
#   plot.title = element_text(size = 20),
#   panel.background = element_rect(fill = "white"),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
#   legend.text = element_text(size = 24),
#   legend.position="bottom")

# Small dot theme ---------------------------------------------------------------
#   geom_point(aes(color = state_party), size = 2) +
# geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
#   scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
#                      na.translate = FALSE) +
#   geom_label_repel(aes(label = state),
#                    size = 2,
#                    box.padding = 0.1,
#                    point.padding = 0.1,
#                    segment.color = 'grey40',
#                    max.overlaps = 12) +

# theme(
#   axis.text = element_text(size = 12),
#   axis.title = element_text(size = 14),
#   plot.title = element_text(size = 14),
#   panel.background = element_rect(fill = "white"),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.border = element_rect(color = "grey", fill=NA, linewidth=1),
#   legend.text = element_text(size = 14),
#   legend.position="bottom")


## Save rect figures ----------------------------------------------------------
ggsave(p1, file = "figures/behavior_OSI/subpanels/work.png", width = , height = 5, units = "in")
ggsave(p2, file = "figures/behavior_OSI/subpanels/gym.png", width = 8, height = 5, units = "in")
ggsave(p3, file = "figures/behavior_OSI/subpanels/friend.png", width = 8, height = 5, units = "in")
ggsave(p4, file = "figures/behavior_OSI/subpanels/cafe.png", width = 8, height = 5, units = "in")
ggsave(p5, file = "figures/behavior_OSI/subpanels/hospital.png", width = 8, height = 5, units = "in")
ggsave(p6, file = "figures/behavior_OSI/subpanels/church.png", width = 8, height = 5, units = "in")
ggsave(p7, file = "figures/behavior_OSI/subpanels/transit.png", width = 8, height = 5, units = "in")
ggsave(p8, file = "figures/behavior_OSI/subpanels/contact.png", width = 8, height = 5, units = "in")
ggsave(p9, file = "figures/behavior_OSI/subpanels/crowd.png", width = 8, height = 5, units = "in")
ggsave(p10, file = "figures/behavior_OSI/subpanels/mask.png", width = 8, height = 5, units = "in")
ggsave(p11, file = "figures/behavior_OSI/subpanels/wash_hands.png", width = 8, height = 5, units = "in")
ggsave(p12, file = "figures/behavior_OSI/subpanels/share_1.png", width = 8, height = 5, units = "in")
ggsave(p13, file = "figures/behavior_OSI/subpanels/share_5_10.png", width = 8, height = 5, units = "in")
ggsave(p14, file = "figures/behavior_OSI/subpanels/share_11_50.png", width = 8, height = 5, units = "in")
ggsave(p15, file = "figures/behavior_OSI/subpanels/share_50_plus.png", width = 8, height = 5, units = "in")

## Save square figures -------------------------------------------------------------------------
ggsave(p1, file = "figures/behavior_OSI/subpanels_square/work.png", width = 5, height = 5, units = "in")
ggsave(p2, file = "figures/behavior_OSI/subpanels_square/gym.png", width = 5, height = 5, units = "in")
ggsave(p3, file = "figures/behavior_OSI/subpanels_square/friend.png", width = 5, height = 5, units = "in")
ggsave(p4, file = "figures/behavior_OSI/subpanels_square/cafe.png", width = 5, height = 5, units = "in")
ggsave(p5, file = "figures/behavior_OSI/subpanels_square/hospital.png", width = 5, height = 5, units = "in")
ggsave(p6, file = "figures/behavior_OSI/subpanels_square/church.png", width = 5, height = 5, units = "in")
ggsave(p7, file = "figures/behavior_OSI/subpanels_square/transit.png", width = 5, height = 5, units = "in")
ggsave(p8, file = "figures/behavior_OSI/subpanels_square/contact.png", width = 5, height = 5, units = "in")
ggsave(p9, file = "figures/behavior_OSI/subpanels_square/crowd.png", width = 5, height = 5, units = "in")
ggsave(p10, file = "figures/behavior_OSI/subpanels_square/mask.png", width = 5, height = 5, units = "in")
ggsave(p11, file = "figures/behavior_OSI/subpanels_square/wash_hands.png", width = 5, height = 5, units = "in")
ggsave(p12, file = "figures/behavior_OSI/subpanels_square/share_1.png", width = 5, height = 5, units = "in")
ggsave(p13, file = "figures/behavior_OSI/subpanels_square/share_5_10.png", width = 5, height = 5, units = "in")
ggsave(p14, file = "figures/behavior_OSI/subpanels_square/share_11_50.png", width = 5, height = 5, units = "in")
ggsave(p15, file = "figures/behavior_OSI/subpanels_square/share_50_plus.png", width = 5, height = 5, units = "in")

## Save square blanks ---------------------------------------------------------------------
ggsave(pb1, file = "figures/behavior_OSI/subpanels_squareblank/work.png", width = 5, height = 5, units = "in")
ggsave(pb2, file = "figures/behavior_OSI/subpanels_squareblank/gym.png", width = 5, height = 5, units = "in")
ggsave(pb3, file = "figures/behavior_OSI/subpanels_squareblank/friend.png", width = 5, height = 5, units = "in")
ggsave(pb4, file = "figures/behavior_OSI/subpanels_squareblank/cafe.png", width = 5, height = 5, units = "in")
ggsave(pb5, file = "figures/behavior_OSI/subpanels_squareblank/hospital.png", width = 5, height = 5, units = "in")
ggsave(pb6, file = "figures/behavior_OSI/subpanels_squareblank/church.png", width = 5, height = 5, units = "in")
ggsave(pb7, file = "figures/behavior_OSI/subpanels_squareblank/transit.png", width = 5, height = 5, units = "in")
ggsave(pb8, file = "figures/behavior_OSI/subpanels_squareblank/contact.png", width = 5, height = 5, units = "in")
ggsave(pb9, file = "figures/behavior_OSI/subpanels_squareblank/crowd.png", width = 5, height = 5, units = "in")
ggsave(pb10, file = "figures/behavior_OSI/subpanels_squareblank/mask.png", width = 5, height = 5, units = "in")
ggsave(pb11, file = "figures/behavior_OSI/subpanels_squareblank/wash_hands.png", width = 5, height = 5, units = "in")
ggsave(pb12, file = "figures/behavior_OSI/subpanels_squareblank/share_1.png", width = 5, height = 5, units = "in")
ggsave(pb13, file = "figures/behavior_OSI/subpanels_squareblank/share_5_10.png", width = 5, height = 5, units = "in")
ggsave(pb14, file = "figures/behavior_OSI/subpanels_squareblank/share_11_50.png", width = 5, height = 5, units = "in")
ggsave(pb15, file = "figures/behavior_OSI/subpanels_squareblank/share_50_plus.png", width = 5, height = 5, units = "in")

# ggsave(p1, file = "figures/behavior_OSI/work_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p2, file = "figures/behavior_OSI/gym_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p3, file = "figures/behavior_OSI/friend_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p4, file = "figures/behavior_OSI/cafe_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p5, file = "figures/behavior_OSI/hospital_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p6, file = "figures/behavior_OSI/church_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p7, file = "figures/behavior_OSI/transit_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p8, file = "figures/behavior_OSI/contact_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p9, file = "figures/behavior_OSI/crowd_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p10, file = "figures/behavior_OSI/mask_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p11, file = "figures/behavior_OSI/wash_hands_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p12, file = "figures/behavior_OSI/share_1_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p13, file = "figures/behavior_OSI/share_5_10_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p14, file = "figures/behavior_OSI/share_11_50_small.png", width = 4, height = 2.5, units = "in")
# ggsave(p15, file = "figures/behavior_OSI/share_50_plus_small.png", width = 4, height = 2.5, units = "in")

# png(filename="figures/behavior_OSI/behavior_OSI_agg.png", width=20, height=25, res = 150, units = "in")
# wrap_plots(p8, p9, p10, p11, p1, p2, p3, p4, p5, p6, p7, p12, p13, p14, p15, ncol = 3, nrow = 5) +
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")
# dev.off()

## Special versions
## Avoid contact in public
dat_osi_ %>%
  filter(state != "ND") %>%
  ggplot(aes(x = StringencyIndex_Average, y = avoid_contact)) +
  geom_point(aes(color = state_party), size = 4) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 4,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="Response [%]",
  ) +
  theme(
    text = element_text(family = "DejaVu Sans"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="none")
ggsave(filename = "figures/avoid_contact_custom.png", width = 7, height = 5, units = "in")

dat_osi_ %>%
  filter(state != "ND") %>%
  ggplot(aes(x = StringencyIndex_Average, y = friend)) +
  geom_point(aes(color = state_party), size = 4) +
  geom_line(stat = 'smooth', method = lm, color = "gold2", alpha = 0.5) +
  scale_color_manual(NULL, values=c("blue", "red", "black", "black"),
                     na.translate = FALSE) +
  geom_label_repel(aes(label = state),
                   size = 4,
                   box.padding = 0.1,
                   point.padding = 0.1,
                   segment.color = 'grey40',
                   max.overlaps = 12) +
  labs(x="OSI", y="Response [%]",
       ) +
  theme(
    text = element_text(family = "DejaVu Sans"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1),
    legend.text = element_text(size = 14),
    legend.position="none")
ggsave(filename = "figures/friend_custom.png", width = 7, height = 5, units = "in")
