## -----------------------------------------------------------------------------
## Script name: sensitivity_repeat_participants.R
##
## Purpose of script: To compare trends with and without repeat survey participants
##
## Author: George Dewey
##
## Date Created: 2024-10-01
##
## Last Updated: 2024-10-01
## -----------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(patchwork)

# Load data
no_return_dat = read_csv("../covid_states_behavior1/behaviors/behaviors_SI_no_return.csv")
standard_dat = read_csv("data/health_behaviors_W27 (1).csv")

no_return_dat = no_return_dat %>% select(-c(Start_Date, End_Date))
no_return_dat$WaveC = as.character(no_return_dat$Wave)
no_return_dat = no_return_dat %>% mutate(across(where(is.numeric), ~ ./100))

standard_dat = standard_dat %>% select(-c(Start_Date, End_Date))
standard_dat$WaveC = as.character(standard_dat$Wave)
standard_dat = standard_dat %>% mutate(across(where(is.numeric), ~ ./100))


# Plot avoid contact against each other ----------------------------------------
no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Avoiding contact with other people`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat %>% filter(state_code == "National"),
            aes(x = as.numeric(WaveC), y = `Avoiding contact with other people`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Avoiding contact with other people") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 15),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

# ggsave("figures/for_manuscript/SA_repeats/avoid_contact.png",
#        width = 6, height = 4, units = "in", bg = "white")

# Generate plots for each behavior

standard_dat_natl = standard_dat %>% filter(state_code == "National") %>%
  select(WaveC, `Go to work`:`Been in a room with over 50 people outside of household in the past 24 hours`)

for(col in 2:(ncol(no_return_dat)-1)){

 p = no_return_dat %>%
    ggplot(aes(x = as.numeric(WaveC), y = .[[col]], color = "Without repeats")) +
    geom_line() +
    geom_line(data = standard_dat_natl,
              aes(x = as.numeric(WaveC), y = standard_dat_natl[[col]], color = "With repeats")) +
    scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1, 27, 1)) +
    labs(x = "Survey wave",
         y = "Adherence",
         title = names(no_return_dat)[col]) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.title = element_text(size = 10),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key.height = unit(1, "lines"),
      legend.position="bottom")

  # ggsave(filename = paste0("../covid_states_behavior1/figures/supplementary/SA_repeats/",names(no_return_dat)[col],".png"), width = 6, height = 4, units = "in", bg = "white")

}

# Plotting individually for combined plot ---------------------------------------------

p1 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Avoiding contact with other people`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Avoiding contact with other people`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Avoiding contact with other people") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p2 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Avoiding public or crowded places`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Avoiding public or crowded places`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Avoiding public or crowded places") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p3 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Wearing a face mask when outside of your home`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Wearing a face mask when outside of your home`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Wearing a face mask when outside of your home") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p4 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Frequently washing hands`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Frequently washing hands`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Frequently washing hands") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p5 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go to work`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go to work`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go to work") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p6 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go to the gym`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go to the gym`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go to the gym") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p7 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go visit a friend`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go visit a friend`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go visit a friend") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p8 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go to a cafe, bar, or restaurant`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go to a cafe, bar, or restaurant`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go to a cafe, bar, or\nrestaurant") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p9 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go to a doctor or visit a hospital`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go to a doctor or visit a hospital`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go to a doctor or\nvisit a hospital") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p10 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Go to church or another place of worship`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Go to church or another place of worship`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Go to church or another\nplace of worship") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p11 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Take mass transit (e.g. subway, bus, or train)`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Take mass transit (e.g. subway, bus, or train)`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Take mass transit (e.g.\nsubway, bus, or train") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p12 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Been in a room with someone outside of household in the past 24 hours`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Been in a room with someone outside of household in the past 24 hours`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with someone outside\nof household in the past 24 hours") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p13 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Been in a room with 5-10 people outside of household in the past 24 hours`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Been in a room with 5-10 people outside of household in the past 24 hours`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with 5-10 people outside\nof household in the past 24 hours") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p14 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Been in a room with 11-50 people outside of household in the past 24 hours`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Been in a room with 11-50 people outside of household in the past 24 hours`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with 11-50 people outside\nof household in the past 24 hours") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

p15 = no_return_dat %>%
  ggplot(aes(x = as.numeric(WaveC), y = `Been in a room with over 50 people outside of household in the past 24 hours`, color = "Without repeats")) +
  geom_line() +
  geom_line(data = standard_dat_natl,
            aes(x = as.numeric(WaveC), y = `Been in a room with over 50 people outside of household in the past 24 hours`, color = "With repeats")) +
  scale_color_manual(values = c("Without repeats" = "dodgerblue3", "With repeats" = "gold2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with over 50 people outside\nof household in the past 24 hours") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill=NA, linewidth=1.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "lines"),
    legend.position="bottom")

png(filename="figures/for_manuscript/SA_repeats/sa_repeats_combined.png", width=20, height=30, res = 150, units = "in")
wrap_plots(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol = 3, nrow = 5) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom",
                                          legend.text = element_text(size = 24),
                                          axis.title.x = element_blank(),
                                          axis.text.y = element_text(size = 15),
                                          plot.title = element_text(size = 18),

                                          )
dev.off()


