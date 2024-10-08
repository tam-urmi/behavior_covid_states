## -----------------------------------------------------------------------------
## Script name: sensitivity_aggregation.R
##
## Purpose of script: Sensitivity analysis for data aggregation methods for
## COVID behavior project
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
standard_dat = read_csv("data/health_behaviors_W27 (1).csv")
agg_alt_dat = read_csv("../covid_states_behavior1/behaviors/behaviors_SI_different_agg.csv")

# Make plots
agg_alt_dat = agg_alt_dat %>% select(-c(Start_Date, End_Date))
names(agg_alt_dat) = c("Wave", "Avoiding contact with other people", "Avoiding public or crowded places",
                       "Frequently washing hands", "Wearing a face mask when outside of your home",
                       "Been in a room with at least 5 people outside of household in the past 24 hours",
                       "Been in a room with at least 11 people outside of household in the past 24 hours")

standard_dat_selected = standard_dat %>%
  filter(state_code == "National") %>%
  select(Wave, `Avoiding contact with other people`,
         `Avoiding public or crowded places`,
         `Frequently washing hands`,
         `Wearing a face mask when outside of your home`,
         `Been in a room with 5-10 people outside of household in the past 24 hours`,
         `Been in a room with 11-50 people outside of household in the past 24 hours`)

agg_alt_dat = agg_alt_dat %>%
  mutate(across(-1, ~ .x/100))

standard_dat_selected = standard_dat_selected %>%
  mutate(across(-1, ~ .x/100))

# Four adherence behaviors ----------------------------------------------------
for(col in 2:5){

  agg_alt_dat %>%
    ggplot(aes(x = Wave, y = .[[col]])) +
    geom_line(data = agg_alt_dat, aes(color = "Somewhat + very likely")) +
    geom_line(data = standard_dat_selected, aes(x = Wave, y = standard_dat_selected[[col]], color = "Very likely")) +
    scale_color_manual(values = c("Somewhat + very likely" = "darkslateblue", "Very likely" = "orange")) +
    labs(x = "Survey wave",
         y = "Adherence",
         title = names(agg_alt_dat)[col]) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1, 27, 1)) +
    annotate("text", x = median(agg_alt_dat$Wave) - 2,
             y = min(c(agg_alt_dat[[col]],
                       standard_dat_selected[[col]])) - 0.05,  # Place slightly below the minimum value
             label = paste("Correlation:", round(rho, 3)),
             color = "black", size = 5, hjust = 0.5, vjust = 0) +
    # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
    #           label = paste("Correlation:", round(rho, 3)),
    #           color = "black", size = 3.5, vjust = -1) +
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

  ggsave(filename = paste0("../covid_states_behavior1/figures/supplementary/SA_aggregation/",names(agg_alt_dat)[col],".png"),
                           width = 6, height = 4, units = "in", bg = "white")

}

# Two exposing behaviors ------------------------------------------------------
for(col in 6:7){

  rho = cor(agg_alt_dat[[col]], standard_dat_selected[[col]], method = "pearson")

  agg_alt_dat %>%
    ggplot(aes(x = Wave, y = .[[col]])) +
    geom_line(data = agg_alt_dat, aes(color = "Cumulative")) +
    geom_line(data = standard_dat_selected, aes(x = Wave, y = standard_dat_selected[[col]], color = "Discrete")) +
    scale_color_manual(values = c("Cumulative" = "darkslateblue", "Discrete" = "orange")) +
    labs(x = "Survey wave",
         y = "Participation",
         title = names(agg_alt_dat)[col]) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1, 27, 1)) +
    annotate("text", x = median(agg_alt_dat$Wave) - 2, y = min(c(agg_alt_dat[[col]], standard_dat_selected[[col]])) - 0.05,  # Place slightly below the minimum value
             label = paste("Correlation:", round(rho, 3)),
             color = "black", size = 5, hjust = 0.5, vjust = 0) +  # Center horizontally and place at the bottom
    # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
    #           label = paste("Correlation:", round(rho, 3)),
    #           color = "black", size = 3.5, vjust = -1) +
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

  ggsave(filename = paste0("../covid_states_behavior1/figures/supplementary/SA_aggregation/",names(agg_alt_dat)[col],".png"),
         width = 6, height = 4, units = "in", bg = "white")

}

# Individual figures for combined plot ---------------------------------------------
names(agg_alt_dat)

p1 = agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Avoiding contact with other people`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Avoiding contact with other people`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Avoiding contact with other people") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Avoiding contact with other people`,
                     standard_dat_selected$`Avoiding contact with other people`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.998),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
  # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
  #           label = paste("Correlation:", round(rho, 3)),
  #           color = "black", size = 3.5, vjust = -1) +
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

p2 =  agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Avoiding public or crowded places`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Avoiding public or crowded places`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Avoiding public or crowded places") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Avoiding public or crowded places`,
                     standard_dat_selected$`Avoiding public or crowded places`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.998),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
  # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
  #           label = paste("Correlation:", round(rho, 3)),
  #           color = "black", size = 3.5, vjust = -1) +
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

p3 =  agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Frequently washing hands`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Frequently washing hands`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Frequently washing hands") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Frequently washing hands`,
                     standard_dat_selected$`Frequently washing hands`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.998),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
  # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
  #           label = paste("Correlation:", round(rho, 3)),
  #           color = "black", size = 3.5, vjust = -1) +
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

p4 =  agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Wearing a face mask when outside of your home`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Wearing a face mask when outside of your home`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Wearing a face mask when\noutside of your home") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Wearing a face mask when outside of your home`,
                     standard_dat_selected$`Wearing a face mask when outside of your home`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.998),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
  # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
  #           label = paste("Correlation:", round(rho, 3)),
  #           color = "black", size = 3.5, vjust = -1) +
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

p5 =  agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Been in a room with at least 5 people outside of household in the past 24 hours`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Been in a room with 5-10 people outside of household in the past 24 hours`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with at least 5 people\noutside of household in the past 24 hours") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Been in a room with at least 5 people outside of household in the past 24 hours`,
                     standard_dat_selected$`Been in a room with 5-10 people outside of household in the past 24 hours`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.982),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
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

p6 =  agg_alt_dat %>%
  ggplot(aes(x = Wave, y = `Been in a room with at least 11 people outside of household in the past 24 hours`)) +
  geom_line(data = agg_alt_dat, aes(color = "Alternative aggregation")) +
  geom_line(data = standard_dat_selected, aes(x = Wave, y = `Been in a room with 11-50 people outside of household in the past 24 hours`, color = "Original aggregation")) +
  scale_color_manual(values = c("Alternative aggregation" = "darkslateblue", "Original aggregation" = "orange")) +
  labs(x = "Survey wave",
       y = "Adherence",
       title = "Been in a room with at least 11 people\noutside of household in the past 24 hours") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1, 27, 1)) +
  annotate("text", x = median(agg_alt_dat$Wave) - 2,
           y = min(c(agg_alt_dat$`Been in a room with at least 11 people outside of household in the past 24 hours`,
                     standard_dat_selected$`Been in a room with 11-50 people outside of household in the past 24 hours`)) - 0.05,  # Place slightly below the minimum value
           label = paste("Correlation:", 0.998),
           color = "black", size = 5, hjust = 0.5, vjust = 0) +
  # geom_text(x = 14, y = min(agg_alt_dat[[col]]),
  #           label = paste("Correlation:", round(rho, 3)),
  #           color = "black", size = 3.5, vjust = -1) +
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

png(filename="../covid_states_behavior1/figures/supplementary/SA_aggregation/sa_aggregation_combined.png", width=15, height=10, res = 150, units = "in")
wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom",
                                          legend.text = element_text(size = 24),
                                          axis.title.x = element_blank(),
                                          axis.text.y = element_text(size = 15),
                                          plot.title = element_text(size = 16),

  )
dev.off()
