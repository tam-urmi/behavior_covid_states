## -----------------------------------------------------------------------------
## Script name: shift_diagram.R
##
## Purpose of script: Build a visualization to demonstrate shifting of time series
##
## Author: George Dewey
##
## Date Created: 2024-10-31
##
## Last Updated: 2024-11-04
## -----------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(patchwork)

# Generate some data

# Severity: keep this constant
severity = rnorm(n = 100, mean = 0, sd = 1)

# Behavior: this is what we shift back and forth
behavior_lag0 = rnorm(n = 100, mean = 0, sd = 2.5)
behavior_lag1 = rnorm(n = 100, mean = -1, sd = 2.5)
behavior_lag2 = rnorm(n = 100, mean = -2, sd = 2.5)
behavior_lead1 = rnorm(n = 100, mean = 1, sd = 2.5)
behavior_lead2 = rnorm(n = 100, mean = 02, sd = 2.5)

# Timescale: use this for the x-axis (don't show)
timescale = 1:100

# Combine these into a df for plotting
plot_data = tibble(
  severity = severity,
  behavior_lag0 = behavior_lag0,
  behavior_lag1 = behavior_lag1,
  behavior_lag2 = behavior_lag2,
  behavior_lead1 = behavior_lead1,
  behavior_lead2 = behavior_lead2,
  time = timescale
)

plot_data %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = severity))

# Plot synchronicity/lag0
p1 = ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), aes(color = "severity"), linewidth = 0.8) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1.25), aes(color = "behavior"), linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1) - 0.025, yend = dnorm(0, mean = 0, sd = 1) + 0.025),
               color = "black", linetype = "solid", size = 0.8) +     # Mean for severity
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1.25) - 0.025, yend = dnorm(0, mean = 0, sd = 1.25) + 0.025),
               color = "gold2", linetype = "solid", size = 0.8) +
  labs(x = "", y = "", title = "Behavior and severity are\nsynchronized (lag 0)") +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(name = "Distribution", values = c("severity" = "black", "behavior" = "gold2")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 5, face = "bold"))
# Plot lags - behavior precedes severity

# Lag 1
p2 = ggplot(data = data.frame(x = c(-4, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), aes(color = "severity"), linewidth = 0.8) +
  stat_function(fun = dnorm, n = 101, args = list(mean = -1, sd = 1.25), aes(color = "behavior"), linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1) - 0.025, yend = dnorm(0, mean = 0, sd = 1) + 0.025),
               color = "black", linetype = "solid", size = 0.8) +     # Mean for severity
  geom_segment(aes(x = -1, xend = -1, y = dnorm(0, mean = 0, sd = 1.25) - 0.025, yend = dnorm(0, mean = 0, sd = 1.25) + 0.025),
               color = "gold2", linetype = "solid", size = 0.8) +
  labs(x = "", y = "", title = "Behavior precedes severity (lag 1)") +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(name = "Distribution", values = c("severity" = "black", "behavior" = "gold2")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 5, face = "bold"))

# Lag 2
p3= ggplot(data = data.frame(x = c(-5, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), aes(color = "severity"), linewidth = 0.8) +
  stat_function(fun = dnorm, n = 101, args = list(mean = -2, sd = 1.25), aes(color = "behavior"), linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1) - 0.025, yend = dnorm(0, mean = 0, sd = 1) + 0.025),
               color = "black", linetype = "solid", size = 0.8) +     # Mean for severity
  geom_segment(aes(x = -2, xend = -2, y = dnorm(0, mean = 0, sd = 1.25) - 0.025, yend = dnorm(0, mean = 0, sd = 1.25) + 0.025),
               color = "gold2", linetype = "solid", size = 0.8) +
  labs(x = "", y = "", title = "Behavior precedes severity (lag 2)") +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(name = "Distribution", values = c("severity" = "black", "behavior" = "gold2")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 5, face = "bold"))

# Plot leads - behavior precedes severity

# Lead 1
p4 = ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), aes(color = "severity"), linewidth = 0.8) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 1, sd = 1.25), aes(color = "behavior"), linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1) - 0.025, yend = dnorm(0, mean = 0, sd = 1) + 0.025),
               color = "black", linetype = "solid", size = 0.8) +     # Mean for severity
  geom_segment(aes(x = 1, xend = 1, y = dnorm(0, mean = 0, sd = 1.25) - 0.025, yend = dnorm(0, mean = 0, sd = 1.25) + 0.025),
               color = "gold2", linetype = "solid", size = 0.8) +
  labs(x = "", y = "", title = "Behavior follows severity (lead 1)") +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(name = "Distribution", values = c("severity" = "black", "behavior" = "gold2")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 5, face = "bold"))

# Lead 2
p5 = ggplot(data = data.frame(x = c(-3, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), aes(color = "Severity"), linewidth = 0.8) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 2, sd = 1.25), aes(color = "Behavior"), linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = 0, y = dnorm(0, mean = 0, sd = 1) - 0.025, yend = dnorm(0, mean = 0, sd = 1) + 0.025),
               color = "black", linetype = "solid", size = 0.8) +     # Mean for severity
  geom_segment(aes(x = 2, xend = 2, y = dnorm(0, mean = 0, sd = 1.25) - 0.025, yend = dnorm(0, mean = 0, sd = 1.25) + 0.025),
               color = "gold2", linetype = "solid", size = 0.8) +
  labs(x = "", y = "", title = "Behavior follows severity (lead 2)") +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(NULL, values = c("Severity" = "black", "Behavior" = "gold2")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 5, face = "bold"))

wrap_plots(p3, p2, p1, p4, p5, nrow = 1) +
  theme(legend.position = "right",
        legend.text = element_text(size = 8))


ggsave(file = "figures/shift_diagram_h2.png", width = 8, height = 2, units = "in", bg = "white")

