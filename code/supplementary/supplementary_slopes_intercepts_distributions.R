library(tidyverse)

dat = read_csv("behavior_trends_state_level_gradient_yint.csv") %>% select(-`...1`)
names(dat) = c("behaviors", "slope", "y_int", "state", "party", "behavior_type")

# Gather the needed values

## REP slopes and intercepts
slopes_REP_risk_averting = dat %>%
  filter(party == "REP", behavior_type == "Risk-averting") %>%
  select(slope) %>% pull()

slopes_REP_risk_seeking = dat %>%
  filter(party == "REP", behavior_type == "Risk-seeking") %>%
  select(slope) %>% pull()

intercepts_REP_risk_averting = dat %>%
  filter(party == "REP", behavior_type == "Risk-averting") %>%
  select(y_int) %>% pull()

intercepts_REP_risk_seeking = dat %>%
  filter(party == "REP", behavior_type == "Risk-seeking") %>%
  select(y_int) %>% pull()

## DEM slopes and intercepts
slopes_DEM_risk_averting = dat %>%
  filter(party == "DEM", behavior_type == "Risk-averting") %>%
  select(slope) %>% pull()

slopes_DEM_risk_seeking = dat %>%
  filter(party == "DEM", behavior_type == "Risk-seeking") %>%
  select(slope) %>% pull()

intercepts_DEM_risk_averting = dat %>%
  filter(party == "DEM", behavior_type == "Risk-averting") %>%
  select(y_int) %>% pull()

intercepts_DEM_risk_seeking = dat %>%
  filter(party == "DEM", behavior_type == "Risk-seeking") %>%
  select(y_int) %>% pull()

# Median differences
median(slopes_REP_risk_averting) - median(slopes_DEM_risk_averting) #0.131
median(slopes_DEM_risk_seeking) - median(slopes_REP_risk_seeking) #-0.044

median(slopes_DEM_risk_averting) - median(slopes_REP_risk_averting) #-0.131
median(slopes_DEM_risk_seeking) - median(slopes_REP_risk_seeking) #0.044


median(intercepts_REP_risk_seeking) - median(intercepts_DEM_risk_seeking)
median(intercepts_REP_risk_averting) - median(intercepts_DEM_risk_averting)


# Run the Mann-Whitney tests
# Risk averting slopes
wilcox.test(slopes_REP_risk_averting, slopes_DEM_risk_averting)
# p = 0.0254

# Risk exposing slopes
wilcox.test(slopes_REP_risk_seeking, slopes_DEM_risk_seeking)
# p = 0.0037

# Risk averting intercepts
wilcox.test(intercepts_REP_risk_averting, intercepts_DEM_risk_averting)
# p < 0.0001

# Risk exposing intercepts
wilcox.test(intercepts_REP_risk_seeking, intercepts_DEM_risk_seeking)
# p = 0.006



