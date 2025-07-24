setwd("COVID19/CODE")
source("DATA PROCESS/csp_functions.R")
source("../OLD FILES/CODE/states_report_prep.R")
library(pollster)
library(tidyverse)
library(survey)
library(srvyr)
library(haven)
library(labelled)

path <- "PAPERS/behaviors/"

### read_data
waves <- c(1:3, 5, 7, 9:14, 16:27)

datasets <- lapply(waves, function(i) readRDS(paste0("../DATA/RDS/CSP_W", i, ".RDS")))


datasets[[11]] <- datasets[[11]] %>% filter(StartDate < as.Date("2021-01-11"))
datasets[[15]] <- datasets[[15]] %>% filter(StartDate < as.Date("2021-09-28"))
datasets[[17]] <- datasets[[17]] %>% filter(StartDate < as.Date("2022-01-26"))
datasets[[19]] <- datasets[[19]] %>% filter(StartDate < as.Date("2022-07-06"))

### set up variable names and fill NA from -99

visit_variables <- paste0("visit_", c(1,2,4,8,9,10,12))
cov_beh_variables <- paste0("cov_beh_", c(1,2,3,5))

datasets <- lapply(datasets, function(d) d %>% 
                     mutate(across(all_of(c(cov_beh_variables, "event")), 
                            ~ zap_missing(.)))
)

cov_beh_map <- substring(var_label(datasets[[1]][cov_beh_variables]),100,200)
cov_beh_map <- setNames(cov_beh_map, cov_beh_variables)

rename_lookup <- c(
  sapply(visit_variables, function(var) val_label(datasets[[1]][[var]], 1)),
  cov_beh_map,
  event_at_least_1 = "Been in a room with someone outside of household in the past 24 hours",
  event_5_10 = "Been in a room with 5-10 people outside of household in the past 24 hours",
  event_11_50 = "Been in a room with 11-50 people outside of household in the past 24 hours",
  event_50 = "Been in a room with over 50 people outside of household in the past 24 hours"
)
rename_lookup <- setNames(names(rename_lookup), rename_lookup)

rename_lookup_se <- setNames( str_c(rename_lookup, "_se"), names(rename_lookup))


### main text numbers 

national_dat <- lapply(datasets,
       function(dat) dat %>% as_survey_design(weight=weight) %>%
         summarise(Wave = first(wave),
                   across(all_of(visit_variables), 
                          ~survey_mean(., na.rm = T)*100),
                   across(all_of(cov_beh_variables),
                          ~survey_mean(.==4, na.rm = T)*100),
                   event_at_least_1 = survey_mean(event > 1, na.rm = T)*100,
                   event_5_10 = survey_mean(event %in% c(4,5,6), na.rm = T)*100,
                   event_11_50 = survey_mean(event == 7, na.rm = T)*100,
                   event_50 = survey_mean(event > 7, na.rm = T)*100,
                   Start_Date = min(StartDate),
                   End_Date = max(EndDate),
                   N = sum(!is.na(event)),
                   Total_N = n())
) %>% bind_rows() 


state_dat <- lapply(datasets,
                       function(dat) dat %>% as_survey_design(weight=weight_state) %>%
                         group_by(state_code) %>% 
                         summarise(Wave = first(wave),
                                   across(all_of(visit_variables), 
                                          ~survey_mean(., na.rm = T)*100),
                                   across(all_of(cov_beh_variables), 
                                          ~survey_mean(.==4, na.rm = T)*100),
                                   event_at_least_1 = survey_mean(event > 1, na.rm = T)*100,
                                   event_5_10 = survey_mean(event %in% c(4,5,6), na.rm = T)*100,
                                   event_11_50 = survey_mean(event == 7, na.rm = T)*100,
                                   event_50 = survey_mean(event > 7, na.rm = T)*100,
                                   Start_Date = min(StartDate),
                                   End_Date = max(EndDate),
                                   N = sum(!is.na(event)),
                                   Total_N = n())
) %>% bind_rows() 


numbers_df <- bind_rows(
  select(state_dat, !ends_with("se")),
  select(national_dat, !ends_with("se")) %>% 
    mutate(state_code = "National"),
) %>%
  rename(all_of(rename_lookup)) 

error_margin_df <- bind_rows(
  select(state_dat, state_code, Wave, ends_with("se")), 
  select(national_dat, Wave, ends_with("se")) %>% 
           mutate(state_code = "National")
) %>%
  mutate(across(ends_with("se"), ~.*1.96)) %>%
  rename(all_of(rename_lookup_se))

full_error_tab <- bind_cols(
  numbers_df %>% select(state_code, Wave) %>% filter(Wave <= 23),
  map2_dfc(filter(numbers_df, Wave <= 23)[,3:17] %>%
             mutate(across(everything(), ~round(.,0))),
        filter(error_margin_df, Wave <=23)[3:17] %>%
          mutate(across(everything(), ~round(.,0))),
        ~ str_c(.x, " (", .y, ")")),
) %>%
  arrange(Wave, factor(state_code, levels = c("National", unique(state_dat$state_code)))) %>%
  relocate(Wave, state_code)


write_csv(numbers_df, str_c(path, "behaviors_perc.csv"))
write_csv(error_margin_df, str_c(path, "behaviors_error_margins.csv"))
write_csv(full_error_tab, str_c(path, "behaviors_perc_error_margins.csv"))


#### different aggregations for SI

cov_beh_map_si <- substring(var_label(datasets[[1]][cov_beh_variables]),100,200) %>%
  str_c(., "_somewhat likely + very likely")
cov_beh_map_si <- setNames(cov_beh_map_si, cov_beh_variables)
rename_lookup_si <- c(
  cov_beh_map_si,
  event_at_least_5 = "Been in a room with at least 5 people outside of household in the past 24 hours",
  event_at_least_11 = "Been in a room with at least 11 people outside of household in the past 24 hours"
)

rename_lookup_si <- setNames(names(rename_lookup_si), rename_lookup_si)


national_dat_SI <- lapply(datasets,
                       function(dat) dat %>% as_survey_design(weight=weight) %>%
                         summarise(Wave = first(wave),
                                   across(all_of(cov_beh_variables),
                                          ~survey_mean(.>=3, na.rm = T)*100),
                                   event_at_least_5 = survey_mean(event >= 4, na.rm = T)*100,
                                   event_at_least_11 = survey_mean(event >= 7, na.rm = T)*100,
                                   Start_Date = min(StartDate),
                                   End_Date = max(EndDate))
) %>% bind_rows() %>%
  select(!ends_with("se")) %>%
  rename(all_of(rename_lookup_si))

write_csv(national_dat_SI, str_c(path, "behaviors_SI_different_agg.csv"))



## Si without returning respondents

datasets_si <- bind_rows(
  lapply(datasets, 
         function(d) select(d, id, wave, state_code, state, all_of(visit_variables), 
                            all_of(cov_beh_variables), event, weight, weight_state,
                            StartDate, EndDate, age, urbanicity, region, gender, 
                            age_cat_4, age_cat_6, income, race, 
                            education_cat, urban_type, party3, employ, education))
  ) %>% 
  distinct(id, .keep_all = T) %>%
  group_by(wave) %>%
  group_split(.keep = T)


national_dat_no_return <- lapply(datasets_si,
                       function(dat) dat %>% 
                         mutate(weight = weight_csp(dat = .)) %>%
                         as_survey_design(weight=weight) %>%
                         summarise(Wave = first(wave),
                                   across(all_of(visit_variables), 
                                          ~survey_mean(., na.rm = T)*100),
                                   across(all_of(cov_beh_variables),
                                          ~survey_mean(.==4, na.rm = T)*100),
                                   event_at_least_1 = survey_mean(event > 1, na.rm = T)*100,
                                   event_5_10 = survey_mean(event %in% c(4,5,6), na.rm = T)*100,
                                   event_11_50 = survey_mean(event == 7, na.rm = T)*100,
                                   event_50 = survey_mean(event > 7, na.rm = T)*100,
                                   Start_Date = min(StartDate),
                                   End_Date = max(EndDate))
) %>% bind_rows() %>%
  select(!ends_with("se")) %>%
  rename(all_of(rename_lookup))

write_csv(national_dat_no_return, str_c(path, "behaviors_SI_no_return.csv"))


### Tables with sample sizes and total N's


sapply(datasets[1:19], function(d) nrow(d)) %>% sum()

bind_rows(
  lapply(datasets[1:19], 
         function(d) select(d, id, wave))
) %>% 
  distinct(id) %>%
  nrow()



national_na <- lapply(datasets[1:19], function(dat) dat %>% 
         summarise(wave = first(wave),
                   across(all_of(visit_variables), ~ sum(is.na(.))),
                   across(all_of(cov_beh_variables), ~ sum(is.na(.))),
                   event = sum(is.na(event)))) %>%
  bind_rows()

write_csv(national_na, str_c(path, "national_na.csv"))



state_n <- lapply(datasets[1:19], function(dat) dat %>%
                    group_by(state_code) %>%
                    summarise(Wave = first(wave),
                              across(all_of(visit_variables), ~ sum(!is.na(.))),
                              across(all_of(cov_beh_variables), ~ sum(!is.na(.))),
                              event = sum(!is.na(event)))) %>%
  bind_rows() %>%
  rename(all_of(rename_lookup[1:11]),
         `People out of household been in a room with` = event)

write_csv(state_n, str_c(path, "state_n.csv"))

n_small_waves <- state_n %>% 
  mutate(across(3:14, ~.<200)) %>%
  group_by(state_code) %>%
  summarize(across(2:13, ~sum(.)))

write_csv(n_small_waves, str_c(path, "n_waves_state_filter.csv"))


large_states <- n_small_waves %>% 
  summarise(state_code = state_code,
            total_small_waves = rowSums(across(2:13))) %>%
  filter(total_small_waves == 0) %>%
  pull(state_code)

length(large_states)

error_margin_flt <- error_margin_df %>%
  filter(state_code %in% large_states) %>%
  select(3:17) 

mean(as.matrix(error_margin_flt))
max(error_margin_flt)

bool_m <- as.matrix(state_n[,3:14]) > 200
bool_m <- cbind(bool_m, bool_m[,12], bool_m[,12], bool_m[,12])

error_margin_flt <- error_margin_df[,3:17]
error_margin_flt <- as.data.frame(error_margin_flt)[bool_m]
max(error_margin_flt)

replace(as.data.frame(error_margin_flt), !bool_m, 0)
