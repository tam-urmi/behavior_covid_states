`behaviors_perc.csv` has the % doing each behavior for each state and wave, and at the national level (at the end of the table). This is the input to the pipeline 

`behaviors_error_margins.csv` has the 95% CI corresponding to each estimate from the previous file. 

`behaviors_perc_error_margins.csv` has the estimate from `behaviors_perc.csv` together with the error margin from `behaviors_error_margins.csv` in parentheses, after rounding both of them. This is the table we will provide as a SM. 

`behaviors_SI_different_agg.csv` has the national % of the cov_beh and the event variables with alternative aggregations

`behaviors_SI_no_return.csv` has the national % of the behavior variables after keeping only the 1st answer of returning respondents and re-weighting. 

`national_na.csv` has the number of missing answer per question and wave

`state_n.csv` has the sample size of each state-wave-question. 

`n_waves_state_filter.csv` is calculated from the previous dataset, and has the #waves where each state-question has less than 200 answers. 
