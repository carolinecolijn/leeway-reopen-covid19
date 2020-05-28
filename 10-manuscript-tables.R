# code to automatically generate tables for parameters etc. for each
# jurisdiction, for Supplementary Table for how-safe-is-reopening manuscript.
# Delay shape and scale are not automatic, but taken from the input files
source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

# observed_data is a list of tibbles, one for each region (jurisdiction)
# fits is a list of fits, which are covidseir objects with all the info

# Here are what's saved:
# summary(fits$BC)
#                      Length Class   Mode
# fit                    1    stanfit S4
# post                  11    -none-  list
# phi_prior              1    -none-  numeric
# R0_prior               2    -none-  numeric
# f_prior                2    -none-  numeric
# obs_model              1    -none-  numeric
# e_prior_trans          2    -none-  numeric
# start_decline_prior    2    -none-  numeric
# end_decline_prior      2    -none-  numeric
# samp_frac_fixed       77    -none-  numeric
# state_0               10    -none-  numeric
# daily_cases           77    -none-  numeric
# days                  77    -none-  numeric
# time                 429    -none-  numeric
# last_day_obs           1    -none-  numeric
# pars                   9    -none-  numeric
# f2_prior_beta_shape1   1    -none-  numeric
# f2_prior_beta_shape2   1    -none-  numeric
# stan_data             36    -none-  list
# days_back              1    -none-  numeric

# If order of Details column changes then also need to change order in the loop, this
details <- tibble::tibble("Detail" = c("Data start",
                                       "Data end",
                                       "Prior mean for $t_1$",
                                       "Prior mean for $t_2$",
                                       "Delay shape",
                                       "Delay scale",
                                       "Sampling fraction(s)**",
                                       "$N$: population (millions)"
                                       )
                          )

# Fill in a column for each REGION
for(i in 1:length(REGIONS)){
  details <- details %>%
    dplyr::mutate(!!REGIONS[i] := c(
                      paste(lubridate::month(min(observed_data[[i]]$date),
                                             label = TRUE,
                                             abbr = TRUE),
                            lubridate::day(min(observed_data[[i]]$date))),
                      paste(lubridate::month(max(observed_data[[i]]$date),
                                             label = TRUE,
                                             abbr = TRUE),
                            lubridate::day(max(observed_data[[i]]$date))),
                      paste(lubridate::month(exp(fits[[i]]$start_decline_prior)[1]
                                             + min(observed_data[[i]]$date),
                                             label = TRUE,
                                             abbr = TRUE),
                            lubridate::day(exp(fits[[i]]$start_decline_prior)[1]
                                           + min(observed_data[[i]]$date))),
                      paste(lubridate::month(exp(fits[[i]]$end_decline_prior)[1]
                                             + min(observed_data[[i]]$date),
                                             label = TRUE,
                                             abbr = TRUE),
                            lubridate::day(exp(fits[[i]]$end_decline_prior)[1]
                                           + min(observed_data[[i]]$date))),
                      ifelse(REGIONS[i] == "NZ", 1.53, 1.73),
                      ifelse(REGIONS[i] == "NZ", 7.83, 9.85),
                      paste(unique(fits[[i]]$samp_frac), collapse=", "),
                      sprintf("%.2f", fits[[i]]$pars["N"]/1e6, 2)
                    ))
}

as.data.frame(details)

kableExtra::kable(details,
                  format = "latex",
                  booktabs = TRUE,
                  escape = FALSE) %>%
  kableExtra::column_spec(1, width="30mm") %>%
  kableExtra::column_spec(2, width="10mm") %>%
  kableExtra::kable_styling(font_size = 8)
# Then paste that into .tex on Overleaf
