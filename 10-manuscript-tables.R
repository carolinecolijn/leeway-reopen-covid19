# code to automatically generate tables for parameters etc. for each
# jurisdiction, for Supplementary Table for how-safe-is-reopening manuscript
source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

# observed_data is a list of tibbles, one for each region (jurisdiction)
# fits is a list of fits, which are covidseir objects with all the info

# Here are what's saved, removing ones I've used or aren't needed
summary(fits$BC)
                     Length Class   Mode
fit                    1    stanfit S4
post                  11    -none-  list
phi_prior              1    -none-  numeric
f_prior                2    -none-  numeric
obs_model              1    -none-  numeric
e_prior_trans          2    -none-  numeric
start_decline_prior    2    -none-  numeric
end_decline_prior      2    -none-  numeric
samp_frac_fixed       77    -none-  numeric
state_0               10    -none-  numeric
daily_cases           77    -none-  numeric
days                  77    -none-  numeric
time                 429    -none-  numeric
last_day_obs           1    -none-  numeric
pars                   9    -none-  numeric
stan_data             36    -none-  list
days_back              1    -none-  numeric
>

# If order of Details changes then also need to change order in the loop
details <- tibble::tibble("Detail" = c("Data start",
                                       "Data end",
                                       "$I_0$",     # incidence 30 days before
                                                    # day 1 (or 0?).
                                       "$f_2$ prior: Beta(mean, sd)",
                                       "$R_{\\mathrm{0b}}$ prior: Lognormal(logmean, sd)"
                                       )
                          )

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
                      fits[[i]]$state_0["I_frac"],
                      paste0(fits[[i]]$f2_prior_beta_shape1,
                             ", ",
                             fits[[i]]$f2_prior_beta_shape2),
                      paste0(round(fits[[i]]$R0_prior[1], 2),
                             ", ",
                             round(fits[[i]]$R0_prior[2], 2))
                    ))

}

as.data.frame(details)

kableExtra::kable(details,
                  format = "latex",
                  booktabs = TRUE,
                  escape = FALSE) %>%
  kableExtra::column_spec(1, width="30mm")
