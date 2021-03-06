# code to automatically generate tables for parameters etc. for each
# jurisdiction, for Supplementary Table for how-safe-is-reopening manuscript.
# Delay shape and scale are not automatic, but taken from the input files
source("analysis/model-prep.R")
source("analysis/projection-prep.R")

# posterior table for paper: ------------------
get_stats <- function(x) {
  s <- rstan::summary(x$fit)
  s <- s$summary %>% as.data.frame()
  s$Parameter <- row.names(s)
  s <- s %>% select(Parameter, mean, `2.5%`, `50%`, `97.5%`, n_eff, Rhat)
  s <- filter(s, Parameter %in% c("R0", "i0", "e", "f_s[1]", "f_s[2]", "phi[1]", "start_decline", "end_decline"))
  row.names(s) <- NULL
  s$Parameter <- gsub("phi\\[1\\]", "$\\\\phi$", s$Parameter)
  s$Parameter <- gsub("f_s\\[1\\]", "$f_1$", s$Parameter)
  s$Parameter <- gsub("f_s\\[2\\]", "$f_2$", s$Parameter)
  s$Parameter <- gsub("^i0$", "$I_0$", s$Parameter)
  s$Parameter <- gsub("^e$", "$e$", s$Parameter)
  s$Parameter <- gsub("^start_decline$", "$t_1$", s$Parameter)
  s$Parameter <- gsub("^end_decline$", "$t_2$", s$Parameter)
  s$Parameter <- gsub("^R0$", "$R_{0\\\\mathrm{b}}$", s$Parameter)
  s
}
purrr::map_dfr(fits, get_stats, .id = "Region") %>%
  knitr::kable(format = "latex", digits = c(0, 0, 2, 2, 2, 2, 0, 2), escape = FALSE, table.envir = "table", booktabs = TRUE, longtable = TRUE,
    col.names = c("Region", "Parameter", "Mean", "2.5\\%", "50\\%", "97.5\\%", "ESS", "$\\hat{R}$"), linesep = c('', '', '', '', '', '', '', '\\addlinespace')) %>%
  kableExtra::kable_styling(latex_options = c("repeat_header"))
# knitr::kable(format = "pandoc", digits = c(0, 0, 2, 2, 2, 2, 0, 2))

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
details <- tibble::tibble(
  "Detail" = c(
    "Data start",
    "Data end",
    "Prior mean for $t_1$",
    "Prior SD for $t_1$",
    "Prior mean for $t_2$",
    "Prior SD for $t_2$",
    "Prior mean for $e$",
    "Delay shape",
    "Delay scale",
    "Sampling fraction(s)",
    "Log mean for $I_0$ prior",
    "$N$: population (millions)"
  )
)

# Fill in a column for each REGION
for (i in 1:length(REGIONS)) {
 # if (
 #    fits[[i]]$stan_data$start_decline_prior[2] !=
 #    fits[[i]]$stan_data$end_decline_prior[2]
 #  ) # need both separately if this fails
 #   warning("CHECK SDs of decline priors; they don't match.")
  details <- details %>%
    dplyr::mutate(!!REGIONS[i] := c(
      paste(
        lubridate::month(min(observed_data[[i]]$date),
          label = TRUE,
          abbr = TRUE
        ),
        lubridate::day(min(observed_data[[i]]$date))
      ),
      paste(
        lubridate::month(max(observed_data[[i]]$date),
          label = TRUE,
          abbr = TRUE
        ),
        lubridate::day(max(observed_data[[i]]$date))
      ),
      paste(
        lubridate::month(exp(fits[[i]]$start_decline_prior)[1]
        + min(observed_data[[i]]$date),
        label = TRUE,
        abbr = TRUE
        ),
        lubridate::day(exp(fits[[i]]$start_decline_prior)[1]
        + min(observed_data[[i]]$date))
      ),
      fits[[i]]$stan_data$start_decline_prior[2],
      paste(
        lubridate::month(exp(fits[[i]]$end_decline_prior)[1]
        + min(observed_data[[i]]$date),
        label = TRUE,
        abbr = TRUE
        ),
        lubridate::day(exp(fits[[i]]$end_decline_prior)[1]
        + min(observed_data[[i]]$date))
      ),
      fits[[i]]$stan_data$end_decline_prior[2],
      # paste0(fits[[i]]$e_prior_trans[1],
      #       ", ",
      #       fits[[i]]$e_prior_trans[2]),  # these are transformed
      ifelse(REGIONS[i] == "NZ", 0.9, 0.8),
      ifelse(REGIONS[i] == "NZ", 1.53, 1.73),
      ifelse(REGIONS[i] == "NZ", 7.83, 9.85),
      paste(unique(fits[[i]]$samp_frac), collapse = ", "),
      round(fits[[i]]$stan_data$i0_prior[1], 2),
      sprintf("%.2f", fits[[i]]$pars["N"] / 1e6, 2)
    ))
}

as.data.frame(details)

kableExtra::kable(details,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE
) %>%
  kableExtra::column_spec(1, width = "30mm") %>%
  kableExtra::column_spec(2, width = "10mm") %>%
  kableExtra::kable_styling(font_size = 8)
# Then paste that into .tex on Overleaf

# To check a value:
for (i in 1:length(REGIONS)) {
  print(fits[[i]]$stan_data$end_decline_prior[2])
}
