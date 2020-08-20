# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

source("fit2.R")
library(rstan)
my_model <- stan_model("stiff_test.stan")
# Notes ---------------------------------------------------------------------

# Physical distancing timeline:
#   - March 12: recommend cancelling large mass gatherings of over 1,000
#   - March 13: recommending all gatherings over 250 be cancelled
#   - March 14--24: Ramp-up in measures
#   - March 25: closed all non-essential workplaces
#   - March 30: public and private parks, sport fields, beaches,playgrounds,
#     dog parks closed urging to stay home unless essential, particularly
#     70+ and immune compromised
# Testing:
#   - April 3: ended test backlog, will prioritize LTC residents, workers
#   - April 11: list of symptoms and testing expanded; daily tests processed
#     expected to double
#   - April 9: testing expanded to all health workers, first responders with
#     symptoms, new residents, and workers at LTC
#   - maybe testing backlog ended around April 1?
# population Ontario: approx. 14.5e6

# Read and prepare data -----------------------------------------------------

# dat <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")
dat <- readr::read_csv(file.path("data-raw/CAN.csv"))
dat$date <- lubridate::dmy(dat$date_report)
dat <- dplyr::filter(dat, province == "Ontario")
# View(dat)
ggplot(dat, aes(date, cases)) +
  geom_point()
# Pick a reasonable starting date:
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
dat$day <- seq_len(nrow(dat))
ggplot(dat, aes(date, cases)) +
  geom_point()

# Try redistributing Apr 01 bump:
dat$adjust_cases <- dat$cases
excess <- (dat[32, ]$cases - dat[33, ]$cases) / 5
dat[32, ]$adjust_cases <- dat[33, ]$cases
dat[27:31, ]$adjust_cases <- round(dat[27:31, ]$cases + excess)
dat$value <- dat$adjust_cases # for plotting function
ggplot(dat, aes(day, value)) +
  geom_point()

ggplot(dat, aes(date, value)) +
  geom_point()

saveRDS(dat, file.path("data-generated/ON-dat.rds"))
dat <- dplyr::filter(dat, date <= ymd("2020-06-07"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 40, length.out = 200)
# plot(x, dlnorm(x, log(12), 0.1), type = "l", xaxs = "i", yaxs = "i")

tictoc::tic()
fit_file <- file.path("data-generated/ON-fit.rds")
# if (!file.exists(fit_file)) {
  fit <- fit2(
    daily_cases = dat$value,
    time_increment = 1,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = i0_PRIOR,
    start_decline_prior = c(log(get_google_start("Ontario", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Ontario", dat)), 0.1),
    N_pop = 14.5e6,
    f_seg = make_f_seg(dat, "2020-05-01"),
    chains = 1,
    iter = 150,
    ode_control = c(1e-6, 1e-5, 1e5)
  )
#   saveRDS(fit, fit_file)
# } else {
#   fit <- readRDS(fit_file)
# }
tictoc::toc()

print(fit)
make_traceplot(fit)

# Check fit -----------------------------------------------------------------

# proj <- covidseir::project_seir(fit, iter = 1:3, forecast_days = 20)
# proj_tidy <- covidseir::tidy_seir(proj)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()

# Calculate threshold for increase ------------------------------------------

# Need to pick reasonable f(s) values for a reasonable time span
# such that fitting a linear regression makes sense.
# Make sure the plot that comes out of this is linear:
# threshold <- get_thresh(fit)
# saveRDS(threshold,
#   here(this_folder, "data-generated/ON-threshold.rds"))

# # Quick plot:
# hist(fit$post$f_s[,1],
#   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
