# Load some packages, functions, and global variables:
source("selfIsolationModel/contact-ratios/model-prep.R")

# Notes ---------------------------------------------------------------------

# Physical distancing timeline:
#   - 16 March: advised against nonessential travel, going out, going to work
#   - 20 March: all pubs, restaurants, gyms, schools close
#   - 23 March: nationwide lockdown. (Only basic shopping, exercise, medical need, essential work)
# Testing:
#   - 15 March: begin rolling out tests for care home staff and residents
#   - 17 April: widened testing for police officers, firefighters and prison staff
#   - 23 April: daily capacity hits 50,000. Testing for key workers and their households. This does not really happen.
#   - 1 May: target of 100,000 tests per day - ramps up week 23rd Apr-1st May
#   - Testing decreases again somewhat after May 1st
# population UK: approx. 66.4e6

# Read and prepare data -----------------------------------------------------

# dat <- readr::read_csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-totals-uk.csv")
dat <- readr::read_csv(here(this_folder, "data-raw/UK.csv"))
dat <- rename(dat, date = Date)
dat$cases <- c(dat$ConfirmedCases[1], diff(dat$ConfirmedCases))
dat$daily_tests <- c(dat$Tests[1], diff(dat$Tests))

n <- dim(dat)[1]
if (is.na(tail(dat$cases, n = 1))) {
  dat <- dat[1:(n - 1), ]
}
# gives NAs if the deaths have been released today but not the cases yet

# View(dat)
ggplot(dat, aes(date, cases)) +
  geom_point()

# Pick a reasonable starting date:
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
dat$day <- seq_len(nrow(dat))
ggplot(dat, aes(date, cases)) +
  geom_point()

# Try redistributing Apr 11th bump:
dat$adjust_cases <- dat$cases
excess <- (dat[42, ]$cases - dat[43, ]$cases) / 5
dat[42, ]$adjust_cases <- dat[43, ]$cases
dat[37:41, ]$adjust_cases <- round(dat[37:41, ]$cases + excess)
dat$value <- dat$adjust_cases # for plotting function
ggplot(dat, aes(day, value)) +
  geom_point()

saveRDS(dat, file.path(this_folder, "data-generated/UK-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit_file <- file.path(this_folder, "data-generated/UK-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = c(rep(0.2, 60), rep(0.3, nrow(dat) - 60)),
    # incorporating testing ramped up 30/04 = day 61
    i0_prior = c(log(1), 1),
    start_decline_prior = c(log(16), 0.1),
    end_decline_prior = c(log(23), 0.1),
    N_pop = 66.4e6,
    chains = CHAINS,
    iter = ITER
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}
print(fit)
make_traceplot(fit)
#
# # Check fit -----------------------------------------------------------------
#
# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
# proj_tidy <- covidseir::tidy_seir(proj)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()
#
# # Calculate threshold for increase ------------------------------------------
#
# # Need to pick reasonable f(s) values for a reasonable time span
# # such that fitting a linear regression makes sense.
# # Make sure the plot that comes out of this is linear:
# threshold <- get_thresh(fit, iter = 1:50,
#   forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
# round(threshold, 2)
# saveRDS(threshold, here(this_folder, "data-generated/UK-threshold.rds"))
#
# # Quick plot:
# hist(fit$post$f_s[,1],
#   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
