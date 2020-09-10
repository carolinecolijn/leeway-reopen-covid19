# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# Notes ---------------------------------------------------------------------

# Read and prepare data -----------------------------------------------------

dat <- readr::read_csv("data-raw/BC.csv")
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$Cases

dat$daily_diffs <- c(NA, diff(dat$Cases))
dat <- dplyr::filter(dat, Date >= ymd("2020-03-01"))
dat <- select(dat, date = Date, value = daily_diffs)
dat$day <- seq_along(dat$date)

saveRDS(dat, file.path("data-generated/BC-dat.rds"))
dat <- dplyr::filter(dat, date <= ymd("2020-06-04"))

# Based on estimation with hospital data in other model:
samp_frac <- c(rep(0.14, 13), rep(0.21, 40 - 13), rep(0.21, 11))
samp_frac <- c(samp_frac, rep(0.37, nrow(dat) - length(samp_frac)))
samp_frac
plot(dat$date, samp_frac)
abline(v = ymd("2020-03-14"))
abline(v = ymd("2020-04-10"))
abline(v = ymd("2020-04-21"))

# Fit model -----------------------------------------------------------------

fit_file <- file.path("data-generated/BC-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = ITER,
    e_prior = c(0.8, 0.05),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)
make_traceplot(fit)

# # Check fit -----------------------------------------------------------------
# # #
# proj <- covidseir::project_seir(fit, iter = 1:100, forecast_days = 20)
# proj_tidy <- covidseir::tidy_seir(proj, resample_y_rep = 150)
# #
# proj_tidy %>%
#   covidseir::plot_projection(dat) +
#   ggsidekick::theme_sleek() +
#   facet_null()
#
# p1 <- project_seir(fit,
#   forecast_days = 100,
#   iter = 1:100,
#   f_fixed_start = nrow(fit$daily_cases) + 20,
#   f_fixed = rep(0.85, 81)
# )
# covidseir::tidy_seir(p1, resample_y_rep = 150) %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()
#
#
# p2 <- project_seir(fit,
#   forecast_days = 30,
#   iter = 1:100,
#   f_fixed_start = nrow(fit$daily_cases) + 1,
#   f_fixed = rep(0.85, 30)
# )
#
# source("analysis/old_get_thresh.R")
# source("analysis/old_project_seir.R")
#
# thresholds_old <- map(list(fit), old_get_thresh,
#   iter = 1:200)
# hist(thresholds_old[[1]])
#
# fit <- readRDS("data-generated/BC-fit.rds")
# load_all("../covidseir/")
# thresholds_new <- map(list(fit), covidseir::get_threshold,
#   iter = 1:200)
# hist(thresholds_new[[1]], breaks = 30, xlim = c(0.25, 0.6))
#
# thresholds_new_old <- map(list(fit), covidseir::get_threshold,
#   iter = 1:200)
# hist(thresholds_old[[1]], breaks = 30, xlim = c(0.25, 0.6))
#
# fit <- readRDS("data-generated/BC-fit.rds")
# fit$stan_data$f_prior <- cbind(rep(2, 27), rep(3, 27))
# fit$stan_data$ode_control <- c(1e-07, 1e-06, 1e+06)
# fit$stan_data$S
# thresholds3 <- map(list(fit), old_get_thresh, iter = 1:20)
#
# fit <- readRDS("data-generated/BC-fit.rds")
# thresholds4 <- map(list(fit), covidseir::get_threshold, iter = 1:220)
#
# hist(thresholds3[[1]], breaks = 30, xlim = c(0.25, 0.6))
# hist(thresholds4[[1]], breaks = 30, xlim = c(0.25, 0.6))
#
# thresholds5 <- map(list(fit2), covidseir::get_threshold, iter = 1:200)
# hist(thresholds5[[1]], breaks = 30, xlim = c(0.25, 0.6))
#
# thresholds6 <- map(list(fit3), covidseir::get_threshold, iter = 1:200)
# hist(thresholds6[[1]], breaks = 30, xlim = c(0.25, 0.6))

# ggsave("~/Downloads/bc-test.svg", width = 5, height = 3.5)

# proj_tidy %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()
#
# # Calculate threshold for increase ------------------------------------------
#
# # Need to pick reasonable f(s) values for a reasonable time span
# # such that fitting a linear regression makes sense.
# # Make sure the plot that comes out of this is linear:
# threshold <- get_thresh(fit)
# saveRDS(threshold,
#   here(this_folder, "data-generated/BC-threshold.rds"))
#
# # # Quick plot:
# # hist(fit$post$f_s[,1],
# #   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# # abline(v = threshold, col = "red", lwd = 2)
