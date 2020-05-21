# Load some packages, functions, and global variables:
source("selfIsolationModel/contact-ratios/model-prep.R")

# Notes ---------------------------------------------------------------------

# Read and prepare data -----------------------------------------------------

#dat <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")

dat <- readr::read_csv("nCoVDailyData/CaseCounts/BC Case Counts.csv")
names(dat)[names(dat) == "BC"] <- "Cases"
dat$Date <- lubridate::dmy(dat$Date)
dat$day <- seq_len(nrow(dat))
dat$Cases

dat$daily_diffs <- c(NA, diff(dat$Cases))
dat <- dplyr::filter(dat, Date >= ymd("2020-03-01"))
dat <- dplyr::filter(dat, Date <= ymd("2020-05-16"))
dat <- select(dat, date = Date, value = daily_diffs)
dat$day <- seq_along(dat$date)

# dat <- readr::read_csv(here(this_folder,"data-raw/CAN.csv"))
# dat$date <- lubridate::dmy(dat$date_report)
# dat <- dplyr::filter(dat, province == "BC")
# # View(dat)
# ggplot(dat, aes(date, cases)) +
#   geom_point()
# # Pick a reasonable starting date:
# dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
# dat$day <- seq_len(nrow(dat))
# ggplot(dat, aes(date, cases)) +
#   geom_point() +
#   geom_line()
#
# # Try redistributing Apr 01 bump:
# dat$adjust_cases <- dat$cases
# excess <- (dat[32,]$cases - dat[33,]$cases)/5
# dat[32,]$adjust_cases <- dat[33,]$cases
# dat[27:31,]$adjust_cases <- round(dat[27:31,]$cases + excess)
# dat$value <- dat$adjust_cases # for plotting function
# ggplot(dat, aes(day, value)) +
#   geom_point()

saveRDS(dat, file.path(this_folder, "data-generated/BC-dat.rds"))

samp_frac <- c(rep(0.14, 13), rep(0.21, 40 - 13), rep(0.21, 11))
samp_frac <- c(samp_frac, rep(0.37, nrow(dat) - length(samp_frac)))
samp_frac
plot(dat$date, samp_frac)
abline(v = ymd("2020-03-14"))
abline(v = ymd("2020-04-10"))
abline(v = ymd("2020-04-21"))

# Fit model -----------------------------------------------------------------

fit_file <- file.path(this_folder, "data-generated/BC-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    e_prior = c(0.8, 0.05),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = ITER
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)
make_traceplot(fit)

# Check fit -----------------------------------------------------------------
#
# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 20)
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
# threshold <- get_thresh(fit)
# saveRDS(threshold,
#   here(this_folder, "data-generated/BC-threshold.rds"))
#
# # # Quick plot:
# # hist(fit$post$f_s[,1],
# #   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# # abline(v = threshold, col = "red", lwd = 2)
