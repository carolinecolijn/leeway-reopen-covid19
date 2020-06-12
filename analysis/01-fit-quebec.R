# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# Notes ---------------------------------------------------------------------

# SD timeline:
#   - March 12: Ban on all indoor events with more than 250 people
#   - March 13: all K-19. post-secondary schools and daycares closed
#   - March 22: All restaurants to close their dining rooms
#   - March 23: Banned indoor gatherings
#   - March 23: ordered all shopping malls to close, with the exception of grocery stores
#   - March 24: banned all gatherings

# Testing:
#   - testing data jumps: Mar 24, Mar 28
# Notes:
# https://montrealgazette.com/news/latest-covid-19-statistics-for-quebec-include-1317-previously-unreported-cases-for-april/
# population quebec: approx. 8.5e6

# Read and prepare data -----------------------------------------------------

dat <- readr::read_csv(file.path("data-raw/CAN.csv"))
dat$date <- lubridate::dmy(dat$date_report)
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
qc_dat <- dplyr::filter(dat, province == "Quebec")
qc_dat$day <- seq_len(nrow(qc_dat))
qc_dat$cases

ggplot(qc_dat, aes(date, cases)) +
  geom_point()

# Quebec public health officials announced Sunday that a computer error resulted
# in 1,317 missing positive COVID-19 cases between April 2-30.
# https://montrealgazette.com/news/latest-covid-19-statistics-for-quebec-include-1317-previously-unreported-cases-for-april/
outlier <- which(qc_dat$cases > 2000 & qc_dat$day < 65)
qc_dat$date[outlier]
qc_dat$cases[outlier]
qc_dat$cases[outlier] <- qc_dat$cases[outlier] - 1317

dates_with_missing <- qc_dat$date >= ymd("2020-04-02") & qc_dat$date <= ymd("2020-04-30")

qc_dat$cases_adjusted <- qc_dat$cases
qc_dat$cases_adjusted[dates_with_missing] <- qc_dat$cases[dates_with_missing] +
  1317 / length(dates_with_missing)
qc_dat$cases_adjusted <- round(qc_dat$cases_adjusted)
qc_dat$value <- qc_dat$cases_adjusted # for plotting function

ggplot(qc_dat, aes(date, cases)) +
  geom_line() +
  geom_line(aes(y = cases_adjusted), colour = "blue") +
  geom_vline(xintercept = ymd("2020-03-23"))

qc_dat$cases_adjusted
diff(qc_dat$date)

dat <- qc_dat

# Fit model -----------------------------------------------------------------

fit_file <- file.path("data-generated/QC-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = i0_PRIOR,
    start_decline_prior = c(log(get_google_start("Quebec", dat)), 0.1), # c(log(12), 0.2),
    end_decline_prior = c(log(get_google_end("Quebec", dat)), 0.1), # c(log(30), 0.2),
    f_seg = make_f_seg(dat),
    N_pop = 14.5e6,
    chains = CHAINS,
    iter = ITER
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)
make_traceplot(fit)

saveRDS(dat, file.path("data-generated/QC-dat.rds"))

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

# threshold <- get_thresh(fit)
# saveRDS(threshold,
#   here(this_folder, "data-generated/ON-threshold.rds"))

# # Quick plot:
# hist(fit$post$f_s[,1],
#   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
