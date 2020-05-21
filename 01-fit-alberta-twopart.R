# Load some packages, functions, and global variables:
source("selfIsolationModel/contact-ratios/model-prep.R")

# Notes ---------------------------------------------------------------------

# Runs and saves two versions, before and after Cargill

# Read and prepare data -----------------------------------------------------

# dat <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")
dat <- readr::read_csv(file.path(this_folder, "data-raw/CAN.csv"))
dat$date <- lubridate::dmy(dat$date_report)
dat <- dplyr::filter(dat, province == "Alberta")
# View(dat)
ggplot(dat, aes(date, cases)) +
  geom_point()
# Pick a reasonable starting date:
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01")) # COULD BE A FEW DAYS LATER
dat$day <- seq_len(nrow(dat))
ggplot(dat, aes(date, cases)) +
  geom_point()

# Try removing at least some of Cargill, for a better picture of community transmission
# Cargill info: Apr 13: 38 cases reported Apr 20: 484 cases in High River - 360 outbreak.
# Apr 22: Deena Henshaw says 440 outbk cases (presumably in total)
# Apr 20 Plant closed for 2 weeks
# May 4 plant re-opens
# Total of 1560 cases, 949 confirmed.
# CONCLUSION: subtract up to 400 between Apr 13, Apr 22; subtract another 500 between Apr 22 and May 1.
dat1 <- filter(dat, date <= ymd("2020-04-13")) # Alberta, pre-Cargill. Fit with parameters below

dat2 <- filter(dat, date > ymd("2020-04-13")) # Cargill only. Will have to adjust N and starting time
dat2$day <- seq_len(nrow(dat2))
# as well as sampling compared to fit below

dat1$value <- dat1$cases # for plotting function
dat2$value <- dat2$cases # for plotting function

ggplot(dat2, aes(day, cases)) +
  geom_point()

dat1$daily_cases <- dat1$cases
dat2$daily_cases <- dat2$cases

saveRDS(dat1, file.path(this_folder, "data-generated/AB1-dat.rds"))
saveRDS(dat2, file.path(this_folder, "data-generated/AB2-dat.rds"))

absampling1 <- rep(0.2, nrow(dat1)) # there was a testing breakpoint at about apr14 anyway
absampling2 <- rep(0.4, nrow(dat2))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

# Fit dat1:
fit_file1 <- file.path(this_folder, "data-generated/AB1-fit.rds")
if (!file.exists(fit_file1)) {
  fit1 <- covidseir::fit_seir(
    daily_cases = dat1$daily_cases,
    samp_frac_fixed = absampling1,
    i0_prior = c(log(1), 0.5),
    f_prior = c(0.2, 0.1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    N_pop = 4.4e6, # population of AB
    chains = CHAINS,
    iter = ITER
  )
  saveRDS(fit1, fit_file1)
} else {
  fit1 <- readRDS(fit_file1)
}

print(fit1)
make_traceplot(fit1)

# Fit dat2:

fit_file2 <- file.path(this_folder, "data-generated/AB2-fit.rds")
if (!file.exists(fit_file2)) {
  fit2 <- covidseir::fit_seir(
    daily_cases = dat2$daily_cases,
    samp_frac_fixed = absampling2,
    i0_prior = c(log(10), 0.5),
    start_decline_prior = c(log(6), 0.1),
    end_decline_prior = c(log(7), 0.1),
    N_pop = 15000, # population of Cargill + families and
    # friends. A guess.
    chains = CHAINS,
    iter = ITER
  )
  saveRDS(fit2, fit_file2)
} else {
  fit2 <- readRDS(fit_file2)
}

print(fit2)
make_traceplot(fit2)

# Check fit -----------------------------------------------------------------
# proj1 <- covidseir::project_seir(fit1, iter = 1:50, forecast_days = 30)
# proj2 <- covidseir::project_seir(fit2, iter = 1:50, forecast_days = 30)


# proj_tidy1 <- covidseir::tidy_seir(proj1)
# proj_tidy2 <- covidseir::tidy_seir(proj2)

# proj_tidy1 %>%
#   covidseir::plot_projection(dat1)
#
# proj_tidy2 %>%
#   covidseir::plot_projection(dat2)
#
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
# saveRDS(threshold, paste0(this_folder, "data-generated/ON-threshold.rds"))
#
# # Quick plot:
# hist(fit$post$f_s[,1],
#   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
#
