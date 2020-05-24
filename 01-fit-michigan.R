# Load some packages, functions, and global variables:
source("selfIsolationModel/contact-ratios/model-prep.R")

# Notes ---------------------------------------------------------------------
# Michigan, from Wikipedia and checking some links to Executive Orders (press
# releases from the government don't seem available now for March and April):
# Physical distancing timeline:
#   - March 10: State of Emergency declared, schools closed,
#   - March 16: bars, restaurants, entertainment venues, other businesses
#        ordered closed
#   - March 17: banned gatherings >50
#   - March 24: stay-at-home order, discontinuing all non-essential travel,
#      business servies and operations
# Suggest 10-24 March for ramping down (values came close to these when using
# the Ontario prior). Google mobility data suggest 2020-03-09 to 2020-03-29 so
# using those for priors.

# Notes:
#   - March 27: Detroit declared a hot spot, 20% of total cases, 25% of deaths
#   - May 18: any Detroit resident can get tested, elsewhere testing open to all
#       healthcare workers and anyone with mild symptoms
#   - May 18: 773 new cases (an uptick), but 513 due to enhanced testing in MI
#       Dept. of Corrections facilities
#   - Data are noisy, so doing three-day running average and rounding to nearest integer.
# population Michigan: 9.986857e6 (give or take)

# Read and prepare data -----------------------------------------------------
# Minimum is: a column with cases named "value", a column named "date", a column named "day".
dat <- readr::read_csv(paste0(this_folder, "data-raw/US.csv"))
dat$date <- lubridate::ymd(dat$date)
dat <- dplyr::rename(dat, daily_cases = "positiveIncrease")
dat <- dplyr::filter(dat, state == "MI") %>%
  dplyr::arrange(date)
# column positive is cumulative cases, column positiveIncrease is daily cases
# except 2020-03-01 is NA for MI (9 cumulative cases on that date, but with 9
# more the next day just including all 9 as being on 2020-03-01, since quite
# feasible and can then start on 2020-03-01). Cumulative count on 2020-05-19
# agrees with @MichiganHHS Tweet.
dat[dat$date == "2020-03-01", "daily_cases"] <- dat[
  dat$date == "2020-03-01",
  "positive"
]
dat <- dplyr::select(dat, date, daily_cases, daily_tests = totalTestResultsIncrease)

# Increase in daily tests through, but low numbers on some days that looks like
#  reporting issues (i.e. low gets followed by high number)
# plot(dat$date, dat$daily_tests, type = "o", main = "Daily tests")

# Data are noisy, do three-day running average:
dat <- dat %>%
  dplyr::mutate(daily_cases_smooth = zoo::rollmean(daily_cases,
    k = 3,
    fill = NA
  ))
dat[1, "daily_cases_smooth"] <- mean(dat[1:2, ]$daily_cases) # Use two-day average
#  for day 1 and final day:
dat[nrow(dat), "daily_cases_smooth"] <- mean(dat[(nrow(dat) - 1):nrow(dat), ]$daily_cases)
dat$value <- round(dat$daily_cases_smooth) # Use rounded value for fitting and plotting

# View(dat)

ggplot(dat, aes(date, value)) +
  geom_point() +
  geom_line()

# Doesn't quite seem to agree with their website figure:
# https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html
# despite agreement in cumulative count (above).
# Pick a reasonable starting date:
#  Default seems reasonable (and is all the data):
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
dat$day <- seq_len(nrow(dat))

saveRDS(dat, file.path(this_folder, "data-generated/MI-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit_file <- file.path(this_folder, "data-generated/MI-fit.rds")

if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = i0_PRIOR,
    start_decline_prior = c(log(get_google_start("Michigan", dat)), 0.1),  # c(log(9), 0.1),
    end_decline_prior = c(log(get_google_end("Michigan", dat)), 0.1),  # c(log(29), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 9.986857e6,
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

# proj <- covidseir::project_seir(fit, iter = 1:3, forecast_days = 20)
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
#   here(this_folder, "data-generated/MI-threshold.rds"))

# Quick plot:
# hist(fit$post$f_s[,1],
#     main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)

# Keeping track of results (don't run) ---------------------------

# MI with Ontario defaults except data and population size
# Inference for Stan model: seir.
# 4 chains, each with iter=250; warmup=125; thin=1;
# post-warmup draws per chain=125, total post-warmup draws=500.
#
#                mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# R0             4.94    0.02 0.20  4.59  4.78  4.92  5.09  5.37    93 1.02
# i0             2.10    0.07 0.67  0.99  1.57  2.02  2.54  3.54    93 1.02
# f_s[1]         0.32    0.00 0.02  0.29  0.31  0.32  0.33  0.35   137 1.01
# start_decline 10.39    0.05 0.91  8.69  9.72 10.39 11.00 12.26   330 1.00
# end_decline   23.61    0.10 1.61 20.73 22.40 23.65 24.65 26.88   258 1.01
# phi[1]         9.36    0.10 1.64  6.57  8.22  9.23 10.41 12.85   250 1.00
#
# # MI with priors on start_decline and end_decline based on Google mobility data:
#                mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# R0             5.15    0.03 0.25  4.68  4.97  5.15  5.32  5.61    95 1.04
# i0             1.63    0.07 0.69  0.72  1.14  1.51  1.97  3.27   101 1.03
# f_s[1]         0.31    0.00 0.02  0.27  0.29  0.30  0.32  0.34   127 1.03
# start_decline  8.33    0.04 0.78  6.85  7.77  8.32  8.84  9.82   337 1.01
# end_decline   24.31    0.10 1.44 21.41 23.34 24.31 25.39 26.97   221 1.03
# phi[1]         9.86    0.13 1.58  6.96  8.89  9.74 10.83 13.45   157 1.03
#
# # MI with then smoothing data using running three-day mean, big increase in phi:
#                mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# R0             5.14    0.02 0.22  4.74  4.98  5.15  5.30  5.59   128 1.03
# i0             1.71    0.05 0.60  0.78  1.27  1.61  2.06  3.07   134 1.03
# f_s[1]         0.31    0.00 0.01  0.28  0.30  0.31  0.32  0.34   156 1.02
# start_decline  8.07    0.04 0.69  6.79  7.61  8.01  8.49  9.41   388 1.00
# end_decline   23.77    0.07 1.23 21.45 22.94 23.68 24.56 26.39   309 1.02
# phi[1]        19.89    0.24 3.43 13.31 17.72 19.86 22.11 27.06   204 1.03
#
# # MI Update package to also estimate the fraction social distancing, e:
#                mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# R0             5.15    0.02 0.23  4.73  5.01  5.14  5.28  5.63   141 1.01
# i0             1.70    0.06 0.65  0.75  1.28  1.60  2.00  3.11   113 1.01
# e              0.82    0.01 0.07  0.70  0.77  0.81  0.87  0.95   129 1.00
# f_s[1]         0.28    0.01 0.08  0.11  0.24  0.29  0.34  0.41   138 1.00
# start_decline  8.06    0.03 0.69  6.77  7.57  8.05  8.50  9.48   412 1.00
# end_decline   23.88    0.08 1.31 21.38 23.03 23.88 24.79 26.30   289 1.00
# phi[1]        20.10    0.23 3.66 12.73 17.78 19.79 22.48 27.29   249 1.01
#
