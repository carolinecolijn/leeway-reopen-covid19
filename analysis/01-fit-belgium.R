# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# Notes ---------------------------------------------------------------------

# Read and prepare data -----------------------------------------------------
# data <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")
data <- readr::read_csv(file.path("data-raw/BE.csv"))
ggplot(data, aes(DATE, CASES)) +
  geom_bar(stat = "identity")

dat <- data %>%
  group_by(DATE) %>%
  summarize(cases = sum(CASES))
dat$day <- seq_len(nrow(dat))
names(dat) <- c("date", "daily_cases", "day")
# Pick a reasonable starting date:

i0_cases <- filter(dat, date < ymd("2020-03-03")) %>% pull(daily_cases) %>% sum()
dat <- dplyr::filter(dat, date >= ymd("2020-03-03"))
dat$day <- seq_len(nrow(dat))

ggplot(dat, aes(date, daily_cases)) +
  geom_bar(stat = "identity")

# smoothing
dat1 <- dat %>%
  select(date, daily_cases = daily_cases, day) %>%
  mutate(daily_cases_smooth = zoo::rollmean(daily_cases, k = 3, fill = NA))

# dat$daily_cases <- round(dat1$daily_cases_smooth)
dat$value <- dat$daily_cases

if (is.na(dat$daily_cases[nrow(dat)])) {
  dat <- dat[-nrow(dat), ]
}

saveRDS(dat, file.path("data-generated/BE-dat.rds"))

dat <- dplyr::filter(dat, date <= ymd("2020-06-07"))

ggplot(dat, aes(date, daily_cases)) +
  geom_point() + geom_line()

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit_file <- file.path("data-generated/BE-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$daily_cases,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(i0_cases), 1),
    start_decline_prior = c(log(get_google_start("Belgium", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Belgium", dat)), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 11.46e6,
    chains = CHAINS,
    iter = ITER,
    # fit_type = "optimizing"
    fit_type = "NUTS",
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
# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
# proj_tidy <- covidseir::tidy_seir(proj)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat)
#
# proj_tidy %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()

# Calculate threshold for increase ------------------------------------------
#
# # Need to pick reasonable f(s) values for a reasonable time span
# # such that fitting a linear regression makes sense.
# # Make sure the plot that comes out of this is linear:
# threshold <- get_thresh(fit, iter = 1:50,
#   forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
# round(threshold, 2)
# saveRDS(threshold, here(this_folder, "data-generated/BE-threshold.rds"))
#
# # Quick plot:
# hist(fit$post$f_s[,1],
#   main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
