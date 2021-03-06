# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
# readr::write_csv(d, here::here("data-generated/us-data.csv"))
d <- readr::read_csv(file.path(this_folder, "data-raw/US.csv"))
d$date <- lubridate::ymd(d$date)

florida <- dplyr::filter(d, state %in% "FL") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-05")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

florida
# View(florida)

plot(florida$day, florida$value, type = "o")
plot(florida$day, florida$tests, type = "o")
plot(florida$date, florida$value, type = "l")
lines(florida$date, florida$hospitalized, col = "red")
lines(florida$date, florida$tests/10, col = "blue")

# g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
# g1 <- filter(g, country_region == "United States")
# g1 <- filter(g, sub_region_1 == "Florida")
# ggplot(g1, aes(date, transit_stations_percent_change_from_baseline)) +
#   geom_point() +
#   geom_vline(xintercept = ymd("2020-03-16")) +
#   geom_vline(xintercept = ymd("2020-03-28"))


# get_days_since <- function(until, since) {
#   abs(as.numeric(difftime(until, since, units = "days")))
# }
# (start_decline <- get_days_since(ymd("2020-03-16"), min(florida$date)))
# (end_decline <- get_days_since(ymd("2020-03-28"), min(florida$date)))
# (f_seg <- c(rep(0, start_decline), rep(1, nrow(florida) - start_decline)))

# (samp_frac_fixed <- rep(0.2, nrow(florida)))

florida$value

dat <- florida
saveRDS(florida, file = file.path(this_folder, "data-generated/FL-dat.rds"))
dat <- dplyr::filter(dat, date <= ymd("2020-06-07"))

fit_file <- file.path(this_folder, "data-generated/FL-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    iter = ITER,
    chains = CHAINS,
    start_decline_prior = c(log(get_google_start("Florida", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Florida", dat)), 0.1),
    f_seg = make_f_seg(dat),
    i0_prior = i0_PRIOR,
    N_pop = 21.48e6,
    fit_type = "optimizing",
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

fit
p <- covidseir::project_seir(fit, iter = 1:50)
covidseir::tidy_seir(p) %>%
  covidseir::plot_projection(florida)# +
# scale_y_log10()


# model <- rstan::stan_model("analysis/seeiqr.stan")
# source("analysis/fit_seeiqr.R")
# source("analysis/make_projection_plot.R")
#
# fl_fit <- fit_seeiqr(florida$value,
#   seeiqr_model = model,
#   forecast_days = 30,
#   R0_prior = c(log(3), 0.2),
#   chains = 8,
#   iter = 500,
#   i0 = 1,
#   sampled_fraction1 = 0.25,
#   sampled_fraction2 = 0.25,
#   pars = c(N = 21.48e6, D = 5, k1 = 1/5, k2 = 1,
#     q = 0.05, r = 0.1, ur = 0.02, f1 = 1,
#     start_decline = 12, end_decline = 27))
# print(fl_fit$fit, pars = c("R0", "f2", "phi"))
#
# make_projection_plot(list(fl_fit), first_date = as.character(min(florida$date)))
