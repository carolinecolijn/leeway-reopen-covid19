source("analysis/model-prep.R")

# timeline:
# Jan 26: first case
# Mar 12: ban mass gatherings
# Mar 13: schools closed
# Mar 15: Bars etc. closed
# Mar 17-24 (?): county stay-at-home orders? Shelter-in-place? seems a bit all over the place
# ca_dat <- filter(dat, state == "CA")
# ca_dat$day <- seq_len(nrow(ca_dat))
# ca_fit <- fit_seir(ca_dat$positiveIncrease, chains = 4, iter = 250,
#   samp_frac_fixed = rep(0.23, nrow(ca_dat)),
#   i0=8,
#   pars = c(
#     N = 39.51e6, D = 5, k1 = 1 / 5, k2 = 1, q = 0.05,
#     r = 0.1, ur = 0.02, f0 = 1.0, start_decline = 12, end_decline =24
#   ))

# d <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
# readr::write_csv(d, here::here("data-generated/us-data.csv"))
d <- readr::read_csv(file.path(this_folder, "data-raw/US.csv"))
d$date <- lubridate::ymd(d$date)

ca <- filter(d, state %in% "CA") %>%
  select(date, positiveIncrease, totalTestResultsIncrease, hospitalizedIncrease) %>%
  filter(date >= ymd("2020-03-05")) %>%
  rename(value = positiveIncrease, tests = totalTestResultsIncrease, hospitalized = hospitalizedIncrease) %>%
  arrange(date) %>%
  mutate(day = seq_len(n()))

ca
# View(ca)

plot(ca$day, ca$value, type = "o")
plot(ca$day, ca$tests, type = "o")
plot(ca$date, ca$value, type = "l")
# lines(ca$date, ca$hospitalized, col = "red")
lines(ca$date, ca$tests / 10, col = "blue")

(.s <- as.numeric(ymd("2020-03-12") - min(ca$date)))
(.e <- as.numeric(ymd("2020-03-24") - min(ca$date)))

# g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
# g1 <- filter(g, country_region == "United States")
# g1 <- filter(g, sub_region_1 == "California")
# ggplot(g1, aes(date, transit_stations_percent_change_from_baseline)) +
#   geom_point() +
#   geom_vline(xintercept = ymd("2020-03-12")) +
#   geom_vline(xintercept = ymd("2020-03-24"))

# (f_seg <- c(rep(0, 11), rep(1, nrow(new_york) - 11)))

ca$value
# stopifnot(unique(ca$value[38]) == 0)
# ca$value[38] <- NA
stopifnot(unique(ca$value[9]) == 0)
ca$value[9] <- NA
ca$value
dat <- ca

fit_file <- paste0(this_folder, "data-generated/CA-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    iter = ITER,
    chains = CHAINS,
    start_decline_prior = c(log(get_google_start("California", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("California", dat)), 0.1),
    f_seg = make_f_seg(dat),
    i0_prior = i0_PRIOR,
    N_pop = 39.51e6
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}
print(fit)
# p <- covidseir::project_seir(fit, iter = 1:50)
# covidseir::tidy_seir(p) %>%
#   covidseir::plot_projection(dat) +
#   scale_y_log10()

saveRDS(dat, file = file.path(this_folder, "data-generated/CA-dat.rds"))