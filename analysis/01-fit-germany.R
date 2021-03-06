source("analysis/model-prep.R")
RawData <- readr::read_csv(file.path(this_folder,"data-raw/EURO.csv"))
RawData$date <- lubridate::dmy(RawData$dateRep)
GermanyRaw <- dplyr::filter(RawData, countriesAndTerritories == "Germany")

dat <- GermanyRaw %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, cases) %>%
  rename(value = cases)

ggplot(dat, aes(date, value)) +
  geom_point()

dat <- dat %>%
  mutate(daily_cases_smooth = round(zoo::rollmean(value, k = 3, fill = NA)))

ggplot(dat, aes(date, value)) +
  geom_point() +
  geom_line(aes(y = daily_cases_smooth))

# Pick a reasonable starting date:
origin <- lubridate::ymd("2020-03-01")

dat_early <- dplyr::filter(dat, date < ymd("2020-02-01"))
i0_observed <- sum(dat_early$value)
i0_observed

dat <- dplyr::filter(dat, date >= origin)
dat$day <- seq_len(nrow(dat))

# dat$value <- dat$daily_cases_smooth
ggplot(dat, aes(date, value)) +
  geom_point()
dat$value

ggplot(dat, aes(date, value)) +
  geom_point()

saveRDS(dat, file.path("data-generated/DE-dat.rds"))

dat <- dplyr::filter(dat, date <= ymd("2020-06-07"))

ggplot(dat, aes(date, value)) +
  geom_point()

fit_file <- file.path("data-generated/DE-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(i0_observed), 1),
    start_decline_prior = c(log(get_google_start("Germany", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Germany", dat)), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 83e6,
    # https://www.destatis.de/EN/Themes/Society-Environment/Population/Current-Population/_node.html
    chains = CHAINS,
    iter = ITER,
    control = list(adapt_delta = 0.9),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)
make_traceplot(fit)

# Check fit -----------------------------------------------------------------

# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
# proj_tidy <- covidseir::tidy_seir(proj)
#
# proj_tidy %>%
# 	covidseir::plot_projection(dat)
#
# proj_tidy %>%
# 	covidseir::plot_projection(dat) +
# 	scale_y_log10()
