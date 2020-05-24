source("selfIsolationModel/contact-ratios/model-prep.R")
RawData <- readr::read_csv(file.path(this_folder,"data-raw/EURO.csv"))
RawData$date <- lubridate::ymd(RawData$dateRep)
Raw <- dplyr::filter(RawData, countriesAndTerritories == "Japan")

dat <- Raw %>%
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

dat$value <- dat$daily_cases_smooth
ggplot(dat, aes(date, value)) +
  geom_point()
dat$value

# remove last NA:
dat <- dat[-nrow(dat), ]
dat$value

saveRDS(dat, file.path(this_folder, "data-generated/JP-dat.rds"))

fit_file <- file.path(this_folder, "data-generated/JP-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(i0_observed), 1),
    start_decline_prior = c(log(get_google_start("Japan", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Japan", dat)), 0.1),
    f_seg = make_f_seg(dat),
    # https://web.archive.org/web/20190606203315/http://www.stat.go.jp/english/data/jinsui/tsuki/index.html
    # https://www.destatis.de/EN/Themes/Society-Environment/Population/Current-Population/_node.html
    N_pop = 126e6,
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

# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
# proj_tidy <- covidseir::tidy_seir(proj)
#
# proj_tidy %>%
# 	covidseir::plot_projection(DenmarkData)
#
# proj_tidy %>%
# 	covidseir::plot_projection(DenmarkData) +
# 	scale_y_log10()
