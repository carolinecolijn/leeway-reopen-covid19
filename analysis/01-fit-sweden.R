# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# Notes ---------------------------------------------------------------------
# https://www.government.se/articles/2020/05/about-covid-19--for-older-people-people-with-health-conditions-and-health-care-and-social-services-staff/
# incr testing May 11


# Read and prepare data -----------------------------------------------------

#dat <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
dat <- readr::read_csv(file.path(this_folder,"data-raw/EURO.csv"))
dat$dateRep <- lubridate::dmy(dat$dateRep)
dat <- dplyr::filter(dat, countriesAndTerritories == "Sweden")
dat <- dat %>% rename(date = dateRep) %>% arrange(date)

# View(dat)
ggplot(dat, aes(date, cases)) +
  geom_point()

dat <- filter(dat, day <= 95)
# moving average, doesn't really work
#library(zoo)
#dat <- dat %>% mutate(ncases = round(rollmean(cases,7,align='right',fill=NA)))


# Pick a reasonable starting date:
origin <- lubridate::ymd("2020-03-01")
dat <- dplyr::filter(dat, date >= origin)
dat$day <- seq_len(nrow(dat))

dat$value <- dat$cases

# View(dat)
ggplot(dat, aes(day, cases)) +
  geom_line(color="blue") + geom_point(aes(day, cases), color="red")

saveRDS(dat, file.path("data-generated/SE-dat.rds"))
dat <- dplyr::filter(dat, date <= ymd("2020-06-08"))

# Fit model -----------------------------------------------------------------

# Google dates DO NOT WORK AT ALL
# get start and end_decline dates from google data
#google_dates <- dplyr::filter(readr::read_csv("../google_data/start-end-google.csv"), region=="Sweden")
#s <- as.numeric(google_dates$start_date - origin, units="days")
#e <- as.numeric(google_dates$end_date - origin, units="days")

fit_file <- file.path("data-generated/SWE-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$cases, #fit to smoothed cases
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = i0_PRIOR,
    start_decline_prior = c(log(6), 0.2), # c(log(get_google_start("Sweden", dat)), 0.2),
    end_decline_prior = c(log(27), 0.2), # c(log(get_google_end("Sweden", dat)), 0.2),
    f_seg = make_f_seg(dat),
    N_pop = 10343403,
    chains = CHAINS,
    iter = ITER,
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)
make_traceplot(fit)
saveRDS(fit, file.path("data-generated/SE-fit.rds"))

# Check fit -----------------------------------------------------------------
# dat$value <- dat$cases #plot using actual cases

# proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
# proj_tidy <- covidseir::tidy_seir(proj)

# proj_tidy %>%
#   covidseir::plot_projection(dat) #+ geom_vline(data = data.frame(end=fit$post$end_decline), aes(xintercept=end))

# proj_tidy %>%
#  covidseir::plot_projection(dat) +
#  scale_y_log10()

# Calculate threshold for increase ------------------------------------------

# Need to pick reasonable f(s) values for a reasonable time span
# such that fitting a linear regression makes sense.
# Make sure the plot that comes out of this is linear:
#threshold <- get_thresh(fit, iter = 1:50,
#  forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
#round(threshold, 2)
#saveRDS(threshold, file.path("data-generated/SWE-threshold.rds"))

# Quick plot:
#hist(fit$post$f_s[,1],
#  main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
#abline(v = threshold, col = "red", lwd = 2)
