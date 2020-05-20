# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

# Notes ---------------------------------------------------------------------
# https://www.government.se/articles/2020/05/about-covid-19--for-older-people-people-with-health-conditions-and-health-care-and-social-services-staff/
# incr testing May 11


# Read and prepare data -----------------------------------------------------

dat <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
dat$dateRep <- lubridate::dmy(dat$dateRep)
dat <- dplyr::filter(dat, countriesAndTerritories == "Sweden")
dat <- dat %>% rename(date = dateRep) %>% arrange(date)

# View(dat)
ggplot(dat, aes(date, cases)) +
  geom_point()

# moving average, doesn't really work
#library(zoo)
#dat <- dat %>% mutate(ncases = round(rollmean(cases,7,align='right',fill=NA)))


# Pick a reasonable starting date:
origin <- lubridate::ymd("2020-03-01")
dat <- dplyr::filter(dat, date >= origin)
dat$day <- seq_len(nrow(dat))


# View(dat)
ggplot(dat, aes(day, ncases)) +
  geom_line(color="blue") + geom_point(aes(day, cases), color="red")

saveRDS(dat, here(this_folder, "data-generated/SWE-dat.rds"))

# Fit model -----------------------------------------------------------------

# Google dates DO NOT WORK AT ALL
# get start and end_decline dates from google data
#google_dates <- dplyr::filter(readr::read_csv("../google_data/start-end-google.csv"), region=="Sweden")
#s <- as.numeric(google_dates$start_date - origin, units="days")
#e <- as.numeric(google_dates$end_date - origin, units="days")


fit <- covidseir::fit_seir(
  daily_cases = dat$cases, #fit to smoothed cases
  samp_frac_fixed = rep(0.2, nrow(dat)),
  i0_prior = c(log(1), 0.5),
  start_decline_prior = c(log(6), 0.2),
  end_decline_prior = c(log(27), 0.2),
  N_pop = 10343403,
  chains = 4,
  iter = 250,
  pars = c(D = 5, k1 = 1/5, k2 = 1,
    q = 0.05, r = 0.1, ur = covidseir:::getu(0.6, r = 0.1), f0 = 1)
)

print(fit)
make_traceplot(fit)
saveRDS(fit, here(this_folder, "data-generated/SWE-fit.rds"))

# Check fit -----------------------------------------------------------------
dat$value <- dat$cases #plot using actual cases

proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
proj_tidy <- covidseir::tidy_seir(proj)

proj_tidy %>%
  covidseir::plot_projection(dat) #+ geom_vline(data = data.frame(end=fit$post$end_decline), aes(xintercept=end))

proj_tidy %>%
  covidseir::plot_projection(dat) +
  scale_y_log10()

# Calculate threshold for increase ------------------------------------------

# Need to pick reasonable f(s) values for a reasonable time span
# such that fitting a linear regression makes sense.
# Make sure the plot that comes out of this is linear:
threshold <- get_thresh(fit, iter = 1:50,
  forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
round(threshold, 2)
saveRDS(threshold, here(this_folder, "data-generated/SWE-threshold.rds"))

# Quick plot:
hist(fit$post$f_s[,1],
  main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
abline(v = threshold, col = "red", lwd = 2)
