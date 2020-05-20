# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

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
# Suggest 10-24 March for ramping down. Google mobility data suggest
#  2020-03-09 to 2020-03-29.

# Notes:
#   - March 27: Detroit declared a hot spot, 20% of total cases, 25% of deaths
#   - May 18: any Detroit resident can get tested, elsewhere testing open to all
#       healthcare workers and anyone with mild symptoms
#   - May 18: 773 new cases (an uptick), but 513 due to enhanced testing in MI
#       Dept. of Corrections facilities

# population Michigan: 9.986857e6 (give or take)

# Ontario:
# Physical distancing timeline:
#   - March 12: recommend cancelling large mass gatherings of over 1,000
#   - March 13: recommending all gatherings over 250 be cancelled
#   - March 14--24: Ramp-up in measures
#   - March 25: closed all non-essential workplaces
#   - March 30: public and private parks, sport fields, beaches,playgrounds,
#     dog parks closed urging to stay home unless essential, particularly
#     70+ and immune compromised
# Testing:
#   - April 3: ended test backlog, will prioritize LTC residents, workers
#   - April 11: list of symptoms and testing expanded; daily tests processed
#     expected to double
#   - April 9: testing expanded to all health workers, first responders with
#     symptoms, new residents, and workers at LTC
#   - maybe testing backlog ended around April 1?
# population Ontario: approx. 14.5e6

# Read and prepare data -----------------------------------------------------
# Minimum is: a column with cases named "value", a column named "date", a column named "day".
dat_ON <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")
dat <- readr::read_csv("https://covidtracking.com/api/v1/states/daily.csv")
dat$date <- lubridate::ymd(dat$date)
dat <- dplyr::rename(dat, value = "positiveIncrease")
dat <- dplyr::filter(dat, state == "MI") %>%
  dplyr::arrange(date)
 # column positive is cumulative cases, column positiveIncrease is daily cases
 # except 2020-03-01 is NA for MI (9 cumulative cases on that date, but with 9
 # more the next day just including all 9 as being on 2020-03-01, since quite
 # feasible and can then start on 2020-03-01). Cumulative count on 2020-05-19
 # agrees with @MichiganHHS Tweet.
dat[dat$date == "2020-03-01", "value"] = dat[dat$date == "2020-03-01",
                                             "positive"]
dat <- dplyr::select(dat, date, value)

# View(dat)

ggplot(dat, aes(date, value)) +
  geom_point() +
  geom_line()

# Doesn't quite seem to agree with their website figure:
# https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html
# despite agreement in cumulative count (above).
# Pick a reasonable starting date:
#  Default seems reasonable, adding in the 9 cases for that day.
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-01"))
dat$day <- seq_len(nrow(dat))


# ON: Try redistributing Apr 01 bump:
# dat$adjust_cases <- dat$cases
# excess <- (dat[32,]$cases - dat[33,]$cases)/5
# dat[32,]$adjust_cases <- dat[33,]$cases
# dat[27:31,]$adjust_cases <- round(dat[27:31,]$cases + excess)
# dat$value <- dat$adjust_cases # for plotting function
# ggplot(dat, aes(day, value)) +
#   geom_point()

saveRDS(dat, here(this_folder, "data-generated/MI-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit <- covidseir::fit_seir(
  daily_cases = dat$value,
  samp_frac_fixed = rep(0.2, nrow(dat)),
  i0_prior = c(log(1), 0.5),
  start_decline_prior = c(log(12), 0.1),
  end_decline_prior = c(log(30), 0.1),
  N_pop = 9.986857e6,
  chains = 4,
  iter = 250
)

print(fit)
make_traceplot(fit)
saveRDS(fit, here(this_folder, "data-generated/MI-fit.rds"))

# Check fit -----------------------------------------------------------------

proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
proj_tidy <- covidseir::tidy_seir(proj)

proj_tidy %>%
  covidseir::plot_projection(dat)

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
saveRDS(threshold, here(this_folder, "data-generated/MI-threshold.rds"))

# Quick plot:
hist(fit$post$f_s[,1],
  main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
abline(v = threshold, col = "red", lwd = 2)
