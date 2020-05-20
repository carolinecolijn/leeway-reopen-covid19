# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

# Notes ---------------------------------------------------------------------

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

dat <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")
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
#Cargill info: Apr 13: 38 cases reported Apr 20: 484 cases in High River - 360 outbreak. 
#Apr 22: Deena Henshaw says 440 outbk cases (presumably in total) 
#Apr 20 Plant closed for 2 weeks 
#May 4 plant re-opens
#Total of 1560 cases, 949 confirmed. 
#CONCLUSION: subtract up to 400 between Apr 13, Apr 22; subtract another 500 between Apr 22 and May 1. 
cargill1= 36 # subtract these from april 14. maybe 2 of the 38 were index cases. who knows. someone must have been
cargill2=400  # subtract these from apr 15-22 uniformly . length of that is 8 days
cargill3=450 # subtract these from apr23-may 1 uniformly. length of this is 9 days

dat$adjust_cases <- dat$cases # copy of cases 

i14=which(dat$date==ymd("2020-04-14"))
dat[i14,]$adjust_cases <- dat[i14,]$cases-cargill1 # subtract first bunch
dat[i14+(1:8),]$adjust_cases <- dat[i14+(1:8),]$cases - round((1/8)*cargill2)
dat[i14+8+(1:9),]$adjust_cases <- dat[i14+8+1:9,]$cases - round((1/9)*cargill3)

dat$value <- dat$adjust_cases # for plotting function
ggplot(dat, aes(day, adjust_cases)) +
  geom_point()
dat$daily_cases <- dat$adjust_cases

# saveRDS(dat, here(this_folder, "data-generated/AB-dat.rds"))
saveRDS(dat, "AB-dat.rds") # here() was not working; did not have data-generated; saved in contact-ratios/

absampling = rep(0.2, nrow(dat))
absampling[ which(dat$date==ymd("2020-03-23")):(i14-1)] = 0.25
absampling[ i14:nrow(dat)] = 0.4

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit <- covidseir::fit_seir(
  daily_cases = dat$daily_cases,
#  samp_frac_fixed = absampling,
samp_frac_fixed = rep(0.2, nrow(dat)),
  i0_prior = c(log(1), 0.5),
  start_decline_prior = c(log(40), 0.2), # OK - there will be no way to model these data with 
  # social distancing in March as the driver of changes in transmission so I am putting the control measures
  # after the Cargill outbreak in here insteasd/ 
  end_decline_prior = c(log(50), 0.2), # seems like BC? i don't know
  N_pop = 4.4e6,
  chains = 4,
  iter = 1000
)

print(fit)
make_traceplot(fit)
saveRDS(fit, here(this_folder, "data-generated/AB-fit.rds"))
saveRDS(fit, "selfIsolationModel/contact-ratios/AB-fit.rds")
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
saveRDS(threshold, here(this_folder, "data-generated/ON-threshold.rds"))

# Quick plot:
hist(fit$post$f_s[,1],
  main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
abline(v = threshold, col = "red", lwd = 2)
