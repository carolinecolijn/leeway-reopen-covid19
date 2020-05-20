# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

# Notes ---------------------------------------------------------------------

# Read and prepare data -----------------------------------------------------
library(ggplot2)
data <- readr::read_csv("C:/Users/rebec/Documents/COVID_BP/CoronaModelsBC/COVID19BE.csv")
ggplot(data, aes(DATE, CASES)) +
  geom_bar(stat="identity")

dat<-data%>% group_by(DATE) %>%
  summarize(cases = sum(CASES))
dat$day<- seq_len(nrow(dat))
names(dat)<-c("date","daily_cases","day")
# Pick a reasonable starting date:
dat <- dplyr::filter(dat, date >= lubridate::ymd("2020-03-03"))
ggplot(dat, aes(date, daily_cases)) +
  geom_bar(stat="identity")

#smoothing
dat1 <- dat %>%
  select(date, daily_cases = daily_cases, day) %>%
  mutate(daily_cases_smooth = zoo::rollmean(daily_cases, k = 3, fill = NA))

dat$daily_cases<-round(dat1$daily_cases_smooth)

saveRDS(dat, here("/selfIsolationModel/contact-ratios/","data-generated/BE-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 200)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit <- covidseir::fit_seir(
  daily_cases = dat$daily_cases,
  samp_frac_fixed = rep(0.2, nrow(dat)),
  i0_prior = c(log(1), 0.5),
  start_decline_prior = c(log(10), 0.2),
  end_decline_prior = c(log(50), 0.2),
  N_pop = 11476279,
  chains = 4,
  iter = 300
)

print(fit)
make_traceplot(fit)
saveRDS(fit, here("/selfIsolationModel/contact-ratios/", "data-generated/BE-fit.rds"))

# Check fit -----------------------------------------------------------------
dat$value <- dat$daily_cases

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
saveRDS(threshold, here("/selfIsolationModel/contact-ratios/", "data-generated/BE-threshold.rds"))

# Quick plot:
hist(fit$post$f_s[,1],
  main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
abline(v = threshold, col = "red", lwd = 2)
