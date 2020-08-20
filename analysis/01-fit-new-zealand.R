# Load some packages, functions, and global variables:
source("analysis/model-prep.R")

# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_New_Zealand#Requirements
# Ardern announced that, effective 01:00 on 16 March, all travellers arriving in or returning to New Zealand from outside of the country must self-isolate for 14 days.
# In addition, restrictions were placed on travel to the Pacific islands from New Zealand, barring travel to the region by those showing signs of coronavirus symptoms, as well as close contacts of coronavirus patients.

# On 16 March, Ardern called for a halt to public gatherings of more than 500 people and warned that the outbreak could lead to a recession greater than the 2008 global financial crisis.[223][224]

# On 19 March, the Government required the cancellation of indoor gatherings of more than 100 people.

# On 23 March, Ardern raised the alert level to three and announced the closure of all schools, beginning that day. She also announced that the alert level would rise to four at 11:59pm on 25 March, instituting a nationwide lockdown.

# g <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
# g <- filter(g, country_region == "New Zealand")
# ggplot(g, aes(date, transit_stations_percent_change_from_baseline)) +
#   geom_point() +
#   # geom_vline(xintercept = ymd("2020-03-16")) +
#   geom_vline(xintercept = ymd("2020-03-18")) +
#   # geom_vline(xintercept = ymd("2020-03-24")) +
#   geom_vline(xintercept = ymd("2020-03-27"))
                                        # to add 0's after final case
d <- readr::read_csv(file.path("data-raw", "NZ.csv"), skip = 3)
d <- rename(d, date = `Date notified of potential case`, overseas = `Overseas travel`) %>%
  select(date, overseas) %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarize(
    all_cases = n(),
    not_overseas_cases = sum(overseas == "No", na.rm = TRUE)
  )
d


tidyr::pivot_longer(d, -date) %>%
  ggplot(aes(date, value, colour = name)) +
  geom_line()

# get_days_since <- function(until, since) {
#   abs(as.numeric(difftime(until, since, units = "days")))
# }
# (start_decline <- get_days_since(ymd("2020-03-17"), min(florida$date)))
# (end_decline <- get_days_since(ymd("2020-04-01"), min(florida$date)))
# (f_seg <- c(rep(0, start_decline), rep(1, nrow(florida) - start_decline)))

nz <- filter(d, date >= ymd("2020-03-15"))
nz$all_cases
diff(nz$date)

latest_data <- nz$date[length(nz$date)]
# latest_data here ensures we get 0's since last reported case
nz <- left_join(tibble(date = seq(min(nz$date), latest_data, by = "1 day")), nz)
nz$all_cases
nz$not_overseas_cases
# nz$not_overseas_cases[is.na(nz$not_overseas_cases)] <- 0
# nz$not_overseas_cases
diff(nz$date)
nz <- nz %>%
  mutate(all_cases = ifelse(!is.na(all_cases), all_cases, 0)) %>%
  mutate(not_overseas_cases = ifelse(!is.na(not_overseas_cases), not_overseas_cases, 0))

nz$all_cases
nz$not_overseas_cases
diff(nz$date)

april1 <- as.numeric(ymd("2020-04-01") - min(nz$date))

# samp_frac_fixed <- c(rep(0.4, april1 - 1), rep(0.6, 9999))
samp_frac_fixed <- c(rep(0.4, nrow(nz)))

# samp_frac_fixed <- c(0.3, 0.3, 0.3, 0.3, 0.3, seq(0.3, 0.6, length.out = 12), rep(0.6, 100))


nz$day <- seq_len(nrow(nz))
nz$value <- nz$not_overseas_cases
saveRDS(nz, file = file.path("data-generated/NZ-dat.rds"))
nz <- dplyr::filter(nz, date <= ymd("2020-06-07"))

samp_frac_fixed <- samp_frac_fixed[1:nrow(nz)]
plot(samp_frac_fixed)

.s <- as.numeric(ymd("2020-03-18") - min(nz$date))
.e <- as.numeric(ymd("2020-03-26") - min(nz$date))
dat <- nz

# https://www.stats.govt.nz/topics/population # 4951500



fit_file <- file.path(this_folder, "data-generated/NZ-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = nz$not_overseas_cases,
    samp_frac_fixed = samp_frac_fixed, # rep(SAMP_FRAC, nrow(dat)),
    # f_ramp_rate = 0.5,
    start_decline_prior = c(log(.s), 0.2), # c(log(get_google_start("New Zealand", dat)), 0.2),
    end_decline_prior = c(log(.e), 0.2), # c(log(get_google_end("New Zealand", dat)), 0.2),
    f_seg = make_f_seg(dat),
    iter = ITER,
    chains = CHAINS,
    pars = c(D = 5, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = covidseir:::get_ur(0.9, ud = 0.1), f0 = 1),
    i0_prior = c(log(0.01), 1), # setting to 0.001 worked before
    delay_shape = 1.53, # estimated
    delay_scale = 7.828, # estimated
    N_pop = 4951500
  )
  saveRDS(fit, fit_file)
} else {
  fit <- readRDS(fit_file)
}

print(fit)

# nz$value <- nz$all_cases
# p <- covidseir::project_seir(fit, iter = 1:100, forecast_days = 0)
#
# covidseir::tidy_seir(p) %>%
#   covidseir::plot_projection(nz)

# source(here::here("analysis/plot_projection_w_inset.R"))
# plot_projection_w_inset(p, nz, obj = fit)


