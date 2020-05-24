# Load some packages, functions, and global variables:
source("selfIsolationModel/contact-ratios/model-prep.R")

# Read and prepare data -----------------------------------------------------
#RawData <-  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
RawData <- readr::read_csv(file.path(this_folder,"data-raw/EURO.csv"))   # note:
                                        # seems to have more than just Europe
RawData$date <- lubridate::ymd(RawData$dateRep)
DenmarkRaw <- dplyr::filter(RawData, countriesAndTerritories == "Denmark")

dat <- DenmarkRaw %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, cases) %>%
  rename(value = cases)

ggplot(dat, aes(date, value)) +
  geom_point()

# Pick a reasonable starting date:
origin <- lubridate::ymd("2020-03-01")   # 2 cases before then
dat <- dplyr::filter(dat, date >= origin)
dat$day <- seq_len(nrow(dat))

ggplot(dat, aes(date, value)) +
	geom_point()
# View(DenmarkData)
saveRDS(dat, file.path(this_folder, "data-generated/DK-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 20)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit_file <- file.path(this_folder, "data-generated/DK-fit.rds")
if (!file.exists(fit_file)) {
  fit <- covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = i0_PRIOR,
    start_decline_prior = c(log(get_google_start("Denmark", dat)), 0.1),  # = 7, was c(log(10), 0.2),
    end_decline_prior = c(log(get_google_end("Denmark", dat)), 0.1),  # =
                                                                     # 17, was c(log(45), 0.2),
    f_seg = make_f_seg(dat),
    N_pop = 5824857,
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

# Calculate threshold for increase ------------------------------------------

# Need to pick reasonable f(s) values for a reasonable time span
# such that fitting a linear regression makes sense.
# Make sure the plot that comes out of this is linear:
# threshold <- get_thresh(fit, iter = 1:50,forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
# round(threshold, 2)
# saveRDS(threshold,  here(this_folder, "data-generated/Denmark-threshold.rds"))

# Quick plot:
# hist(fit$post$f_s[,1],
# 		 main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
# abline(v = threshold, col = "red", lwd = 2)
