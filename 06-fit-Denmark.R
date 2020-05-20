# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

# Read and prepare data -----------------------------------------------------
RawData <-  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
RawData$date <- lubridate::dmy(RawData$dateRep)
DenmarkRaw <- dplyr::filter(RawData, countriesAndTerritories == "Denmark")

DenmarkRaw <- DenmarkRaw[, c("date","cases")]

# arranging the data in descending order
DenmarkRaw$date <- rev(DenmarkRaw$date)
DenmarkRaw$cases <- rev(DenmarkRaw$cases)
DenmarkRaw$value <- DenmarkRaw$cases

DenmarkRaw <- DenmarkRaw[, c("date","value")]

ggplot(DenmarkRaw, aes(date, value)) +	geom_point()

# Pick a reasonable starting date:
DenmarkData <- dplyr::filter(DenmarkRaw, date >= lubridate::ymd("2020-03-4"))
DenmarkData$day <- seq_len(nrow(DenmarkData))

ggplot(DenmarkData, aes(date, value)) +
	geom_point()
# View(DenmarkData)
saveRDS(DenmarkData, here(this_folder, "data-generated/Denmark-dat.rds"))

# Fit model -----------------------------------------------------------------

# Example of visualizing a prior:
# x <- seq(0, 10, length.out = 20)
# plot(x, dlnorm(x, log(1), 0.5), type = "l", xaxs = "i", yaxs = "i")

fit <- covidseir::fit_seir(
	daily_cases = DenmarkData$value,
	samp_frac_fixed = rep(0.2, nrow(DenmarkData)),
	i0_prior = c(log(1), 0.5),
	start_decline_prior = c(log(10), 0.2),
	end_decline_prior = c(log(45), 0.2),
	N_pop = 5824857,
	chains = 4,
	iter = 250
)

print(fit)
make_traceplot(fit)
saveRDS(fit, here(this_folder, "data-generated/Denmark-fit.rds"))

# Check fit -----------------------------------------------------------------

proj <- covidseir::project_seir(fit, iter = 1:50, forecast_days = 30)
proj_tidy <- covidseir::tidy_seir(proj)

proj_tidy %>%
	covidseir::plot_projection(DenmarkData)

proj_tidy %>%
	covidseir::plot_projection(DenmarkData) +
	scale_y_log10()

# Calculate threshold for increase ------------------------------------------

# Need to pick reasonable f(s) values for a reasonable time span
# such that fitting a linear regression makes sense.
# Make sure the plot that comes out of this is linear:
threshold <- get_thresh(fit, iter = 1:50,forecast_days = 30, fs = seq(0.1, 0.7, length.out = 5))
round(threshold, 2)
saveRDS(threshold,  here(this_folder, "data-generated/Denmark-threshold.rds"))

# Quick plot:
hist(fit$post$f_s[,1],
		 main = "", xlab = "Estimated fraction of normal contacts", breaks = 20)
abline(v = threshold, col = "red", lwd = 2)

