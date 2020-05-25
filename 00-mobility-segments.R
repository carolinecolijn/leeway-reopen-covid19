library(ggplot2)
library(dplyr)
# library(reshape2)
library(segmented)
# library(cowplot)
# library(tidyr)
library(purrr)

# Analyze google mobility data
# dat is data to be fitted
# should have transit data & dates
get_sd_ramp_dates <- function(dat, region, return_plots = FALSE) {
  #---Google data------------------#
  dat <- dat %>%
    group_by(date) %>%
    summarise(transitMean = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE))
  dat$day <- seq(1, nrow(dat))
  #--- Segmented Linear Regression----#
  dat.lm <- lm(transitMean ~ day, dat)
  start <- which(dat$date == lubridate::ymd("2020-03-12")) # these usually work pretty well
  end <- which(dat$date == lubridate::ymd("2020-03-22"))
  dat.seg <- segmented(dat.lm, seg.Z = ~day, psi = list(day = c(start, end)))

  if (slope(dat.seg)$day["slope3", "Est."] < -0.1) { # Japan
    end <- which(dat$date == lubridate::ymd("2020-04-28"))
    dat.seg <- segmented(dat.lm, seg.Z = ~day, psi = list(day = c(start, end)))
  }

  #---Plot------------------#
  dat.fitted <- fitted(dat.seg)
  dat.model <- data.frame(date = dat$date, change = dat.fitted)

  plt <- ggplot(dat) +
    ggtitle(region) +
    geom_line(data = dat, aes(x = date, y = transitMean), alpha = 0.4) +
    geom_line(data = dat.model, aes(x = date, y = change)) +
    theme_light()

  # ggsave(here::here(paste0("plots/", region, "_plot.pdf")),
  #     plot = plt,
  #    width = 8,
  #    height = 10)

  if (return_plots) {
    plt
  } else {
    data.frame(
      startDate = as.Date(dat.seg$psi[[1, 2]], origin = min(dat$date)),
      endDate = as.Date(dat.seg$psi[[2, 2]], origin = min(dat$date)),
      f2_slope = round(slope(dat.seg)$day["slope3", "Est."], 3)
    )
  }
}

file1 <- "selfIsolationModel/contact-ratios/data-generated/google-data.rds"
file2 <- "selfIsolationModel/contact-ratios/data-generated/google-data-select.rds"
if (!file.exists(file2)) {
  g <- readr::read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
  saveRDS(g, file =  file)
  g$date <- lubridate::ymd(g$date)
  regions <- c("British Columbia", "North Carolina", "New York", "France", "Spain", "Australia", "Michigan", "Sweden", "Denmark", "Germany", "Alberta", "Quebec", "Washington", "New Zealand", "Florida", "Ontario", "Belgium", "United Kingdom", "California", "Japan")
  g <- dplyr::filter(g, country_region %in% regions | sub_region_1 %in% regions)
  g <- g %>% dplyr::mutate(region = ifelse(country_region %in% regions, country_region, sub_region_1))
  g <- dplyr::select(g, region, transit_stations_percent_change_from_baseline, date)
  saveRDS(g, file = file2)
} else {
  g <- readRDS(file2)
}

#----PLOT & DATES----------#
g_dates <- g %>%
  group_by(region) %>%
  tidyr::nest() %>%
  mutate(rampDates = map(data, get_sd_ramp_dates, region = region)) %>%
  select(-data) %>%
  tidyr::unnest(c(rampDates))
print(g_dates)

g_plots <- g %>%
  group_by(region) %>%
  group_split() %>%
  purrr::map(~ get_sd_ramp_dates(dat = .x, region = unique(.x$region), return_plots = TRUE))

plots <- cowplot::plot_grid(plotlist = g_plots)
ggsave("selfIsolationModel/contact-ratios/figs/google-plots.pdf", width = 10, height = 9)
ggsave("selfIsolationModel/contact-ratios/figs/google-plots.png", width = 10, height = 9)

g_dates %>%
  rename(start_date = startDate, end_date = endDate) %>%
  readr::write_csv("selfIsolationModel/contact-ratios/data-generated/start-end-google.csv")
