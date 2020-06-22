library(ggplot2)
library(dplyr)
library(segmented)
library(purrr)

# Analyze Google mobility data

mobility_regions <- c("British Columbia", "New York", "Sweden", "Germany", "Quebec", "Washington", "New Zealand", "Ontario", "Belgium", "United Kingdom", "California", "Japan")

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
    # theme_light()
    ggsidekick::theme_sleek() +
    ylab("Mean transit value") +
    theme(axis.title.x = element_blank()) +
    ylim(-95, 15)

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

file2 <- "data-generated/google-data-select.rds"
if (!file.exists(file2)) {
  g <- readr::read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f"
  )
  saveRDS(g, file = file)
  g$date <- lubridate::ymd(g$date)

  g <- dplyr::filter(g, country_region %in% mobility_regions | sub_region_1 %in% mobility_regions, date <= lubridate::ymd("2020-05-16"))
  g <- g %>% dplyr::mutate(region = ifelse(country_region %in% mobility_regions, country_region, sub_region_1))
  g <- dplyr::select(g, region, transit_stations_percent_change_from_baseline, date)
  saveRDS(g, file = file2)
} else {
  g <- readRDS(file2)
}

#----PLOT & DATES----------#
g_dates <- g %>%
  filter(region %in% mobility_regions) %>%
  group_by(region) %>%
  tidyr::nest() %>%
  mutate(rampDates = map(data, get_sd_ramp_dates, region = region)) %>%
  select(-data) %>%
  tidyr::unnest(c(rampDates))
print(g_dates)

g_plots <- g %>%
  filter(region %in% mobility_regions) %>%
  group_by(region) %>%
  group_split() %>%
  purrr::map(~ get_sd_ramp_dates(dat = .x, region = unique(.x$region), return_plots = TRUE))

plots <- cowplot::plot_grid(plotlist = g_plots)
ggsave("figs/google-plots.pdf", width = 9, height = 5.5)
ggsave("figs/google-plots.png", width = 8, height = 7)

g_dates %>%
  rename(start_date = startDate, end_date = endDate) %>%
  readr::write_csv("data-generated/start-end-google.csv")
