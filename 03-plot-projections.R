source("selfIsolationModel/contact-ratios/model-prep.R")
library(purrr)

.names <- c("AB1", "AB2", "BC", "CA", "MI", "NY", "ON", "QC", "WA")

obj_files <- paste0(
  "selfIsolationModel/contact-ratios/data-generated/",
  .names, "-fit.rds"
)

fits <- map(obj_files, readRDS) %>% set_names(.names)
fits

dat_files <- paste0(
  "selfIsolationModel/contact-ratios/data-generated/",
  .names, "-dat.rds"
)
dat_files

observed_data <- map(dat_files, readRDS) %>%
  set_names(.names) %>%
  map(select, date, day, value)
observed_data

observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- .names[[.x]]
  temp
}) %>% set_names(.names)

observed_data_orig <- observed_data

observed_data$AB2$day <- max(observed_data$AB1$day) + observed_data$AB2$day
observed_data$AB <- bind_rows(observed_data$AB1, observed_data$AB2)
observed_data$AB1 <- NULL
observed_data$AB2 <- NULL
observed_data <- map(observed_data, mutate,
  region = ifelse(grepl("AB", region), "AB", region))

# Multiplicative projection:

PROJ <- 60
ITER_PROJ <- 1:200
projections_multi <- map(names(fits), function(.x) {
  print(.x)
  if (.x == "AB1") {
    day_total <- nrow(observed_data_orig[["AB1"]]) +
      nrow(observed_data_orig[["AB2"]])
    print(day_total)
    AB2_days <- nrow(observed_data_orig[["AB2"]])
    .forecast_days <- AB2_days + PROJ
    .f_fixed_start <- day_total + 1
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = .forecast_days,
      f_fixed_start = .f_fixed_start,
      f_multi = rep(1.2, PROJ)
    )
  } else { # if (.x %in% c("AB2", "BC", "ON", "QC")) {
    days <- length(observed_data_orig[[.x]]$day)
    print(days)
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(1.2, PROJ)
    )
  }
  # else {
  #   covidseir::project_seir(
  #     fits[[.x]],
  #     iter = ITER_PROJ,
  #     forecast_days = 0
  #   )
  # }
}) %>% set_names(.names)
# plan(sequential)

saveRDS(projections_multi,
  file = "selfIsolationModel/contact-ratios/data-generated/all-projections-multi.rds"
)
projections_multi <- readRDS("selfIsolationModel/contact-ratios/data-generated/all-projections-multi.rds")

# check:
# tidy_projections <- map(projections_multi, covidseir::tidy_seir, resample_y_rep = 0)
# tidy_projections <- tidy_projections %>% .[order(names(.))]
# observed_data <- observed_data %>% .[order(names(.))]
# observed_data_orig <- observed_data_orig %>% .[order(names(.))]
# plots <- map2(tidy_projections, observed_data_orig, function(x, y) {
#   covidseir::plot_projection(pred_dat = x, obs_dat = y) +
#     facet_null() +
#     ggtitle(unique(y$region))
# })
# cowplot::plot_grid(plotlist = plots)

# Join the 2 Alberta models:
ab1_look_up <- tibble(
  date = seq(
    min(observed_data_orig[["AB1"]]$date),
    min(observed_data_orig[["AB1"]]$date) + max(projections_multi$AB1$day),
    by = "1 day"
  ),
  day = seq(1, max(projections_multi$AB1$day) + 1)
)
p_ab1 <- left_join(
  projections_multi$AB1,
  ab1_look_up
)
ab2_look_up <- tibble(
  date = seq(
    min(observed_data_orig[["AB2"]]$date),
    min(observed_data_orig[["AB2"]]$date) + max(projections_multi$AB2$day),
    by = "1 day"
  ),
  day = seq(1, max(projections_multi$AB2$day) + 1)
)
p_ab2 <- left_join(
  projections_multi$AB2,
  ab2_look_up
)
p_ab <- bind_rows(p_ab1, p_ab2) %>%
  select(-day) %>%
  group_by(.iteration) %>%
  arrange(date) %>%
  mutate(data_type = 1)
p_ab <- left_join(p_ab, ab1_look_up) %>%
  select(-date)
projections_multi$AB <- p_ab
projections_multi$AB1 <- NULL
projections_multi$AB2 <- NULL

tidy_projections <- map(projections_multi, custom_tidy_seir, resample_y_rep = 100)

# order
tidy_projections <- tidy_projections %>% .[order(names(.))]
observed_data <- observed_data %>% .[order(names(.))]

names(tidy_projections)
names(observed_data)

plots <- map2(tidy_projections, observed_data, function(x, obs) {
  pred <- left_join(ab1_look_up, x, by = "day")
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region))
})

g <- cowplot::plot_grid(plotlist = plots[c("BC", "AB", "ON", "QC")])

ggsave("selfIsolationModel/contact-ratios/figs/projections.svg", width = 8, height = 6.5, plot = g)
ggsave("selfIsolationModel/contact-ratios/figs/projections.pdf", width = 8, height = 6.5, plot = g)
ggsave("selfIsolationModel/contact-ratios/figs/projections.png", width = 8, height = 6.5, plot = g)

# 1.4 -----------------------------------------------------------------

PROJ <- 60
ITER_PROJ <- 1:40
mults <- c(1.2, 1.4)
PROV <- "BC"
projections_select <- map(mults, function(.x) {
  cat(.x, "\n")
  days <- length(observed_data_orig[[PROV]]$day)
  covidseir::project_seir(
    fits[[PROV]],
    iter = ITER_PROJ,
    forecast_days = PROJ,
    f_fixed_start = days + 1,
    f_multi = rep(.x, PROJ)
  )
}) %>% set_names(as.character(mults))
tidy_projections <- map(projections_select,
  custom_tidy_seir,
  resample_y_rep = 150
)
plots <- map(tidy_projections, function(x) {
  pred <- left_join(ab1_look_up, x, by = "day")
  obs <- observed_data[[PROV]]
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region))
})
cowplot::plot_grid(plotlist = plots)

# Histograms ------------------------------------------------

library(future)
plan(multisession)
ITER <- 1:200
# -2 is to avoid Alberta2
thresholds <- furrr::future_map(fits[-2], get_thresh, iter = ITER)
plan(sequential)
saveRDS(thresholds,
  file = "selfIsolationModel/contact-ratios/data-generated/contact-ratio-thresholds.rds"
)
thresholds <- readRDS("selfIsolationModel/contact-ratios/data-generated/contact-ratio-thresholds.rds")

# -2 is to avoid Alberta2
f2 <- map(fits[-2], ~ .x$post$f_s[ITER, 1])
ratios <- map2_dfr(thresholds, f2, ~ tibble(ratio = .y / .x), .id = "region")

ggplot(ratios, aes(ratio)) +
  facet_wrap(~region) +
  geom_histogram() +
  geom_vline(xintercept = 1, lty = 2) +
  ggsidekick::theme_sleek()

breaks <- seq(0.25, 1.25, length.out = 25)

make_hist <- function(df) {
  region <- gsub("1", "", unique(df$region))
  ggplot(df) +
    ylab("Density") +
    geom_vline(xintercept = 1, lty = 2, col = "grey60") +
    geom_histogram(
      breaks = breaks, aes(x = ratio, y = ..density..),
      fill = "#377EB8", alpha = .75, colour = "grey90", lwd = 0.3
    ) +
    coord_cartesian(xlim = range(breaks), expand = FALSE) +
    xlab("Contact ratio") +
    # facet_wrap(~region) +
    ggsidekick::theme_sleek() +
    theme(axis.title.y.left = element_blank()) +
    theme(axis.text.y.left = element_blank()) +
    theme(axis.ticks.y.left = element_blank()) +
    theme(plot.margin = margin(t = 11 / 2, r = 13, b = 11 / 2, l = 13)) +
    ggtitle(region)
}

hists <- group_split(ratios, region) %>%
  map(make_hist)
g <- cowplot::plot_grid(plotlist = hists)

ggsave("selfIsolationModel/contact-ratios/figs/contact-ratios.svg", width = 8, height = 6.5, plot = g)
ggsave("selfIsolationModel/contact-ratios/figs/contact-ratios.pdf", width = 8, height = 6.5, plot = g)
ggsave("selfIsolationModel/contact-ratios/figs/contact-ratios.png", width = 8, height = 6.5, plot = g)
