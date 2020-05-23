source("selfIsolationModel/contact-ratios/model-prep.R")
library(purrr)
library(future)
future::plan(future::multisession)

dg_folder <- "selfIsolationModel/contact-ratios/data-generated/"
fig_folder <- "selfIsolationModel/contact-ratios/figs/"
dir.create(dg_folder, showWarnings = FALSE)
dir.create(fig_folder, showWarnings = FALSE)
REGIONS <- c("BC", "BE", "CA", "DE", "FL", "MI", "NY", "NZ", "ON", "QC", "UK", "WA", "SWE")
REGIONS <- sort(REGIONS)
N_ITER <- CHAINS * ITER / 2
PROJ_ITER <- 100
RESAMPLE_ITER <- 100

obj_files <- paste0(dg_folder, REGIONS, "-fit.rds")
obj_files

fits <- map(obj_files, readRDS) %>% set_names(REGIONS)
walk(fits, print)

dat_files <- paste0(dg_folder, REGIONS, "-dat.rds")
dat_files

observed_data <- map(dat_files, readRDS) %>%
  set_names(REGIONS) %>%
  map(select, date, day, value)
observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- REGIONS[[.x]]
  temp
}) %>% set_names(REGIONS)
observed_data

# Multiplicative projection: ------------------------------------------------

PROJ <- 60 # days
set.seed(274929)

F_MULTI <- 1.2
ITER_PROJ <- sample(seq_len(N_ITER), PROJ_ITER) # downsample for speed

projections_multi <- furrr::future_map2(fits, observed_data, function(.x, .y) {
  days <- length(.y$day)
  covidseir::project_seir(
    .x,
    iter = ITER_PROJ,
    forecast_days = PROJ,
    f_fixed_start = days + 1,
    f_multi = rep(F_MULTI, PROJ)
  )
})

saveRDS(projections_multi, file = file.path(dg_folder, "all-projections-multi-1.2.rds"))
projections_multi <- readRDS(file.path(dg_folder, "all-projections-multi-1.2.rds"))

tidy_projections <- furrr::future_map(
  projections_multi, custom_tidy_seir,
  resample_y_rep = RESAMPLE_ITER
)
stopifnot(identical(names(tidy_projections), names(observed_data)))

# Add dates:
tidy_projections <- map2(tidy_projections, observed_data, function(pred, obs) {
  first_day <- min(obs$date)
  mutate(pred, date = seq(first_day, first_day + nrow(pred) - 1, by = "1 day"))
})

# With projection:
plots <- map2(tidy_projections, observed_data, function(pred, obs) {
  pred <- dplyr::filter(pred, date <= lubridate::ymd("2020-07-15"))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-07-15")
      )
    )
})

g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)
ggsave(file.path(fig_folder, "projections-all-1.2.pdf"),
  width = 12, height = 8, plot = g
)
ggsave(file.path(fig_folder, "projections-all-1.2.png"),
  width = 12, height = 8, plot = g
)

# No projection:
plots <- map2(tidy_projections, observed_data, function(pred, obs) {
  pred <- dplyr::filter(pred, date <= max(obs$date))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-05-21")
      )
    )
})
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)
ggsave(file.path(fig_folder, "projections-all.pdf"),
  width = 12, height = 8, plot = g
)
ggsave(file.path(fig_folder, "projections-all.png"),
  width = 12, height = 8, plot = g
)

# Histograms ----------------------------------------------------------------

# ITER <- sample(seq_len(N_ITER), 400) # downsample for speed (not matching iters!?)
ITER <- 1:150 # downsample for speed
thresholds <- map(fits, get_thresh, iter = ITER)
saveRDS(thresholds, file = file.path(dg_folder, "contact-ratio-thresholds.rds"))
thresholds <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))
# check:
thresholds %>%
  bind_rows(.id = "ignore") %>%
  tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name)

f2 <- map(fits, ~ .x$post$f_s[ITER, 1])
# check:
f2 %>%
  bind_rows(.id = "ignore") %>%
  tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name)

ratios <- map2_dfr(thresholds, f2, ~ tibble(ratio = .y / .x), .id = "region")
ggplot(ratios, aes(ratio)) +
  facet_wrap(~region) +
  geom_histogram() +
  geom_vline(xintercept = 1, lty = 2) +
  ggsidekick::theme_sleek()

# hists <- group_split(ratios, region) %>% map(make_hist)
# g <- cowplot::plot_grid(plotlist = hists, align = "hv")
# ggsave(file.path(fig_folder, "contact-ratios.pdf"), width = 8, height = 6.5, plot = g)
# ggsave(file.path(fig_folder, "contact-ratios.png"), width = 8, height = 6.5, plot = g)

# Violin plots: -------------------------------------------------------------

country_lookup <- tibble::tribble(
  ~region, ~region_group,
  "BC", "CAN",
  "BE", "EU",
  "CA", "US",
  "DE", "EUR",
  "FL", "US",
  "MI", "US",
  "NY", "US",
  "NZ", "PAC",
  "ON", "CAN",
  "QC", "CAN",
  "SWE", "EUR",
  "UK", "EUR",
  "WA", "US"
)
set.seed(1)
g <- ratios %>%
  left_join(country_lookup) %>%
  mutate(region_group = forcats::fct_shuffle(region_group)) %>%
  group_by(region) %>%
  mutate(mean_ratio = mean(ratio)) %>%
  ungroup() %>%
  ggplot(aes(x = forcats::fct_reorder(region, -mean_ratio), y = ratio)) +
  geom_hline(yintercept = 1, alpha = 0.4) +
  geom_violin(aes(fill = region_group), colour = "grey40", lwd = 0.35) +
  coord_flip(ylim = c(0, 1.4), expand = FALSE) +
  ggsidekick::theme_sleek() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y = element_blank(), legend.position = c(0.12, 0.15)) +
  labs(fill = "Region", y = "Threshold ratio")
ggsave(file.path(fig_folder, "ratio-violins.pdf"), width = 4, height = 5)
ggsave(file.path(fig_folder, "ratio-violins.png"), width = 4, height = 5)

# Example projections at multiple levels for all regions: -------------------

PROJ <- 60
set.seed(12898221)
ITER_PROJ <- sample(seq_len(N_ITER), round(PROJ_ITER / 2)) # *double* downsample for speed
mults <- c(1.0, 1.2, 1.4, 1.6, 1.8)

projections_fan <- map(mults, function(.mult) {
  cat(.mult, "\n")
  out <- furrr::future_map2(fits, observed_data, function(.fit, .obs) {
    days <- length(.obs$day)
    covidseir::project_seir(
      .fit,
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(.mult, PROJ)
    )
  })
  map(out, mutate, f_multi = .mult)
}) %>% set_names(as.character(mults))
saveRDS(projections_fan, file = file.path(dg_folder, "projections-multi-fan.rds"))
projections_fan <- readRDS(file.path(dg_folder, "projections-multi-fan.rds"))

tidy_projections <- furrr::future_map(projections_fan, function(x) {
  map(x, function(y) {
    covidseir::tidy_seir(y, resample_y_rep = RESAMPLE_ITER)
  })
})
tidy_projections1 <- map(tidy_projections, bind_rows, .id = "region")
tidy_projections1 <- tidy_projections1 %>% bind_rows(.id = "f_multi")
tidy_projections1 <- split(tidy_projections1, tidy_projections1$region)

# Add dates:
tidy_projections2 <- map2(tidy_projections1, observed_data, function(pred, obs) {
  first_day <- min(obs$date)
  lu <- tibble(
    day = sort(unique(pred$day)),
    date = seq(first_day, first_day + length(day) - 1, by = "1 day")
  )
  left_join(pred, lu, by = "day")
})

# Plot!

fan_plot <- function(pred, obs) {
  ggplot(pred, aes(x = date)) +
    geom_ribbon(aes(ymin = y_rep_0.05, ymax = y_rep_0.95, fill = f_multi), alpha = 0.25) +
    geom_line(aes(y = y_rep_0.50, col = f_multi), lwd = 0.9) +
    scale_colour_viridis_d(end = 0.95) +
    scale_fill_viridis_d(end = 0.95) +
    ylab("Reported cases") +
    theme(axis.title.x = element_blank()) +
    geom_line(
      data = obs,
      col = "black", inherit.aes = FALSE,
      aes_string(x = "date", y = "value"),
      lwd = 0.35, alpha = 0.9
    ) +
    geom_point(
      data = obs,
      col = "grey30", inherit.aes = FALSE,
      aes_string(x = "date", y = "value"),
      pch = 21, fill = "grey95", size = 1.25
    ) +
    annotate("rect", xmin = max(obs$date), xmax = ymd("2020-07-15"), col = "grey40", alpha = 0.1,
      ymin = 0, ymax = max(obs$value, na.rm = TRUE) * 2) +
    coord_cartesian(expand = FALSE, ylim = c(0, max(obs$value, na.rm = TRUE) * 2),
      xlim = c(ymd("2020-03-01"), ymd("2020-07-15"))) +
    ggsidekick::theme_sleek() +
    theme(axis.title.x.bottom = element_blank()) +
    labs(colour = "Re-opening\nfraction", fill = "Re-opening\nfraction") +
    guides(fill = FALSE, colour = FALSE) +
    ggtitle(unique(obs$region))
}

stopifnot(identical(names(tidy_projections2), names(observed_data)))
plots <- map2(tidy_projections2, observed_data, fan_plot)
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)

ggsave(file.path(fig_folder, "proj-fan.pdf"), width = 12, height = 8, plot = g)
ggsave(file.path(fig_folder, "proj-fan.png"), width = 12, height = 8, plot = g)

future::plan(future::sequential)
