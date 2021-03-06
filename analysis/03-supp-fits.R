source("analysis/model-prep.R")
source("analysis/projection-prep.R")
future::plan(future::multisession)

# Look at fits: -------------------------------------------------------

set.seed(274929)
ITER_PROJ <- sample(seq_len(N_ITER), 200) # downsample for speed

projections_multi <- furrr::future_map2(fits, observed_data, function(.x, .y) {
  days <- length(.y$day)
  covidseir::project_seir(
    .x,
    iter = ITER_PROJ,
    forecast_days = 0
  )
})

saveRDS(projections_multi, file = file.path(dg_folder, "all-projections.rds"))
projections_multi <- readRDS(file.path(dg_folder, "all-projections.rds"))

tidy_projections <- furrr::future_map(
  projections_multi, covidseir::tidy_seir,
  resample_y_rep = RESAMPLE_ITER
)

stopifnot(identical(names(tidy_projections), names(observed_data)))

# Add dates:
tidy_projections <- map2(tidy_projections, observed_data, function(pred, obs) {
  first_day <- min(obs$date)
  mutate(pred, date = seq(first_day, first_day + nrow(pred) - 1, by = "1 day"))
})

responses <- tibble::tribble(~region, ~mass_gather, ~essential_services,
  "BC", "2020-03-12", "2020-03-16",
  "BE", "2020-03-13", "2020-03-18",
  "CA", "2020-03-11", "2020-03-19",
  "DE", "2020-03-8",  "2020-03-16",
  "JP", "2020-03-10", "2020-04-16",
  "NY", "2020-03-12", "2020-03-22",
  "NZ", "2020-03-16", "2020-03-25",
  "ON", "2020-03-13", "2020-03-17",
  "QC", "2020-03-12", "2020-03-13",
  "SE", "2020-03-11", "2020-03-26",
  "UK", "2020-03-16", "2020-03-23",
  "WA", "2020-03-11", "2020-03-23"
)

# No projection:
plots <- pmap(list(fits, tidy_projections, observed_data), function(fit, pred, obs) {

  # pred <- dplyr::filter(pred, day <= max(obs$date))
  obs <- dplyr::filter(obs, day <= max(pred$day))
  .s1 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.05) - 1
  .s2 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.95) - 1
  .e1 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.05) - 1
  .e2 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.95) - 1

  resp <- dplyr::filter(responses, region == obs$region[1])
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-06-07")
      )
    ) +
    geom_vline(xintercept = ymd("2020-05-01"), lty = 2, col = "grey50", alpha = 0.6) +
    geom_vline(xintercept = ymd(resp$mass_gather), lty = 3, col = "red", alpha = 0.8) +
    geom_vline(xintercept = ymd(resp$essential_services), lty = 3, col = "blue", alpha = 0.8) +
    annotate("rect", xmin = .s1, xmax = .s2, ymin = 0, ymax = Inf, fill = "grey50", alpha = 0.5) +
    annotate("rect", xmin = .e1, xmax = .e2, ymin = 0, ymax = Inf, fill = "grey50", alpha = 0.5)
})
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)
ggsave(file.path(fig_folder, "projections-all.pdf"),
  width = 12, height = 8, plot = g
)
ggsave(file.path(fig_folder, "projections-all.png"),
  width = 12, height = 8, plot = g
)

future::plan(future::sequential)
