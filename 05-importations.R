source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

PROJ <- 60
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 100)
imports <- c(0, 500)

future::plan(future::multisession)
projections_imp <- map(imports, function(.import) {
  cat(.import, "\n")
  out <- furrr::future_map2(fits, observed_data, function(.fit, .obs) {
    days <- length(.obs$day)
    covidseir::project_seir(
      .fit,
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(1.8, PROJ),
      f_multi_seg = 1,
      imported_cases = .import,
      imported_window = 1
    )
  })
  map(out, mutate, imports = .import)
}) %>% set_names(as.character(imports))

saveRDS(projections_imp, file = file.path(dg_folder, "projections-multi-imp.rds"))
projections_imp <- readRDS(file.path(dg_folder, "projections-multi-imp.rds"))

tidy_seir_cumulative <- function(x, resample_y_rep = 10, data_type_names = NULL) {
  x <- group_by(x, data_type, .iteration) %>%
    mutate(y_rep = cumsum(y_rep))
  out <- dplyr::group_by(x, data_type, day)
  out <- dplyr::summarise(out,
    y_rep_0.05 = stats::quantile(y_rep, probs = 0.05),
    y_rep_0.25 = stats::quantile(y_rep, probs = 0.25),
    y_rep_mean = mean(y_rep),
    y_rep_0.50 = stats::quantile(y_rep, probs = 0.50),
    y_rep_0.75 = stats::quantile(y_rep, probs = 0.75),
    y_rep_0.95 = stats::quantile(y_rep, probs = 0.95),
    mu_0.05 = stats::quantile(mu, probs = 0.05),
    mu_0.25 = stats::quantile(mu, probs = 0.25),
    mu_mean = mean(mu),
    mu_0.50 = stats::quantile(mu, probs = 0.50),
    mu_0.75 = stats::quantile(mu, probs = 0.75),
    mu_0.95 = stats::quantile(mu, probs = 0.95),
    mu_0.5 = stats::quantile(mu, probs = 0.50)
  )
  if (!is.null(data_type_names)) {
    out$data_type <- names(data_type_names[as.numeric(out$data_type)])
  }
  out
}

tidy_projections <- furrr::future_map(projections_imp, function(x) {
  map(x, function(y) {
    tidy_seir_cumulative(y, resample_y_rep = RESAMPLE_ITER)
  })
})
future::plan(future::sequential)
tidy_projections1 <- map(tidy_projections, bind_rows, .id = "region")
tidy_projections1 <- tidy_projections1 %>% bind_rows(.id = "f_multi") # FAKE not actuall f_multi; for plotting hack
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

observed_data_cumsum <- map(observed_data, function(x) {
  x$value[is.na(x$value)] <- 0
  x$value <- cumsum(x$value)
  x
})

stopifnot(identical(names(tidy_projections2), names(observed_data)))
plots <- pmap(list(fits, tidy_projections2, observed_data_cumsum), function(x, y, z) {
  fan_plot(x, y, z) +
    coord_cartesian(
      expand = FALSE, ylim = c(0, NA),
      xlim = c(ymd("2020-03-01"), ymd("2020-07-15"))
    ) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2")
})

g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)

ggsave(file.path(fig_folder, "proj-imports.pdf"), width = 12, height = 8, plot = g)
ggsave(file.path(fig_folder, "proj-imports.png"), width = 12, height = 8, plot = g)
