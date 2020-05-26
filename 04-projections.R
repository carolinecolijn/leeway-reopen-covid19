source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")
future::plan(future::multisession)

# Look at fits: ------------------------------------------------
PROJ <- 1 # days
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
    f_multi = rep(F_MULTI, PROJ),
    f_multi_seg = 1
  )
})

saveRDS(projections_multi, file = file.path(dg_folder, "all-projections-multi-1.2.rds"))
projections_multi <- readRDS(file.path(dg_folder, "all-projections-multi-1.2.rds"))

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

# No projection:
plots <- furrr::future_pmap(list(fits, tidy_projections, observed_data), function(fit, pred, obs) {
  pred <- dplyr::filter(pred, date <= max(obs$date))

  .s1 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.05) - 1
  .s2 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.95) - 1
  .e1 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.05) - 1
  .e2 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.95) - 1

  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-05-21")
      )
    ) +
    geom_vline(xintercept = ymd("2020-05-01"), lty = 2, col = "grey50", alpha = 0.6) +
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

# Example projections at multiple levels for all regions: -------------------

PROJ <- 60
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), round(PROJ_ITER)) # *double* downsample for speed
ITER_PROJ <- sample(seq_len(N_ITER), round(60)) # *double* downsample for speed
mults <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0)

projections_fan <- map(mults, function(.mult) {
  cat(.mult, "\n")
  out <- furrr::future_map2(fits, observed_data, function(.fit, .obs) {
    days <- length(.obs$day)
    covidseir::project_seir(
      .fit,
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(.mult, PROJ),
      f_multi_seg = 1
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

stopifnot(identical(names(tidy_projections2), names(observed_data)))
plots <- pmap(list(fits, tidy_projections2, observed_data), fan_plot)
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)

ggsave(file.path(fig_folder, "proj-fan.pdf"), width = 12, height = 8, plot = g)
ggsave(file.path(fig_folder, "proj-fan.png"), width = 12, height = 8, plot = g)

# Risk calcs:

N <- map_dfr(fits, ~tibble(N = .x$stan_data$x_r[["N"]]), .id = "region")

hist_thresh <-
  # map_dfr(list(projections_fan[["2"]]['BC']), function(x1) {
  map_dfr(projections_fan, function(x1) {
    map_dfr(x1, function(x2) {
      group_by(x2, .iteration) %>%
        summarise(
          max_hist = max(y_rep[!forecast]),
          max_60 = y_rep[day == max(day)],
          above_hist_thresh = max_60 > max_hist
        )
    }, .id = "region")
  }, .id = "f_multi") %>%
  group_by(f_multi, region) %>%
  left_join(N, by = "region") %>%
  summarise(
    p_above_hist_thresh = mean(above_hist_thresh),
    p_above_1_1000_N = mean(max_60 > N / 1000),
    p_above_1_10000_N = mean(max_60 > N / 10000)
    # p_above_1_100000_N = mean(max_60 > N / 100000)
  ) %>%
  ungroup() %>%
  mutate(f_multi = as.numeric(f_multi))
hist_thresh

# check:
bc <- projections_fan[["1.8"]][["BC"]]
bc_summ <- group_by(bc, .iteration) %>%
  summarise(
    max_hist = max(y_rep[!forecast]),
    max_60 = y_rep[day == max(day)],
    above_hist_thresh = max_60 > max_hist
  )
plots <- ggplot(bc, aes(day, y_rep)) + geom_line() +
  facet_wrap(~.iteration) +
  geom_hline(aes(yintercept = max_60, colour = above_hist_thresh), data = bc_summ) +
  geom_hline(aes(yintercept = max_hist), data = bc_summ) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  ggsidekick::theme_sleek()
ggsave(file.path(fig_folder, "bc-hist-thresh-check.pdf"), width = 10, height = 10)
ggsave(file.path(fig_folder, "bc-hist-thresh-check.png"), width = 10, height = 10)

hist_thresh <- hist_thresh %>%
  left_join(country_lookup, by = "region")

cols <- hist_thresh %>%
  group_by(region) %>%
  summarise(region_group = region_group[1]) %>%
  group_by(region_group) %>%
  mutate(col = RColorBrewer::brewer.pal(8, "Dark2")[1:n()])
cols <- cols$col %>% set_names(cols$region)

# hist_thresh %>%
#   ggplot(aes(f_multi, p_above_hist_thresh, colour = region)) +
#   geom_line() +
#   facet_wrap(~region_group) +
#   ggsidekick::theme_sleek() +
#   ggrepel::geom_text_repel(data = filter(hist_thresh, f_multi == 1.8),
#     mapping = aes(x = f_multi + 0.01, label = region), hjust = 0, direction = "y") +
#   theme(legend.position = "none") +
#   scale_color_manual(values = cols)

hist_thresh_long <- hist_thresh %>% tidyr::pivot_longer(c(-f_multi, -region, -region_group))

g <- hist_thresh_long %>%
  ggplot(aes(f_multi, value, colour = region)) +
  annotate("rect", xmin = 1.8, xmax = 2.4, ymin = 0,
    ymax = 1, fill = "grey50", alpha = 0.1) +
  geom_line() +
  facet_grid(name~region_group) +
  ggsidekick::theme_sleek() +
  ggrepel::geom_text_repel(data = filter(hist_thresh_long, f_multi == 2),
    mapping = aes(x = f_multi, label = region), hjust = 0,
    direction = "y", nudge_x = 0.08,
    segment.colour = "grey65", segment.alpha = 0.7, segment.size = 0.3,
    size = 2.75) +
  theme(legend.position = "none", panel.spacing.y = unit(15, "pt")) +
  scale_color_manual(values = cols) +
  coord_cartesian(expand = FALSE, xlim = c(1, 2.25), ylim = c(-0.015, 1.015)) +
  scale_x_continuous(breaks = unique(hist_thresh_long$f_multi)) +
  xlab("Contact rate increase")+ylab("Probability")

ggsave(file.path(fig_folder, "f-mult-ref-probs.pdf"), width = 9, height = 5)
ggsave(file.path(fig_folder, "f-mult-ref-probs.png"), width = 9, height = 5)

future::plan(future::sequential)
