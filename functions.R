get_thresh <- function(obj, iter = seq_along(obj$post$R0),
  forecast_days = 25,
  fs = seq(0.3, 0.8, length.out = 4),
  show_plot = TRUE,
  window_check = 25) {
  # m_fs <- purrr::map(fs, function(.f) {
  m_fs <- furrr::future_map(fs, function(.f) {
    cat("Projecting", round(.f, 2), "\n")
    covidseir::project_seir(obj, forecast_days = forecast_days,
      iter = iter,
      f_fixed_start = nrow(obj$daily_cases) + 1,
      f_fixed = rep(.f, forecast_days),
      return_states = TRUE)
  })
  slopes <- purrr::map2_df(m_fs, fs, function(x, y) {
    temp <- x %>%
      dplyr::filter(time > max(x$time) - window_check,
        variable %in% c("I", "Id")) %>%
      dplyr::group_by(.iteration, time) %>%
      dplyr::summarize(
        I = value[variable == "I"], Id = value[variable == "Id"],
        prevalence = I + Id
      )
    iters <- temp %>%
      dplyr::group_by(.iteration) %>%
      dplyr::summarise(iter = .iteration[[1]])

    temp %>%
      dplyr::group_by(.iteration) %>%
      dplyr::group_split() %>%
      purrr::map(~ stats::lm(log(prevalence) ~ time, data = .x)) %>%
      purrr::map_df(~ tibble::tibble(slope = stats::coef(.x)[[2]])) %>%
      dplyr::mutate(f = y) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.iteration = iters$iter)
  })
  if (show_plot) {
    plot(slopes$f, slopes$slope)
  }

  nd <- data.frame(f = seq(0.01, 0.99, length.out = 2500))
  slopes %>%
    dplyr::group_by(.iteration) %>%
    dplyr::group_split() %>%
    purrr::map(~ stats::lm(slope ~ f, data = .x)) %>%
    purrr::map_dbl(function(.x) {
      nd$predicted_slope <- stats::predict(.x, newdata = nd)
      dplyr::filter(nd, predicted_slope > 0) %>% `[`(1, "f")
    })
}


# plot_f2_hist <- function(f2, col = RColorBrewer::brewer.pal(3, "Set1")[2],
#   threshold) {
#   .x <- seq(0, 1, length.out = 300)
#   breaks <- seq(min(.x), max(.x), 0.022)
#   ggplot(tibble(f2 = f2)) +
#     geom_histogram(
#       breaks = breaks, aes(x = f2, y = ..density..),
#       fill = col, alpha = .7, colour = "grey90", lwd = 0.15
#     ) +
#     ylab("Density") +
#     coord_cartesian(xlim = range(.x), expand = FALSE) +
#     xlab("Fraction of normal contacts") +
#     scale_x_continuous(breaks = seq(0, 1, 0.2)) +
#     geom_vline(xintercept = threshold, lty = 2, col = "grey40")
# }

custom_projection_plot <- function(pred_dat, obs_dat, col = "#377EB8") {
  g <- ggplot(pred_dat, aes_string(x = "date")) +
    geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95"),
      alpha = 0.2, fill = col) +
    geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75"),
      alpha = 0.2, fill = col) +
    geom_line(aes_string(y = "y_rep_0.50"), lwd = 0.9, col = col) +
    coord_cartesian(expand = FALSE, xlim = range(pred_dat$date)) +
    ylab("Reported cases") +
    theme(axis.title.x = element_blank())
  g <- g +
    geom_line(
      data = obs_dat,
      col = "black", inherit.aes = FALSE,
      aes_string(x = "date", y = "value"),
      lwd = 0.35, alpha = 0.9
    ) +
    geom_point(
      data = obs_dat,
      col = "grey30", inherit.aes = FALSE,
      aes_string(x = "date", y = "value"),
      pch = 21, fill = "grey95", size = 1.25
    ) +
    ggsidekick::theme_sleek() +
    theme(axis.title.x.bottom = element_blank())
  g
}

make_hist <- function(df) {
  region <- unique(df$region)
  ggplot(df) +
    ylab("Density") +
    geom_histogram(
      breaks = breaks, aes(x = ratio, y = ..density..),
      fill = "#377EB8", alpha = .75, colour = "grey90", lwd = 0.3
    ) +
    geom_vline(xintercept = 1, lty = 2, col = "grey60") +
    coord_cartesian(xlim = range(breaks), expand = FALSE) +
    xlab("Contact ratio") +
    ggsidekick::theme_sleek() +
    theme(axis.title.y.left = element_blank()) +
    theme(axis.text.y.left = element_blank()) +
    theme(axis.ticks.y.left = element_blank()) +
    theme(plot.margin = margin(t = 11 / 2, r = 13, b = 11 / 2, l = 13)) +
    ggtitle(region)
}

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
    annotate("rect",
      xmin = max(obs$date), xmax = ymd("2020-07-15"), fill = "grey40", alpha = 0.1,
      ymin = 0, ymax = max(obs$value, na.rm = TRUE) * 2
    ) +
    coord_cartesian(
      expand = FALSE, ylim = c(0, max(obs$value, na.rm = TRUE) * 2),
      xlim = c(ymd("2020-03-01"), ymd("2020-07-15"))
    ) +
    ggsidekick::theme_sleek() +
    theme(axis.title.x.bottom = element_blank()) +
    labs(colour = "Re-opening\nfraction", fill = "Re-opening\nfraction") +
    guides(fill = FALSE, colour = FALSE) +
    ggtitle(unique(obs$region))
}

make_f_seg <- function(.dat, .date = "2020-05-01") {
  f_seg <- c(0L, rep(1L, nrow(.dat) - 1))
  day_new_f <- which(.dat$date == lubridate::ymd(.date))
  f_seg[seq(day_new_f, length(f_seg))] <- 2L
  f_seg
}
