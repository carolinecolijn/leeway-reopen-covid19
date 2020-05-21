get_thresh <- function(obj, iter = seq_along(obj$post$R0), forecast_days = 30,
  fs = seq(0.3, 0.8, length.out = 5),
  show_plot = TRUE,
  window_check = 25) {
  m_fs <- purrr::map(fs, function(.f) {
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

  nd <- data.frame(f = seq(0.2, 0.8, length.out = 2000))
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
