get_thresh <- function(obj, iter = seq_along(obj$post$R0),
  forecast_days = 25,
  fs = seq(0.3, 0.8, length.out = 4),
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

custom_tidy_seir <- function(x, resample_y_rep = 10, data_type_names = NULL) {
  if (resample_y_rep > 0) {
    x <- purrr::map_dfr(seq_len(resample_y_rep), function(i) {
      temp <- x
      temp$y_rep <- MASS::rnegbin(length(temp$y_rep),
        temp$mu,
        theta = temp$phi
      )
      temp$.obs_iteration <- i
      temp
    })
  }
  out <- dplyr::group_by(x, data_type, day, .obs_iteration, .iteration) %>%
    summarise(y_rep = sum(y_rep, na.rm = TRUE), mu = sum(mu, na.rm =
        TRUE))

  out <- dplyr::group_by(out, data_type, day)
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


custom_projection_plot <- function(pred_dat, obs_dat, col = "#377EB8") {
  g <- ggplot(pred_dat, aes_string(x = "date")) +
    geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95"),
      alpha = 0.2, fill = col) +
    geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75"),
      alpha = 0.2, fill = col) +
    geom_line(aes_string(y = "y_rep_mean"), lwd = 0.9, col = col) +
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
