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

fan_plot2 <- function(fit, pred, obs, multiplier) {

  .s1 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.05) - 1
  .s2 <- min(obs$date) + quantile(fit$post$start_decline, probs = 0.95) - 1
  .e1 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.05) - 1
  .e2 <- min(obs$date) + quantile(fit$post$end_decline, probs = 0.95) - 1

  pred_hist <- filter(pred, day <= max(fit$days))
  pred <- filter(pred, day >= max(fit$days))

  # multiplier <- mult
  # if (obs$region[[1]] %in% c("JP")) {
  #   multiplier <- 1.5
  # }
  ymax <- max(obs$value, na.rm = TRUE) * multiplier

  ggplot(pred, aes(x = date)) +
    geom_vline(xintercept = ymd("2020-05-01"), lty = 2, col = "grey50", alpha = 0.6) +
    annotate("rect", xmin = .s1, xmax = .s2, ymin = 0, ymax = Inf, fill = "grey60", alpha = 0.55) +
    annotate("rect", xmin = .e1, xmax = .e2, ymin = 0, ymax = Inf, fill = "grey60", alpha = 0.55) +
    annotate("rect",
      xmin = max(pred_hist$date),
      xmax = ymd("2020-08-28"), fill = "grey40", alpha = 0.1,
      ymin = 0, ymax = Inf,
    ) +
    geom_ribbon(aes(ymin = y_rep_0.05, ymax = y_rep_0.95), data = pred_hist, alpha = 0.40, fill = "grey30") +
    geom_ribbon(aes(ymin = y_rep_0.05, ymax = y_rep_0.95, fill = f_multi), alpha = 0.25) +
    scale_colour_viridis_d(end = 0.95) +
    scale_fill_viridis_d(end = 0.95) +
    geom_line(aes(y = y_rep_0.50, col = f_multi), lwd = 0.9) +
    geom_line(aes(y = y_rep_0.50), data = pred_hist, lwd = 0.9, colour = "grey52") +
    ylab("Reported cases") +
    theme(axis.title.x = element_blank()) +
    geom_line(
      data = obs,
      col = "black", inherit.aes = FALSE,
      aes_string(x = "date", y = "value"),
      lwd = 0.35, alpha = 1.0
    ) +
    # geom_point(
    #   data = obs,
    #   col = "grey30", inherit.aes = FALSE,
    #   aes_string(x = "date", y = "value"),
    #   pch = 21, fill = "grey95", size = 1.15
    # ) +
    coord_cartesian(
      expand = FALSE, ylim = c(0, ymax),
      xlim = c(ymd("2020-03-01"), ymd("2020-08-18"))
    ) +
    ggsidekick::theme_sleek() +
    theme(axis.title.x.bottom = element_blank(), legend.position = "none") +
    labs(colour = "Re-opening\nfraction", fill = "Re-opening\nfraction") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 4))
}

make_f_seg <- function(.dat, .date = "2020-05-01") {
  f_seg <- c(0L, rep(1L, nrow(.dat) - 1))
  day_new_f <- which(.dat$date == lubridate::ymd(.date))
  f_seg[seq(day_new_f, length(f_seg))] <- 2L
  f_seg
}
