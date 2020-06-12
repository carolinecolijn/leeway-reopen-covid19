
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
custom_projection_plot2 <- function(pred_dat, obs_dat) {
  g <- ggplot(pred_dat, aes(x = date)) +
    geom_ribbon(aes(ymin = y_rep_0.05, ymax = y_rep_0.95, fill = frac),
      alpha = 0.25) +
    # geom_ribbon(aes(ymin = y_rep_0.25, ymax = y_rep_0.75, fill = frac),
    #   alpha = 0.2) +
    geom_line(aes(y = y_rep_0.50, col = frac), lwd = 0.9) +
    scale_colour_viridis_d(end = 0.95) +
    scale_fill_viridis_d(end = 0.95) +
    coord_cartesian(expand = FALSE, xlim = range(out$date), ylim = c(0, 2000)) +
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
    theme(axis.title.x.bottom = element_blank()) +
    labs(colour = "Re-opening\nfraction", fill = "Re-opening\nfraction")
  g
}
