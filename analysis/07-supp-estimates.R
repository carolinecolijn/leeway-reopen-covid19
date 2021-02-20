source("analysis/model-prep.R")
source("analysis/projection-prep.R")

posterior <- purrr::map2_df(fits, observed_data, function(x, y) {
  post_tidy <- tidybayes::gather_draws(x$fit, c(R0, i0, e, start_decline, end_decline)) %>% ungroup()
  post_tidy_f <- tidybayes::gather_draws(x$fit, f_s[f_seg]) %>%
    ungroup() %>%
    mutate(.variable = paste0(gsub("_s", "", .variable), f_seg)) %>%
    select(-f_seg)
  phi <- tidybayes::gather_draws(x$fit, phi[i]) %>%
    ungroup() %>%
    select(-i)
  bind_rows(post_tidy, post_tidy_f) %>%
    bind_rows(phi)
}, .id = "region")


m <- fits[[1]] # example; these values are all the same
getR0 <- function(R0b, fit) {
  k2 <- fit$stan_data$x_r[["k2"]]
  D <- fit$stan_data$x_r[["D"]]
  q <- fit$stan_data$x_r[["q"]]
  R0b * (  1/(q+1/D) + 1/k2 ) / ( D+1/k2)
}

posterior_nonR0 <- filter(posterior,!.variable == 'R0')
posterior_R0 <- filter(posterior,.variable == 'R0')
# posterior_R0$.value <- getR0(posterior_R0$.value , m)

# Informative priors to get:
# i0, R0, start_decline, end_decline, f1, f2, e

get_lnorm_dens <- function(pars, name, lwr = 0, upr = 60, length.out = 400) {
  x <- seq(lwr, upr, length.out = length.out)
  tibble(.variable = name, prior_x = x,
    prior_density = dlnorm(x, meanlog = pars[[1]], sdlog = pars[[2]]))
}
get_beta_dens <- function(pars, name, lwr = 0, upr = 1, length.out = 200) {
  x <- seq(lwr, upr, length.out = length.out)
  tibble(.variable = name, prior_x = x,
    prior_density = dbeta(x, shape1 = pars[[1]], shape2 = pars[[2]]))
}

posteriors <- bind_rows(posterior_nonR0, posterior_R0)
maxes <- group_by(posteriors, .variable) %>%
  summarise(.max = max(.value))
maxes$.max[maxes$.variable == "end_decline"] <- 60

priors <- map_dfr(names(fits), function(i) {
  m <- fits[[i]]
  priors <- list()
  priors[[1]] <- get_lnorm_dens(m$stan_data$i0_prior, "i0", upr = maxes$.max[maxes$.variable == "i0"], length.out = 1000)
  priors[[2]] <- get_lnorm_dens(m$stan_data$R0_prior, "R0", upr = maxes$.max[maxes$.variable == "R0"])
  priors[[3]] <- get_lnorm_dens(m$stan_data$start_decline_prior, "start_decline",
    upr = maxes$.max[maxes$.variable == "start_decline"])
  priors[[4]] <- get_lnorm_dens(m$stan_data$end_decline_prior, "end_decline", upr = maxes$.max[maxes$.variable == "end_decline"])
  priors[[5]] <- get_beta_dens(m$stan_data$f_prior[1,], "f1", upr = maxes$.max[maxes$.variable == "f1"])
  priors[[6]] <- get_beta_dens(m$stan_data$f_prior[2,], "f2", upr = maxes$.max[maxes$.variable == "f2"])
  priors[[7]] <- get_beta_dens(m$stan_data$e_prior, "e", upr = maxes$.max[maxes$.variable == "e"])
  priors <- bind_rows(priors) %>% mutate(region = i)
  priors
})

# prior_posteriors <- left_join(posterior, priors)

make_post_prior_plot <- function(posterior_dat, prior_dat = NULL, vars) {
  posterior_dat <- posterior_dat %>%
    mutate(.variable = factor(.variable,
      levels = c("i0", "R0b", "start_decline",
        "end_decline", "f1", "f2", "e", "phi")))
  g <- posterior_dat %>%
    ggplot(aes(x = .value)) +
    geom_histogram(aes(y = ..density..), bins = 35L, fill = "grey55") +
    facet_grid(region ~ .variable, scales = "free") +
    ylab("Density") +
    xlab("Parameter value") +
    coord_cartesian(expand = FALSE)
    # theme(axis.title.y = element_blank(), axis.title.x = element_blank())
  if (!is.null(prior_dat)) {
    prior_dat <- prior_dat %>%
      mutate(.variable = factor(.variable,
        levels = c("i0", "R0b", "start_decline",
          "end_decline", "f1", "f2", "e", "phi")))
    g <- g + geom_line(aes(x = prior_x, y = prior_density), data = prior_dat, inherit.aes = FALSE, lty = 1)
  }
  g
}

posteriors$.variable[posteriors$.variable == "R0"] <- "R0b"
priors$.variable[priors$.variable == "R0"] <- "R0b"

g_ef <- make_post_prior_plot(
  filter(posteriors, .variable %in% c("e", "f1", "f2")),
  filter(priors, .variable %in% c('e', 'f1', 'f2'))
  )
g_decline <- make_post_prior_plot(
  filter(posteriors, .variable %in% c("start_decline", "end_decline")),
  filter(priors, .variable %in% c('start_decline', 'end_decline'))
)
g_R0 <- make_post_prior_plot(
  filter(posteriors, .variable %in% c('R0b')),
  filter(priors, .variable %in% c('R0b'))
)
g_i0 <- make_post_prior_plot(
  filter(posteriors, .variable %in% c('i0')),
  filter(priors, .variable %in% c('i0'))
)
g_phi <- make_post_prior_plot(
  filter(posteriors, .variable %in% c('phi'))
)

g <- cowplot::plot_grid(g_i0, g_R0, g_decline, g_ef, g_phi, nrow = 1L,
  rel_widths = c(1, 1, 1.5, 1.85, 1))

ggsave(file.path(fig_folder, "parameter-estimates.pdf"), width = 14, height = 10.5, plot = g)
ggsave(file.path(fig_folder, "parameter-estimates.png"), width = 14, height = 10.5, plot = g)
