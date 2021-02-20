source("analysis/model-prep.R")
source("analysis/projection-prep.R")
future::plan(future::multisession)

# Critical contact sensitivity: ---------------------------

# dat <- observed_data$DE
dat <- observed_data$BC
SAMP_FRAC <- 0.2
SENS_ITER <- 220

plot(dat$date, dat$value)
dat <- dplyr::filter(dat, day %in% fits$BC$days)
plot(dat$date, dat$value)

samp_frac <- c(rep(0.14, 13), rep(0.21, 40 - 13), rep(0.21, 11))
samp_frac <- c(samp_frac, rep(0.37, nrow(dat) - length(samp_frac)))
# samp_frac <- SAMP_FRAC

sens_fits <- list()
sens_fits[[1]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = SENS_ITER,
    pars = c(D = 4, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
    e_prior = c(0.8, 0.05),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )

sens_fits[[2]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = SENS_ITER,
    pars = c(D = 5, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = covidseir:::get_ur(0.7, 0.1), f0 = 1),
    e_prior = c(0.7, 0.1),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
sens_fits[[3]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = SENS_ITER,
    pars = c(D = 6, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
    e_prior = c(0.8, 0.05),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC
  )
# sens_fits[[4]] <- covidseir::fit_seir( # regular fit
#   daily_cases = dat$value,
#   samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
#   i0_prior = c(log(5), 1),
#   start_decline_prior = c(log(get_google_start("Germany", dat)), 0.1),
#   end_decline_prior = c(log(get_google_end("Germany", dat)), 0.1),
#   f_seg = make_f_seg(dat),
#   N_pop = 83e6,
#   pars = c(D = 5, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
#   chains = CHAINS,
#   iter = SENS_ITER,
#   # fit_type = "optimizing",
#   control = list(adapt_delta = 0.95)
# )
sens_fits[[4]] <- fits$BC

saveRDS(sens_fits, file = file.path(dg_folder, "fit-sens.rds"))
sens_fits <- readRDS(file.path(dg_folder, "fit-sens.rds"))

ITER <- 1:150 # downsample for speed
future::plan(future::multisession)
thresholds_sens <- map(sens_fits, covidseir::get_threshold, iter = ITER) # subroutine is parallel
future::plan(future::sequential)
saveRDS(thresholds_sens, file = file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
thresholds_sens <- readRDS(file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
# thresholds_main <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))

for (i in 1:4) hist(thresholds_sens[[i]], main = names(thresholds_sens)[[i]], xlim = c(0.2, 0.43))
for (i in 1:4) hist(sens_fits[[i]]$post$f_s[,1], main = names(thresholds_sens)[[i]], xlim = c(0, 0.3))

# sens_fits <- c(sens_fits, list(fits$DE))
# thresholds_sens <- c(thresholds_sens, list(thresholds_main$DE))
names(sens_fits) <- c("D = 4, e ~ 0.8", "D = 5, e ~ 0.7", "D = 6, e ~ 0.8", "D = 5, e ~ 0.8")
names(thresholds_sens) <- c("D = 4, e ~ 0.8", "D = 5, e ~ 0.7", "D = 6, e ~ 0.8", "D = 5, e ~ 0.8")

f1 <- map(sens_fits, ~ .x$post$f_s[ITER, 1])
f2 <- map(sens_fits, ~ .x$post$f_s[ITER, 2])
ratios <- pmap_dfr(list(thresholds_sens, f1, f2),
  ~ tibble(`Post measures (f1)` = ..2 / ..1, `Recent (f2; after May 1)` = ..3 / ..1),
  .id = "sensitivity"
)

g <- ratios %>% tidyr::pivot_longer(-1) %>%
  ggplot(aes(sensitivity, value)) +
  geom_violin() +
  facet_wrap(~name) +
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 90)) +
  ylab("Threshold ratio") + xlab("") +
  ylim(0, 1.5) +
  geom_hline(yintercept = 1, lty = 2, col = "grey70") +
  # coord_cartesian(xlim = c(0, 1.23), expand = FALSE) +
  theme(panel.spacing = unit(10, "pt"))
ggsave(file.path(fig_folder, "sens-ratios-violin.pdf"), width = 5, height = 5, plot = g)
ggsave(file.path(fig_folder, "sens-ratios-violin.png"), width = 5, height = 5, plot = g)

# Sensitivity to delay distribution: ----------------------------------------

x <- seq(0, 20, length.out = 200)
plot(x, dweibull(x, scale = 9.85, shape = 1.73), type = "l", ylim = c(0, 0.2))
lines(x, dweibull(x, scale = 5, shape = 1.73), col = "red")

sens_fits2 <- list()
sens_fits2[[1]] <- fits$BC
sens_fits2[[2]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = samp_frac, # from hospital fit
    i0_prior = c(log(8), 1),
    start_decline_prior = c(log(15), 0.1),
    end_decline_prior = c(log(22), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 5.1e6,
    chains = CHAINS,
    iter = SENS_ITER,
    e_prior = c(0.8, 0.05),
    ode_control = c(1e-07, 1e-06, 1e+06),
    time_increment = TIME_INC,
    init = "optimizing",
    delay_scale = 5,
    delay_shape = 1.73
  )

saveRDS(sens_fits2, file = file.path(dg_folder, "fit-sens2.rds"))
sens_fits2 <- readRDS(file.path(dg_folder, "fit-sens2.rds"))

ITER <- 1:200 # downsample for speed
thresholds_sens <- map(sens_fits2, covidseir::get_threshold, iter = ITER)
saveRDS(thresholds_sens, file = file.path(dg_folder, "contact-ratio-thresholds-sens2.rds"))
thresholds_sens <- readRDS(file.path(dg_folder, "contact-ratio-thresholds-sens2.rds"))

names(sens_fits2) <- c("delay_scale = 9.85", "delay_scale = 5")
names(thresholds_sens) <- c("delay_scale = 9.85", "delay_scale = 5")

p <- map(sens_fits2, project_seir)
g1 <- tidy_seir(p[[1]]) %>% plot_projection(obs_dat = observed_data[["BC"]],
  col = RColorBrewer::brewer.pal(3, "Set1")[2])
g2 <- tidy_seir(p[[2]]) %>% plot_projection(obs_dat = observed_data[["BC"]],
  col = RColorBrewer::brewer.pal(3, "Set1")[1])
cowplot::plot_grid(g1, g2, ncol = 1)
posterior <- purrr::map_dfr(sens_fits2, function(x) {
  post_tidy <- tidybayes::gather_draws(x$fit, c(R0, i0, e, start_decline, end_decline)) %>%
    ungroup()
  post_tidy_f <- tidybayes::gather_draws(x$fit, f_s[f_seg]) %>%
    ungroup() %>%
    mutate(.variable = paste0(gsub("_s", "", .variable), f_seg)) %>%
    select(-f_seg)
  phi <- tidybayes::gather_draws(x$fit, phi[i]) %>%
    ungroup() %>%
    select(-i)
  bind_rows(post_tidy, post_tidy_f) %>%
    bind_rows(phi)
}, .id = "delay_scale")

g <- posterior %>%
  mutate(.variable = factor(.variable,
    levels = c("i0", "R0", "start_decline",
      "end_decline", "f1", "f2", "e", "phi"))) %>%
  ggplot(aes(.value, fill = delay_scale)) + geom_density(alpha = 0.6) +
  facet_wrap(~.variable, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Parameter value") + ylab("Density")

ggsave(file.path(fig_folder, "sens-delay-posterior.pdf"), width = 7, height = 5, plot = g)
ggsave(file.path(fig_folder, "sens-delay-posterior.png"), width = 7, height = 5, plot = g)

f1 <- map(sens_fits2, ~ .x$post$f_s[ITER, 1])
f2 <- map(sens_fits2, ~ .x$post$f_s[ITER, 2])
ratios <- pmap_dfr(list(thresholds_sens, f1, f2),
  ~ tibble(`Post measures (f1)` = ..2 / ..1, `Recent (f2; after May 1)` = ..3 / ..1),
  .id = "sensitivity"
)

g <- ratios %>% tidyr::pivot_longer(-1) %>%
  ggplot(aes(value, fill = sensitivity)) +
  # geom_density(alpha = 0.6) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  scale_fill_brewer(palette = "Set1") +
  # geom_violin() +
  facet_wrap(~name) +
  # coord_flip() +
  # theme(axis.text.x = element_text(angle = 90)) +
  # ylab("Threshold ratio") + xlab("") +
  # ylim(0, 1.5) +
  # geom_hline(yintercept = 1, lty = 2, col = "grey70") +
  # coord_cartesian(xlim = c(0, 1.23), expand = FALSE) +
  # theme(panel.spacing = unit(10, "pt")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(limits = c(0, 1.1), expand = c(0, 0)) +
  geom_vline(xintercept = 1, lty = 2, col = "grey70") +
  theme(legend.position = c(0.2, 0.8)) +
  xlab("Threshold ratio") + ylab("Posterior sample count")
g
# ggsave(file.path(fig_folder, "sens-ratios-delay-violin.pdf"), width = 5, height = 5, plot = g)
# ggsave(file.path(fig_folder, "sens-ratios-delay-violin.png"), width = 5, height = 5, plot = g)

gg <- cowplot::plot_grid(g1 + ggtitle("delay_scale = 9.85"), g2 + ggtitle("delay_scale = 5"), ncol = 1)
cowplot::plot_grid(gg, g, rel_widths = c(1, 1.3))

ggsave(file.path(fig_folder, "sens-proj-ratios-delay.png"), width = 8, height = 5)
ggsave(file.path(fig_folder, "sens-proj-ratios-delay.pdf"), width = 8, height = 5)
