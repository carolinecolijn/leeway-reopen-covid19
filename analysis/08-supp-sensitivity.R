source("analysis/model-prep.R")
source("analysis/projection-prep.R")
future::plan(future::multisession)

# Critical contact sensitivity: ---------------------------

dat <- observed_data$BC
SAMP_FRAC <- 0.2
SENS_ITER <- 220

plot(dat$date, dat$value)
dat <- dplyr::filter(dat, day %in% fits$BC$days)
plot(dat$date, dat$value)

samp_frac <- c(rep(0.14, 13), rep(0.21, 40 - 13), rep(0.21, 11))
samp_frac <- c(samp_frac, rep(0.37, nrow(dat) - length(samp_frac)))
samp_frac

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
names(sens_fits) <- c("D = 4, e ~ 0.83", "D = 5, e ~ 0.7", "D = 6, e ~ 0.83", "D = 5, e ~ 0.83")
names(thresholds_sens) <- c("D = 4, e ~ 0.83", "D = 5, e ~ 0.7", "D = 6, e ~ 0.83", "D = 5, e ~ 0.83")

f1 <- map(sens_fits, ~ .x$post$f_s[ITER, 1])
f2 <- map(sens_fits, ~ .x$post$f_s[ITER, 2])
ratios <- pmap_dfr(list(thresholds_sens, f1, f2),
  ~ tibble(`Post measures (f1)` = ..2 / ..1, `Recent (f2; after May 1)` = ..3 / ..1),
  .id = "sensitivity"
)

g <- ratios %>% tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.08) +
  facet_grid(sensitivity~name) +
  xlab("Threshold ratio") + ylab("Count") +
  coord_cartesian(xlim = c(0, 1.23), expand = FALSE) +
  theme(panel.spacing = unit(10, "pt"))
ggsave(file.path(fig_folder, "sens-ratios.pdf"), width = 6, height = 6, plot = g)
ggsave(file.path(fig_folder, "sens-ratios.png"), width = 6, height = 6, plot = g)

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
