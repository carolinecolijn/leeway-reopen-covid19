source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")
future::plan(future::multisession)

# Critical contact sensitivity: ---------------------------

dat <- observed_data$DE
SAMP_FRAC <- 0.2
SENS_ITER <- 300

sens_fits <- list()
sens_fits[[1]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(5), 1),
    start_decline_prior = c(log(get_google_start("Germany", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Germany", dat)), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 83e6,
    pars = c(D = 4, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
    chains = CHAINS,
    iter = SENS_ITER
  )
sens_fits[[2]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(5), 1),
    start_decline_prior = c(log(get_google_start("Germany", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Germany", dat)), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 83e6,
    e_prior = c(0.7, 0.025),
    chains = CHAINS,
    iter = SENS_ITER
  )
sens_fits[[3]] <-
  covidseir::fit_seir(
    daily_cases = dat$value,
    samp_frac_fixed = rep(SAMP_FRAC, nrow(dat)),
    i0_prior = c(log(5), 1),
    start_decline_prior = c(log(get_google_start("Germany", dat)), 0.1),
    end_decline_prior = c(log(get_google_end("Germany", dat)), 0.1),
    f_seg = make_f_seg(dat),
    N_pop = 83e6,
    pars = c(D = 6, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
    chains = CHAINS,
    iter = SENS_ITER
  )
saveRDS(sens_fits, file = file.path(dg_folder, "fit-sens.rds"))
sens_fits <- readRDS(file.path(dg_folder, "fit-sens.rds"))

ITER <- 1:200 # downsample for speed
thresholds_sens <- map(sens_fits, get_thresh, iter = ITER) # subroutine is parallel
future::plan(future::sequential)
saveRDS(thresholds_sens, file = file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
thresholds_sens <- readRDS(file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
thresholds_main <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))

sens_fits <- c(sens_fits, list(fits$DE))
thresholds_sens <- c(thresholds_sens, list(thresholds_main$DE))
names(sens_fits) <- c("D = 4, e ~ 0.8", "D = 5, e ~ 0.7", "D = 6, e ~ 0.8", "D = 5, e ~ 0.8")
names(thresholds_sens) <- c("D = 4, e ~ 0.8", "D = 5, e ~ 0.7", "D = 6, e ~ 0.8", "D = 5, e ~ 0.8")

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
