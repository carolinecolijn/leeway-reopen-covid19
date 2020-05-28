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

# ITER <- 1:150 # downsample for speed
# thresholds_sens <- map(fits, get_thresh, iter = ITER) # subroutine is parallel
# future::plan(future::sequential)
# saveRDS(thresholds_sens, file = file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
# thresholds_sens <- readRDS(file.path(dg_folder, "contact-ratio-thresholds-sens.rds"))
#
# if ("SWE" %in% names(thresholds)) {
#   names(thresholds)[names(thresholds) == "SWE"] <- "SE"
# }
#
# f1 <- map(fits, ~ .x$post$f_s[ITER, 1])
# f2 <- map(fits, ~ .x$post$f_s[ITER, 2])
# ratios <- pmap_dfr(list(thresholds, f1, f2),
#   ~ tibble(ratio1 = ..2 / ..1, ratio2 = ..3 / ..1),
#   .id = "region"
# )
#
