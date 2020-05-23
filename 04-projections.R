source("selfIsolationModel/contact-ratios/model-prep.R")
library(purrr)
library(future)
future::plan(future::multisession)

dg_folder <- "selfIsolationModel/contact-ratios/data-generated/"
fig_folder <- "selfIsolationModel/contact-ratios/figs/"
dir.create(dg_folder, showWarnings = FALSE)
dir.create(fig_folder, showWarnings = FALSE)
REGIONS <- c("BC", "BE", "CA", "DE", "FL", "MI", "NY", "NZ", "ON", "QC", "UK", "WA", "SWE")
REGIONS <- sort(REGIONS)
N_ITER <- CHAINS * ITER / 2
PROJ_ITER <- 100
RESAMPLE_ITER <- 100

obj_files <- paste0(dg_folder, REGIONS, "-fit.rds")
obj_files

fits <- map(obj_files, readRDS) %>% set_names(REGIONS)
walk(fits, print)

dat_files <- paste0(dg_folder, REGIONS, "-dat.rds")
dat_files

observed_data <- map(dat_files, readRDS) %>%
  set_names(REGIONS) %>%
  map(select, date, day, value)
observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- REGIONS[[.x]]
  temp
}) %>% set_names(REGIONS)
observed_data

# Multiplicative projection: ------------------------------------------------

PROJ <- 60 # days
set.seed(274929)

F_MULTI <- 1.2
ITER_PROJ <- sample(seq_len(N_ITER), PROJ_ITER) # downsample for speed

projections_multi <- furrr::future_map2(fits, observed_data, function(.x, .y) {
  days <- length(.y$day)
  covidseir::project_seir(
    .x,
    iter = ITER_PROJ,
    forecast_days = PROJ,
    f_fixed_start = days + 1,
    f_multi = rep(F_MULTI, PROJ)
  )
})

saveRDS(projections_multi, file = file.path(dg_folder, "all-projections-multi-1.2.rds"))
projections_multi <- readRDS(file.path(dg_folder, "all-projections-multi-1.2.rds"))

tidy_projections <- furrr::future_map(
  projections_multi, custom_tidy_seir,
  resample_y_rep = RESAMPLE_ITER
)
stopifnot(identical(names(tidy_projections), names(observed_data)))

# Add dates:
tidy_projections <- map2(tidy_projections, observed_data, function(pred, obs) {
  first_day <- min(obs$date)
  mutate(pred, date = seq(first_day, first_day + nrow(pred) - 1, by = "1 day"))
})

# With projection:
plots <- map2(tidy_projections, observed_data, function(pred, obs) {
  pred <- dplyr::filter(pred, date <= lubridate::ymd("2020-07-15"))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-07-15")
      )
    )
})

g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)
ggsave(file.path(fig_folder, "projections-all-1.2.pdf"),
  width = 12, height = 8, plot = g
)
ggsave(file.path(fig_folder, "projections-all-1.2.png"),
  width = 12, height = 8, plot = g
)

# No projection:
plots <- map2(tidy_projections, observed_data, function(pred, obs) {
  pred <- dplyr::filter(pred, date <= max(obs$date))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(
      expand = FALSE,
      xlim = c(
        lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-05-21")
      )
    )
})
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4)
ggsave(file.path(fig_folder, "projections-all.pdf"),
  width = 12, height = 8, plot = g
)
ggsave(file.path(fig_folder, "projections-all.png"),
  width = 12, height = 8, plot = g
)

# Histograms ----------------------------------------------------------------

# ITER <- sample(seq_len(N_ITER), 400) # downsample for speed (not matching iters!?)
ITER <- 1:150 # downsample for speed
thresholds <- map(fits, get_thresh, iter = ITER)
saveRDS(thresholds, file = file.path(dg_folder, "contact-ratio-thresholds.rds"))
thresholds <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))
# check:
thresholds %>%
  bind_rows(.id = "ignore") %>%
  tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name)

f2 <- map(fits, ~ .x$post$f_s[ITER, 1])
# check:
f2 %>%
  bind_rows(.id = "ignore") %>%
  tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name)

ratios <- map2_dfr(thresholds, f2, ~ tibble(ratio = .y / .x), .id = "region")
ggplot(ratios, aes(ratio)) +
  facet_wrap(~region) +
  geom_histogram() +
  geom_vline(xintercept = 1, lty = 2) +
  ggsidekick::theme_sleek()

q <- range(ratios$ratio)
breaks <- seq(q[1], q[2], length.out = 25)

hists <- group_split(ratios, region) %>% map(make_hist)
g <- cowplot::plot_grid(plotlist = hists, align = "hv")
ggsave(file.path(fig_folder, "contact-ratios.pdf"), width = 8, height = 6.5, plot = g)
ggsave(file.path(fig_folder, "contact-ratios.png"), width = 8, height = 6.5, plot = g)

# Violin plots: -------------------------------------------------------------

ratios %>%
  ggplot(aes(x = region, y = ratio)) +
  geom_violin() +
  coord_flip() +
  ggsidekick::theme_sleek() +
  geom_hline(yintercept = 1)

# Example projections at multiple levels for one region: --------------------

PROJ <- 60
set.seed(12898221)
ITER_PROJ <- sample(seq_len(N_ITER), PROJ_ITER) # downsample for speed
PROV <- "ON"
mults <- c(1.0, 1.2, 1.4, 1.6, 1.8)

days <- length(observed_data[[PROV]]$day)
projections_select <- furrr::future_map(mults, function(.x) {
  covidseir::project_seir(
    fits[[PROV]],
    iter = ITER_PROJ,
    forecast_days = PROJ,
    f_fixed_start = days + 1,
    f_multi = rep(.x, PROJ)
  )
}) %>% set_names(as.character(mults))
tidy_projections <- map(
  projections_select,
  custom_tidy_seir,
  resample_y_rep = RESAMPLE_ITER
)

# Add dates:
obs <- observed_data[[PROV]]
tidy_projections <- map(tidy_projections, function(pred) {
  first_day <- min(obs$date)
  mutate(pred, date = seq(first_day, first_day + nrow(pred) - 1, by = "1 day"))
})

out <- tidy_projections %>% bind_rows(.id = "frac")
g <- custom_projection_plot2(pred_dat = out, obs_dat = obs) +
  ggtitle(unique(obs$region))
ggsave(file.path(fig_folder, "proj-ON-fractions.pdf"), width = 5.5, height = 3.5, plot = g)
ggsave(file.path(fig_folder, "proj-ON-fractions.png"), width = 5.5, height = 3.5, plot = g)

future::plan(future::sequential)
