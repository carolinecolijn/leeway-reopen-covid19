source("selfIsolationModel/contact-ratios/model-prep.R")
library(purrr)
library(future)
future::plan(future::multisession)

dg_folder <- "selfIsolationModel/contact-ratios/data-generated"
fig_folder <- "selfIsolationModel/contact-ratios/figs"
REGIONS <- c("AB1", "AB2", "BC", "CA", "MI", "NY", "ON", "QC", "WA")

obj_files <- paste0(
  "selfIsolationModel/contact-ratios/data-generated/",
  REGIONS, "-fit.rds"
)

fits <- map(obj_files, readRDS) %>% set_names(REGIONS)
fits
N_ITER <- length(fits[["QC"]]$post$R0)

dat_files <- paste0(
  "selfIsolationModel/contact-ratios/data-generated/",
  REGIONS, "-dat.rds"
)
dat_files

observed_data <- map(dat_files, readRDS) %>%
  set_names(REGIONS) %>%
  map(select, date, day, value)
observed_data

observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- REGIONS[[.x]]
  temp
}) %>% set_names(REGIONS)

observed_data_orig <- observed_data

observed_data$AB2$day <- max(observed_data$AB1$day) + observed_data$AB2$day
observed_data$AB <- bind_rows(observed_data$AB1, observed_data$AB2)
observed_data$AB1 <- NULL
observed_data$AB2 <- NULL
observed_data <- map(observed_data, mutate,
  region = ifelse(grepl("AB", region), "AB", region))

# Multiplicative projection:

PROJ <- 60 # days
set.seed(274929)

F_MULTI <- 1.2
ITER_PROJ <- sample(seq_len(N_ITER), 300) # downsample for speed

projections_multi <- map(names(fits), function(.x) {
  cat(.x, "\n")

  if (.x %in% c("MI", "NY", "WA", "CA") && F_MULTI == 1.4)
    return(NULL)

  if (.x == "AB1") {
    day_total <- nrow(observed_data_orig[["AB1"]]) +
      nrow(observed_data_orig[["AB2"]])
    AB2_days <- nrow(observed_data_orig[["AB2"]])
    .forecast_days <- AB2_days + PROJ
    .f_fixed_start <- day_total + 1
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = .forecast_days,
      f_fixed_start = .f_fixed_start,
      f_multi = rep(F_MULTI, PROJ)
    )
  } else { # if (.x %in% c("AB2", "BC", "ON", "QC")) {
    days <- length(observed_data_orig[[.x]]$day)
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(F_MULTI, PROJ)
    )
  }
}) %>% set_names(REGIONS)

if (F_MULTI == 1.2) {
  saveRDS(projections_multi, file = file.path(dg_folder, "all-projections-multi-1.2.rds"))
  projections_multi <- readRDS(file.path(dg_folder, "all-projections-multi-1.2.rds"))
}

if (F_MULTI == 1.4) {
  saveRDS(projections_multi, file = file.path(dg_folder, "all-projections-multi-1.4.rds"))
  projections_multi <- readRDS(file.path(dg_folder, "all-projections-multi-1.4.rds"))
}

# Join the 2 Alberta models:
ab1_look_up <- tibble(
  date = seq(
    min(observed_data_orig[["AB1"]]$date),
    min(observed_data_orig[["AB1"]]$date) + max(projections_multi$AB1$day),
    by = "1 day"
  ),
  day = seq(1, max(projections_multi$AB1$day) + 1)
)
p_ab1 <- left_join(
  projections_multi$AB1,
  ab1_look_up
)
ab2_look_up <- tibble(
  date = seq(
    min(observed_data_orig[["AB2"]]$date),
    min(observed_data_orig[["AB2"]]$date) + max(projections_multi$AB2$day),
    by = "1 day"
  ),
  day = seq(1, max(projections_multi$AB2$day) + 1)
)
p_ab2 <- left_join(
  projections_multi$AB2,
  ab2_look_up
)
p_ab <- bind_rows(p_ab1, p_ab2) %>%
  select(-day) %>%
  group_by(.iteration) %>%
  arrange(date) %>%
  mutate(data_type = 1)
p_ab <- left_join(p_ab, ab1_look_up) %>%
  select(-date)
projections_multi$AB <- p_ab
projections_multi$AB1 <- NULL
projections_multi$AB2 <- NULL

projections_multi <- purrr::compact(projections_multi) # in case some NULL on purpose
tidy_projections <- furrr::future_map(
  projections_multi, custom_tidy_seir, resample_y_rep = 150)

tidy_projections <- tidy_projections %>% .[order(names(.))]
observed_data <- observed_data %>% .[order(names(.))]
observed_data <- observed_data[names(tidy_projections)] # in case some removed
stopifnot(identical(names(tidy_projections), names(observed_data)))

plots <- map2(tidy_projections, observed_data, function(x, obs) {
  pred <- left_join(ab1_look_up, x, by = "day")
  pred <- dplyr::filter(pred, date <= lubridate::ymd("2020-07-15"))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(expand = FALSE,
      xlim = c(lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-07-15")))
})

g <- cowplot::plot_grid(plotlist = plots[c("BC", "AB", "ON", "QC")], align = "hv")
ggsave(file.path(fig_folder, "projections-canada-1.2.svg"),
  width = 7.5, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-canada-1.2.pdf"),
  width = 7.5, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-canada-1.2.png"),
  width = 7.5, height = 5.5, plot = g)

# g <- cowplot::plot_grid(plotlist = plots[c("BC", "AB", "ON", "QC")], align = "hv")
# ggsave(file.path(fig_folder, "projections-canada-1.4.svg"),
#   width = 7.5, height = 5.5, plot = g)
# ggsave(file.path(fig_folder, "projections-canada-1.4.pdf"),
#   width = 7.5, height = 5.5, plot = g)
# ggsave(file.path(fig_folder, "projections-canada-1.4.png"),
#   width = 7.5, height = 5.5, plot = g)

g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 2)
ggsave(file.path(fig_folder, "projections-all-1.2.svg"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all-1.2.pdf"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all-1.2.png"),
  width = 14, height = 5.5, plot = g)

plots <- map2(tidy_projections, observed_data, function(x, obs) {
  pred <- left_join(ab1_look_up, x, by = "day")
  pred <- dplyr::filter(pred, date <= max(obs$date))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(expand = FALSE,
      xlim = c(lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-07-15")))
})
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 2)
ggsave(file.path(fig_folder, "projections-all.svg"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all.pdf"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all.png"),
  width = 14, height = 5.5, plot = g)

plots <- map2(tidy_projections, observed_data, function(x, obs) {
  pred <- left_join(ab1_look_up, x, by = "day")
  pred <- dplyr::filter(pred, date <= max(obs$date))
  custom_projection_plot(pred_dat = pred, obs_dat = obs) +
    ggtitle(unique(obs$region)) +
    coord_cartesian(expand = FALSE,
      xlim = c(lubridate::ymd("2020-03-01"),
        lubridate::ymd("2020-05-21")))
})
g <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 2)
ggsave(file.path(fig_folder, "projections-all2.svg"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all2.pdf"),
  width = 14, height = 5.5, plot = g)
ggsave(file.path(fig_folder, "projections-all2.png"),
  width = 14, height = 5.5, plot = g)

# Histograms ------------------------------------------------

# ITER <- sample(seq_len(N_ITER), 10) # downsample for speed (not matching iters!?)
ITER <- 1:400 # downsample for speed
# -2 is to avoid Alberta2
# thresholds <- furrr::future_map(fits[-2], get_thresh, iter = ITER)
thresholds <- map(fits[-2], get_thresh, iter = ITER)

saveRDS(thresholds, file = file.path(dg_folder, "contact-ratio-thresholds.rds"))
thresholds <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))

# check:
thresholds %>% bind_rows(.id = "ignore") %>% tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~name)

# -2 is to avoid Alberta2
f2 <- map(fits[-2], ~ .x$post$f_s[ITER, 1])

# check:
f2 %>% bind_rows(.id = "ignore") %>% tidyr::pivot_longer(-1) %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~name)

ratios <- map2_dfr(thresholds, f2, ~ tibble(ratio = .y / .x), .id = "region")

ggplot(ratios, aes(ratio)) +
  facet_wrap(~region) +
  geom_histogram() +
  geom_vline(xintercept = 1, lty = 2) +
  ggsidekick::theme_sleek()

(q <- quantile(ratios$ratio, probs = c(0, 1)))
breaks <- seq(q[1], q[2], length.out = 25)

make_hist <- function(df) {
  region <- gsub("1", "", unique(df$region))
  ggplot(df) +
    ylab("Density") +
    geom_histogram(
      breaks = breaks, aes(x = ratio, y = ..density..),
      fill = "#377EB8", alpha = .75, colour = "grey90", lwd = 0.3
    ) +
    geom_vline(xintercept = 1, lty = 2, col = "grey60") +
    coord_cartesian(xlim = range(breaks), expand = FALSE) +
    xlab("Contact ratio") +
    ggsidekick::theme_sleek() +
    theme(axis.title.y.left = element_blank()) +
    theme(axis.text.y.left = element_blank()) +
    theme(axis.ticks.y.left = element_blank()) +
    theme(plot.margin = margin(t = 11 / 2, r = 13, b = 11 / 2, l = 13)) +
    ggtitle(region)
}

hists <- group_split(ratios, region) %>%
  map(make_hist)
g <- cowplot::plot_grid(plotlist = hists, align = "hv")

ggsave(file.path(fig_folder, "contact-ratios.svg"), width = 8, height = 6.5, plot = g)
ggsave(file.path(fig_folder, "contact-ratios.pdf"), width = 8, height = 6.5, plot = g)
ggsave(file.path(fig_folder, "contact-ratios.png"), width = 8, height = 6.5, plot = g)

# ----------------------------------------

PROJ <- 60
set.seed(12898221)
ITER_PROJ <- sample(seq_len(N_ITER), 60) # downsample for speed
mults <- c(1, 1.2, 1.4, 1.6)
PROV <- "ON"
projections_select <- map(mults, function(.x) {
  cat(.x, "\n")
  days <- length(observed_data_orig[[PROV]]$day)
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
  resample_y_rep = 100
)

custom_projection_plot2 <- function(pred_dat, obs_dat, col = "#377EB8") {
  g <- ggplot(pred_dat, aes_string(x = "date")) +
    geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95", fill = "frac"),
      alpha = 0.2) +
    geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75", fill = "frac"),
      alpha = 0.2) +
    geom_line(aes_string(y = "y_rep_0.50", col = "frac"), lwd = 0.9) +
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

out <- tidy_projections %>% bind_rows(.id = "frac")
out <- left_join(ab1_look_up, out, by = "day")
obs <- observed_data[[PROV]]

g <- custom_projection_plot2(pred_dat = out, obs_dat = obs) +
  ggtitle(unique(obs$region)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  coord_cartesian(expand = FALSE, xlim = range(out$date), ylim = c(0, 2000))


ggsave(file.path(fig_folder, "proj-ON-fractions.svg"), width = 5.5, height = 3.5, plot = g)
ggsave(file.path(fig_folder, "proj-ON-fractions.pdf"), width = 5.5, height = 3.5, plot = g)
ggsave(file.path(fig_folder, "proj-ON-fractions.png"), width = 5.5, height = 3.5, plot = g)
