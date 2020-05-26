source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

PROJ <- 30
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 100)
mults <- seq(1, 2, 0.2) %>% set_names()
imports <- c(0, 50) %>% set_names()
round_time <- seq(50, 500) # time increments to save for efficiency

future::plan(future::multisession)
tictoc::tic()
projections_imp <-
  map_dfr(mults, function(.mult) {
    cat(.mult, "\n")
    furrr::future_map2_dfr(fits, observed_data, function(.fit, .obs) {
      map_dfr(imports, function(.import) {
        days <- length(.obs$day)
        # be extra careful; first can make a big diff:
        .fit$stan_data$ode_control <- c(1e-09, 1e-08, 1e+08)
        x <- covidseir::project_seir(
          obj = .fit,
          iter = ITER_PROJ,
          forecast_days = PROJ,
          f_fixed_start = days + 1,
          f_multi = rep(.mult, PROJ),
          f_multi_seg = 1,
          imported_cases = .import,
          imported_window = 7,
          return_states = TRUE
        )
        dplyr::filter(
          x, time %in% round_time,
          variable %in% c("I", "Id", "R", "Rd", "E2", "E2d")
        ) # save space
      }, .id = "import")
    }, .id = "region")
  }, .id = "f_multi")
tictoc::toc()
future::plan(future::sequential)
saveRDS(projections_imp, file = file.path(dg_folder, "projections-multi-imp1.rds"))
projections_imp <- readRDS(file.path(dg_folder, "projections-multi-imp1.rds"))

R <- projections_imp %>%
  filter(variable %in% c("R", "Rd")) %>%
  filter(f_multi %in% 1.6) %>%
  group_by(f_multi, region, import, .iteration, time) %>%
  summarise(R_Rd = value[variable == "R"] + value[variable == "Rd"])

g <- ggplot(
  R,
  aes(time, R_Rd, colour = import, group = paste(.iteration, import))
) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  coord_cartesian(xlim = c(60, 150)) +
  theme_light()
ggsave(file.path(fig_folder, "proj-R.png"), width = 12, height = 9)

R <- projections_imp %>%
  filter(time %in% round_time) %>%
  filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration, time) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"])

g <- R %>%
  filter(time > 70) %>%
  group_by(time, f_multi, region, .iteration) %>%
  summarise(extra_R_Rd = cases[import == "50"] - cases[import == "0"]) %>%
  ggplot(aes(time, extra_R_Rd, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  theme_light() +
  scale_color_brewer(palette = "Spectral", direction = -1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE))
ggsave(file.path(fig_folder, "proj-new-R-Rd.png"), width = 12, height = 9)
ggsave(file.path(fig_folder, "proj-new-R-Rd.pdf"), width = 12, height = 9)

import_cases <- projections_imp %>%
  filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration) %>%
  filter(time == max(time)) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"]) %>%
  group_by(f_multi, region, .iteration) %>%
  summarise(extra_R_Rd = cases[import == "50"] - cases[import == "0"])

g <- import_cases %>%
  ggplot(aes(extra_R_Rd, fill = f_multi, colour = f_multi)) +
  geom_density(position = "identity", alpha = 0.3) +
  facet_wrap(~region, scales = "free_x") +
  scale_x_log10() +
  theme_light() +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  scale_colour_brewer(palette = "Spectral", direction = -1) +
  guides(colour = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE))

ggsave(file.path(fig_folder, "hist-imports.pdf"), width = 9, height = 9)
ggsave(file.path(fig_folder, "hist-imports.png"), width = 9, height = 9)
