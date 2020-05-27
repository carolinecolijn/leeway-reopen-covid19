source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

WEEKS <- 8
PROJ <- WEEKS * 7
CASE_PER_WEEK <- 10
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 80)
mults <- seq(1, 2, 0.2) %>% set_names()
imports <- c(0, CASE_PER_WEEK * WEEKS) %>% set_names()
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
          imported_window = PROJ,
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

last_obs <- observed_data %>% map_dfr(~tibble(.last_obs = max(.$day)), .id = "region")

extra_cases <- R %>%
  filter(time > 50) %>%
  filter(!f_multi %in% "2") %>%
  left_join(last_obs) %>%
  group_by(region) %>%
  filter(time <= .last_obs + 42, time >= .last_obs) %>%
  mutate(day = time - .last_obs + 1) %>%
  group_by(day, f_multi, region, .iteration) %>%
  summarise(
    orig_R_Rd = cases[import == "0"],
    extra_R_Rd = cases[import == names(imports[2])] - cases[import == "0"],
    extra_R_Rd1 = extra_R_Rd / 10,
    extra_R_Rd10 = extra_R_Rd,
    extra_frac_R_Rd1 = extra_R_Rd1 / orig_R_Rd * 100,
    extra_perc_R_Rd10 = extra_R_Rd10 / orig_R_Rd * 100
  )

# extra_cases %>%
#   ggplot(aes(day, extra_perc_R_Rd10, colour = f_multi, group = paste(.iteration, f_multi))) +
#   geom_line(alpha = 0.35) +
#   facet_wrap(~region, scales = "free_y") +
#   scale_colour_viridis_d(option = "C") +
#   ylab("Extra cases") +
#   coord_cartesian(expand = FALSE) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE))


g <- extra_cases %>%
  ggplot(aes(day, extra_R_Rd1, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.35) +
  facet_wrap(~region, scales = "free_y") +
  scale_colour_viridis_d(option = "C") +
  ylab("Extra cases") +
  coord_cartesian(expand = FALSE) +
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
  # scale_fill_brewer(palette = "Spectral", direction = -1) +
  # scale_colour_brewer(palette = "Spectral", direction = -1) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  guides(colour = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE))

make_dens_plot <- function(dat) {
  dat %>% group_by(region) %>% filter(day == max(day)) %>%
    ggplot(aes(extra_R_Rd1, fill = f_multi, colour = f_multi)) +
    # geom_violin(alpha = 0.5) +
    # geom_histogram(position = "identity", alpha = 0.1) +
    geom_density(position = "identity", alpha = 0.4) +
    # facet_wrap(~region, scales = "") +
    scale_x_log10() +
    # coord_cartesian(xlim = c(5, 200)) +
    scale_colour_viridis_d() +
    scale_fill_viridis_d()
  # coord_flip()
  # scale_fill_brewer(palette = "Spectral", direction = 1) +
  # scale_colour_brewer(palette = "Spectral", direction = 1)
}

group_split(import_cases, region)

ggsave(file.path(fig_folder, "hist-imports.pdf"), width = 9, height = 9)
ggsave(file.path(fig_folder, "hist-imports.png"), width = 9, height = 9)
