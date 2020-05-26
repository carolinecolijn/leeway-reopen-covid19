source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

PROJ <- 60
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 100)
mults <- c(1.0, 1.5, 2.0) %>% set_names()
imports <- c(0, 10) %>% set_names()
round_time <- seq_len(500)

future::plan(future::multisession)
projections_imp <-
  map_dfr(mults, function(.mult) {
    cat(.mult, "\n")
    furrr::future_map2_dfr(fits, observed_data, function(.fit, .obs) {
      out <- map_dfr(imports, function(.import) {
        days <- length(.obs$day)
        covidseir::project_seir(
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
      }, .id = "import")
      dplyr::filter(out, time %in% round_time, variable %in% c("R", "Rd", "E2")) # save space
    }, .id = "region")
  }, .id = "f_multi")
future::plan(future::sequential)
saveRDS(projections_imp, file = file.path(dg_folder, "projections-multi-imp1.rds"))
projections_imp <- readRDS(file.path(dg_folder, "projections-multi-imp1.rds"))

R <- projections_imp %>%
  filter(time %in% round_time) %>%
  filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration, time) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"])

E2 <- projections_imp %>%
  filter(time %in% round_time) %>%
  filter(variable %in% c("E2")) %>%
  group_by(f_multi, region, import, .iteration, time) %>%
  summarise(cases = value[variable == "E2"])

E2 %>%
  filter(import == "0") %>%
  ggplot(aes(time, cases, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  theme_light()

R %>%
  filter(import == "0", region == "JP") %>%
  ggplot(aes(time, cases, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  theme_light()

R %>%
  filter(import == "10", region == "JP") %>%
  ggplot(aes(time, cases, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  theme_light()

g <- R %>%
  group_by(time, f_multi, region, .iteration) %>%
  summarise(new_cases = cases[import == "10"] - cases[import == "0"]) %>%
  ggplot(aes(time, new_cases, colour = f_multi, group = paste(.iteration, f_multi))) +
  geom_line(alpha = 0.3) +
  facet_wrap(~region, scales = "free_y") +
  theme_light() +
  ylab("New R + Rd")
ggsave(file.path(fig_folder, "proj-new-cases.png"), width = 12, height = 9)

import_cases <- projections_imp %>%
  filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration) %>%
  filter(time == max(time)) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"]) %>%
  group_by(f_multi, region, .iteration) %>%
  summarise(new_cases = cases[import == "10"] - cases[import == "0"])

import_cases %>%
  ggplot(aes(new_cases, fill = f_multi)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(~region, scales = "free_x") +
  # scale_x_log10() +
  theme_light()

ggsave(file.path(fig_folder, "hist-imports.pdf"), width = 9, height = 9)
ggsave(file.path(fig_folder, "hist-imports.png"), width = 9, height = 9)
