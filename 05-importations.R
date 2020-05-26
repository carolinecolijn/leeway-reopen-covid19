source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

PROJ <- 60
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 100)
mults <- c(1.0, 1.5, 2.0) %>% set_names()
imports <- c(0, 10) %>% set_names()

future::plan(future::multisession)
projections_imp <-
  map_dfr(mults, function(.mult) {
    cat(.mult, "\n")
    furrr::future_map2_dfr(fits, observed_data, function(.fit, .obs) {
      map_dfr(imports, function(.import) {
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
    }, .id = "region")
  }, .id = "f_multi")
future::plan(future::sequential)
saveRDS(projections_imp, file = file.path(dg_folder, "projections-multi-imp.rds"))
projections_imp <- readRDS(file.path(dg_folder, "projections-multi-imp.rds"))

import_cases <- projections_imp %>%
  dplyr::filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration) %>%
  dplyr::filter(time == max(time)) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"]) %>%
  group_by(f_multi, region, .iteration) %>%
  summarise(new_cases = cases[import == "10"] - cases[import == "0"])

import_cases %>%
  ggplot(aes(new_cases, fill = f_multi)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(~region, scales = "free_x") +
  scale_x_log10()

ggsave(file.path(fig_folder, "hist-imports.pdf"), width = 9, height = 9)
ggsave(file.path(fig_folder, "hist-imports.png"), width = 9, height = 9)
