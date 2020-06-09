source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

WEEKS <- 6
PROJ <- WEEKS * 7
CASE_PER_WEEK <- 10
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 80)
mults <- seq(1, 2, 0.2) %>% set_names()
imports <- c(0, CASE_PER_WEEK * WEEKS) %>% set_names()
round_time <- seq(50, 500) # time increments to save for efficiency

f1_vs_f2 <- readRDS(file.path(dg_folder, "f1_vs_f2.rds"))

future::plan(future::multisession)
tictoc::tic()
projections_imp <-
  map_dfr(mults, function(.mult) {
    cat(.mult, "\n")
    furrr::future_map2_dfr(fits, observed_data, function(.fit, .obs) {
    # purrr::map2_dfr(fits, observed_data, function(.fit, .obs) {
      map_dfr(imports, function(.import) {
        days <- length(.obs$day)
        use_f1 <- f1_vs_f2$f1_lower[f1_vs_f2$region == .obs$region[1]]
        # be extra careful; first can make a big diff:
        .fit$stan_data$ode_control <- c(1e-09, 1e-08, 1e+08)
        x <- covidseir::project_seir(
          obj = .fit,
          iter = ITER_PROJ,
          forecast_days = PROJ,
          f_fixed_start = days + 1,
          f_multi = rep(.mult, PROJ),
          f_multi_seg = if (use_f1) 1L else 2L,
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
projections_imp <- filter(projections_imp, !region %in% c("MI", "FL"))
projections_imp <- mutate(projections_imp, region = ifelse(region == "SWE", "SE", region))

# R <- projections_imp %>%
#   filter(variable %in% c("R", "Rd")) %>%
#   filter(f_multi %in% 1.6) %>%
#   group_by(f_multi, region, import, .iteration, time) %>%
#   summarise(R_Rd = value[variable == "R"] + value[variable == "Rd"])
#
# g <- ggplot(
#   R,
#   aes(time, R_Rd, colour = import, group = paste(.iteration, import))
# ) +
#   geom_line(alpha = 0.3) +
#   facet_wrap(~region, scales = "free_y") +
#   coord_cartesian(xlim = c(60, 150)) +
#   theme_light()
# ggsave(file.path(fig_folder, "proj-R.png"), width = 12, height = 9)
#
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
    extra_R_Rd = cases[import == max(imports)] - cases[import == "0"],
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
  scale_colour_viridis_d(option = "D") +
  ylab("Extra cases") +
  coord_cartesian(expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE))

ggsave(file.path(fig_folder, "proj-new-R-Rd.png"), width = 12, height = 9)
ggsave(file.path(fig_folder, "proj-new-R-Rd.pdf"), width = 12, height = 9)

import_cases <- projections_imp %>%
  filter(variable %in% c("R", "Rd")) %>%
  group_by(f_multi, region, import, .iteration) %>%
  filter(time == max(time) - 0) %>%
  summarise(cases = value[variable == "R"] + value[variable == "Rd"]) %>%
  group_by(f_multi, region, .iteration) %>%
  summarise(extra_R_Rd = cases[import == names(imports[2])] - cases[import == "0"])

import_cases_q <- import_cases %>%
  mutate(extra_R_Rd1 = extra_R_Rd / 10) %>%
  group_by(f_multi, region) %>%
  summarise(
    q0.05 = quantile(extra_R_Rd1, probs = 0.05),
    q0.25 = quantile(extra_R_Rd1, probs = 0.25),
    q0.50 = quantile(extra_R_Rd1, probs = 0.50),
    q0.75 = quantile(extra_R_Rd1, probs = 0.75),
    q0.95 = quantile(extra_R_Rd1, probs = 0.95)
  )

.width <- 0.9
g <- import_cases_q %>%
  group_by(region) %>%
  left_join(country_lookup) %>%
  mutate(mean_cases = mean(q0.50)) %>%
  ungroup() %>%
  mutate(region_long = forcats::fct_reorder(region_long, mean_cases)) %>%
  ggplot(aes(x = region_long, colour = f_multi)) +
  geom_point(aes(y = q0.50), position = position_dodge(width = .width)) +
  geom_linerange(aes(ymin = q0.05, ymax = q0.95), position = position_dodge(width = .width), lwd = 0.4) +
  geom_linerange(aes(ymin = q0.25, ymax = q0.75), position = position_dodge(width = .width), lwd = 0.9) +
  scale_colour_viridis_d(guide = guide_legend(reverse = TRUE)) +
  scale_y_log10() +
  coord_flip(ylim = c(4, NA), expand = TRUE) +
  theme(legend.position = c(0.8, 0.185), legend.background = element_rect(fill = "#ffffff90"), legend.key.size = unit(10, "pt"), axis.title.y.left = element_blank(), axis.title.x.bottom = element_text(size = 10), axis.text.x.bottom = element_text(size = 10), axis.text.y.left = element_text(size = 8.5, angle = 0)) +
  labs(colour = "Contact rate\nincrease", y = "Extra cases after 6 weeks with\n1 pre-symptomatic infectious\ntraveller per week")

for (i in seq(2, 12, 2)) {
  g <- g + annotate("rect", xmin = i - 0.5, xmax = i + if (i < 12) 0.5 else 0.7, ymin = 2, ymax = Inf, col = NA, fill = "grey60", alpha = 0.2)
}

ggsave(file.path(fig_folder, "imports.pdf"), width = 3.5, height = 4.5)

g <- g + theme(legend.position = c(0.807, 0.19))
ggsave(file.path(fig_folder, "imports.png"), width = 3.5, height = 4.5, dpi = 400)
