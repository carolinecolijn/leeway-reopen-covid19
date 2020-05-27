source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")

projections_fan <- readRDS(file.path(dg_folder, "projections-multi-fan.rds"))
f1_vs_f2 <- readRDS(file.path(dg_folder, "f1_vs_f2.rds"))

# Risk calcs: ---------------------------------------------------------

N <- map_dfr(fits, ~tibble(N = .x$stan_data$x_r[["N"]]), .id = "region")

hist_thresh <-
  map_dfr(projections_fan, function(x1) {
    map_dfr(x1, function(x2) {
      group_by(x2, .iteration) %>%
        summarise(
          max_hist = max(y_rep[!forecast]),
          max_60 = y_rep[day == max(day)],
          above_hist_thresh = max_60 > max_hist
        )
    }, .id = "region")
  }, .id = "f_multi") %>%
  group_by(f_multi, region) %>%
  left_join(N, by = "region") %>%
  summarise(
    p_above_hist_thresh = mean(above_hist_thresh),
    p_above_1_1000_N = mean(max_60 > N / 1000),
    p_above_1_5000_N = mean(max_60 > N / 5000),
    p_above_1_10000_N = mean(max_60 > N / 10000)
  ) %>%
  ungroup() %>%
  mutate(f_multi = as.numeric(f_multi)) %>%
  left_join(country_lookup)

# check: --------------------------------------------------------------

# bc <- projections_fan[["1.8"]][["BC"]]
# bc_summ <- group_by(bc, .iteration) %>%
#   summarise(
#     max_hist = max(y_rep[!forecast]),
#     max_60 = y_rep[day == max(day)],
#     above_hist_thresh = max_60 > max_hist
#   )
# plots <- ggplot(bc, aes(day, y_rep)) + geom_line() +
#   facet_wrap(~.iteration) +
#   geom_hline(aes(yintercept = max_60, colour = above_hist_thresh), data = bc_summ) +
#   geom_hline(aes(yintercept = max_hist), data = bc_summ) +
#   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
#   ggsidekick::theme_sleek()
# ggsave(file.path(fig_folder, "bc-hist-thresh-check.pdf"), width = 10, height = 10)
# ggsave(file.path(fig_folder, "bc-hist-thresh-check.png"), width = 10, height = 10)

# plot probability of various thresholds: -----------------------------

hist_thresh_long <- hist_thresh %>%
  tidyr::pivot_longer(c(-f_multi, -region, -region_long, -country,
  -region_group)) %>%
  filter(name %in% c("p_above_1_5000_N", "p_above_hist_thresh")) %>%
  mutate(name = ifelse(name == "p_above_1_1000_N", "P(cases > 1/1,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_1_5000_N", "P(cases > 1/5,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_1_10000_N", "P(cases > 1/10,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_hist_thresh", "P(cases > historical peak)", name))

hist_thresh_long$group <- ifelse(hist_thresh_long$country %in% c("US", "CAN"), "North America", "Other")

cols <- hist_thresh_long %>%
  group_by(region) %>%
  summarise(group = group[1]) %>%
  group_by(group) %>%
  mutate(col = RColorBrewer::brewer.pal(8, "Dark2")[1:n()])
cols <- cols$col %>% set_names(cols$region)

g <- hist_thresh_long %>%
  ggplot(aes(f_multi, value, colour = region)) +
  annotate("rect", xmin = 2, xmax = 2.4, ymin = 0,
    ymax = Inf, fill = "grey50", alpha = 0.1) +
  geom_line() +
  facet_grid(name~group) +
  ggsidekick::theme_sleek() +
  ggrepel::geom_text_repel(data = filter(hist_thresh_long, f_multi == 2),
    mapping = aes(x = f_multi, label = region), hjust = 0,
    direction = "y", nudge_x = 0.08,
    segment.colour = "grey65", segment.alpha = 0.7, segment.size = 0.3,
    size = 2.75) +
  theme(legend.position = "none", panel.spacing.y = unit(9, "pt")) +
  scale_color_manual(values = cols) +
  coord_cartesian(expand = FALSE, xlim = c(1, 2.25), ylim = c(-0.01, 1.01)) +
  scale_x_continuous(breaks = unique(hist_thresh_long$f_multi)) +
  xlab("Contact rate increase")+ylab("Probability") +
  theme(strip.text.x = element_blank())
# g

ggsave(file.path(fig_folder, "f-mult-ref-probs.pdf"), width = 5, height = 3.75)
ggsave(file.path(fig_folder, "f-mult-ref-probs.png"), width = 5, height = 3.75, dpi = 400)

hist_thresh_long %>%
  ggplot(aes(region, value, colour = as.factor(f_multi))) +
  geom_point(position = position_dodge(width = 0.5)) +
  ggsidekick::theme_sleek() +
  facet_wrap(~name) +
  scale_colour_viridis_d() +
  coord_flip()
