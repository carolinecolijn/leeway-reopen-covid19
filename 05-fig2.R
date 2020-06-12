source("analysis/model-prep.R")
source("analysis/projection-prep.R")

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
          last_cases = y_rep[day == max(day) - 14],
          above_hist_thresh = last_cases > max_hist
        )
    }, .id = "region")
  }, .id = "f_multi") %>%
  mutate(region = ifelse(region == "SWE", "SE", region)) %>%
  group_by(f_multi, region) %>%
  left_join(N, by = "region") %>%
  summarise(
    p_above_hist_thresh = mean(above_hist_thresh, na.rm = TRUE),
    p_above_1_1000_N = mean(last_cases > N / 1000),
    p_above_1_5000_N = mean(last_cases > N / 5000),
    p_above_1_10000_N = mean(last_cases > N / 10000),
    p_above_1_20000_N = mean(last_cases > N / 20000)
  ) %>%
  ungroup() %>%
  mutate(f_multi = as.numeric(f_multi)) %>%
  left_join(country_lookup)

# check: --------------------------------------------------------------

# bc <- projections_fan[["1.8"]][["BC"]]
# bc_summ <- group_by(bc, .iteration) %>%
#   summarise(
#     max_hist = max(y_rep[!forecast]),
#     last_cases = y_rep[day == max(day)],
#     above_hist_thresh = last_cases > max_hist
#   )
# plots <- ggplot(bc, aes(day, y_rep)) + geom_line() +
#   facet_wrap(~.iteration) +
#   geom_hline(aes(yintercept = last_cases, colour = above_hist_thresh), data = bc_summ) +
#   geom_hline(aes(yintercept = max_hist), data = bc_summ) +
#   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
#   ggsidekick::theme_sleek()
# ggsave(file.path(fig_folder, "bc-hist-thresh-check.pdf"), width = 10, height = 10)
# ggsave(file.path(fig_folder, "bc-hist-thresh-check.png"), width = 10, height = 10)

# plot probability of various thresholds: -----------------------------

hist_thresh_long <- hist_thresh %>%
  tidyr::pivot_longer(c(-f_multi, -region, -region_long, -country,
  -region_group)) %>%
  filter(name %in% c("p_above_1_20000_N", "p_above_hist_thresh")) %>%
  mutate(name = ifelse(name == "p_above_1_1000_N", "P(cases > 1/1,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_1_5000_N", "P(cases > N/5,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_1_10000_N", "P(cases > 1/10,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_1_20000_N", "P(cases > N/20,000)", name)) %>%
  mutate(name = ifelse(name == "p_above_hist_thresh", "P(cases > historical peak)", name))

hist_thresh_long$group <- ifelse(hist_thresh_long$country %in% c("US", "CAN"), "North America", "Other")

cols <- hist_thresh_long %>%
  group_by(region) %>%
  summarise(group = group[1]) %>%
  group_by(group) %>%
  mutate(col = RColorBrewer::brewer.pal(8, "Dark2")[1:n()])
cols <- cols$col %>% set_names(cols$region)

make_plot <- function(dat) {
  dat %>%
    ggplot(aes(f_multi, value, colour = region)) +
    annotate("rect", xmin = 2, xmax = 2.4, ymin = 0,
      ymax = Inf, fill = "grey50", alpha = 0.1) +
    geom_line() +
    facet_grid(rows = vars(name)) +
    ggsidekick::theme_sleek() +
    ggrepel::geom_text_repel(data = filter(dat, f_multi == 2),
      mapping = aes(x = f_multi, label = region), hjust = 0,
      direction = "y", nudge_x = 0.08,
      segment.colour = "grey65", segment.alpha = 0.7, segment.size = 0.3,
      size = 2.75, min.segment.length = 0) +
    theme(legend.position = "none", panel.spacing.y = unit(9, "pt")) +
    scale_color_manual(values = cols) +
    coord_cartesian(expand = FALSE, xlim = c(1, 2.25), ylim = c(-0.01, 1.01)) +
    scale_x_continuous(breaks = unique(hist_thresh_long$f_multi)) +
    xlab("Contact rate increase")+ylab("Probability")# +
    # theme(strip.text.x = element_blank())
}

hist_thresh_long <- hist_thresh_long %>% group_by(region) %>%
  mutate(max_prob = max(value, na.rm = TRUE)) %>%
  filter(max_prob > 0)
# g1 <- make_plot(filter(hist_thresh_long, group == "North America"))
g1 <- make_plot(filter(hist_thresh_long, region %in% c("CA", "WA", "ON", "SE")))
g1 <- g1 + theme(strip.text.y = element_blank()) + xlab("") #+
  # theme(axis.title.x = element_blank())
# g1

# g2 <- make_plot(filter(hist_thresh_long, group == "Other")) +
g2 <- make_plot(filter(hist_thresh_long, !region %in% c("CA", "WA", "ON", "SE"))) +
  coord_cartesian(expand = FALSE, xlim = c(1, 2.25), ylim = c(0, 0.5)) +
  theme(axis.title.y = element_blank()) + xlab("")
# g2

g <- cowplot::plot_grid(g1, g2) +
  # plot.margin = margin(t = 1, r = 1, b = 10, l = 1) +
  cowplot::draw_text("Contact rate increase", x = 0.5, y = 0.04, col = "grey30", size = 11)
# g

.x1 <- 0.125
.x2 <- 0.578
.y1 <- 0.48
.y2 <- 0.93

g <- g + cowplot::draw_text(
  c("A", "B", "C", "D"),
  x = c(.x1, .x2, .x1, .x2),
  y = c(.y2, .y2, .y1, .y1),
  hjust = 0, vjust = 0, fontface = "bold", size = 12, colour = "grey10"
)

ggsave(file.path(fig_folder, "f-mult-ref-probs.pdf"), width = 5, height = 3.75)
ggsave(file.path(fig_folder, "f-mult-ref-probs.png"), width = 5, height = 3.75, dpi = 400)
