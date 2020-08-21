source("analysis/model-prep.R")
source("analysis/projection-prep.R")
future::plan(future::multisession)

# Critical contact: ---------------------------

set.seed(28492)
ITER <- sample(seq_len(1500), 400) # downsample for speed
thresholds <- map(fits, covidseir::get_threshold, iter = ITER) # subroutine is parallel

saveRDS(thresholds, file = file.path(dg_folder, "contact-ratio-thresholds.rds"))
thresholds <- readRDS(file.path(dg_folder, "contact-ratio-thresholds.rds"))

f1 <- map(fits, ~ .x$post$f_s[ITER, 1])
f2 <- map(fits, ~ .x$post$f_s[ITER, 2])
ratios <- pmap_dfr(list(thresholds, f1, f2),
  ~ tibble(ratio1 = ..2 / ..1, ratio2 = ..3 / ..1),
  .id = "region"
)

ratios %>%
  mutate(check1 = ratio1 < 0.8, check2 = ratio2 < 0.8) %>%
  group_by(region) %>%
  summarise(p1 = mean(check1), p2 = mean(check2)) %>%
  arrange(desc(p1))

f1_vs_f2 <- group_by(ratios, region) %>%
  summarise(f1_lower = mean(ratio1) < mean(ratio2))
saveRDS(f1_vs_f2, file = file.path(dg_folder, "f1_vs_f2.rds"))

# Projections -----------------------------

PROJ <- 8 * 7
set.seed(12893)
ITER_PROJ <- sample(seq_len(N_ITER), 200)
mults <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0)

projections_fan <- map(mults, function(.mult) {
  cat(.mult, "\n")
  # out <- furrr::future_map2(fits, observed_data, function(.fit, .obs) {
  out <- purrr::map2(fits, observed_data, function(.fit, .obs) {
    use_f1 <- f1_vs_f2$f1_lower[f1_vs_f2$region == .obs$region[1]]
    days <- nrow(.fit$daily_cases)
    covidseir::project_seir(
      .fit,
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1L,
      f_multi = rep(.mult, PROJ),
      f_multi_seg = if (use_f1) 1L else 2L
    )
  })
  map(out, mutate, f_multi = .mult)
}) %>% set_names(as.character(mults))
saveRDS(projections_fan, file = file.path(dg_folder, "projections-multi-fan.rds"))
projections_fan <- readRDS(file.path(dg_folder, "projections-multi-fan.rds"))

tidy_projections <- furrr::future_map(projections_fan, function(x) {
  map(x, function(y) {
    covidseir::tidy_seir(y, resample_y_rep = RESAMPLE_ITER)
  })
})
tidy_projections1 <- map(tidy_projections, bind_rows, .id = "region")
tidy_projections1 <- tidy_projections1 %>% bind_rows(.id = "f_multi")
tidy_projections1 <- split(tidy_projections1, tidy_projections1$region)

# Add dates:
tidy_projections2 <- map2(tidy_projections1, observed_data, function(pred, obs) {
  first_day <- min(obs$date)
  lu <- tibble(
    day = sort(unique(pred$day)),
    date = seq(first_day, first_day + length(day) - 1, by = "1 day")
  )
  left_join(pred, lu, by = "day")
})

if ("SWE" %in% names(tidy_projections2)) {
  names(tidy_projections2)[names(tidy_projections2) == "SWE"] <- "SE"
}

# Violin plots: -------------------------------------------------------

# set.seed(10)
ratios_ordered <- ratios %>%
  left_join(country_lookup) %>%
  # mutate(country = forcats::fct_shuffle(country)) %>%
  group_by(region) %>%
  mutate(mean_ratio1 = mean(ratio1), mean_ratio2 = mean(ratio2)) %>%
  mutate(ratio_used_proj = min(c(mean_ratio1, mean_ratio1))) %>%
  ungroup(region) %>%
  mutate(region_ordered = forcats::fct_reorder(region, ratio_used_proj))

cols <- RColorBrewer::brewer.pal(length(unique(ratios_ordered$country)), "Set3")
regs <- c("UK", "SE", "NZ", "JP", "CAN", "US", "BE", "DE")
stopifnot(identical(sort(regs), sort(unique(ratios_ordered$country))))
names(cols) <- regs

add_label <- function(letter, region, ymax) {
  regional_label <- country_lookup$region_long[country_lookup$region == region]
  list(
    annotate("rect",
      xmin = ymd("2020-03-01"), xmax = ymd("2020-03-05") + 62,
      ymin = ymax * 0.77, ymax = ymax, fill = "white", alpha = 1
    ),
    cowplot::draw_label(letter,
      x = ymd("2020-03-05"),
      y = ymax * .83, hjust = 0, vjust = 0, fontface = "bold", size = 12, colour = "grey10"
    ),
    cowplot::draw_label(regional_label,
      x = ymd("2020-03-05") + 13,
      y = ymax * .83, hjust = 0, vjust = 0, fontface = "plain", size = 10, colour = "grey30"
    )
  )
}


violins <- ratios_ordered %>%
  ggplot(aes(x = region_ordered, y = ratio1)) +
  geom_hline(yintercept = 1, col = "grey70", lty = 2) +
  geom_violin(aes(fill = country), colour = "grey40", lwd = 0.35, alpha = 1) +
  coord_flip(ylim = c(0, 1.4), expand = FALSE) +
  ggsidekick::theme_sleek() +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = cols) +
  # theme(axis.title.y = element_blank(), legend.position = c(0.12, 0.15)) +
  theme(axis.title.y = element_blank(), legend.position = "none") +
  labs(fill = "Region", y = "Threshold ratio")
ggsave(file.path(fig_folder, "ratio-violins-f1.pdf"), width = 3.5, height = 6, plot = violins)
violins <- violins + geom_violin(aes(fill = country, y = ratio2),
  colour = "grey40",
  lwd = 0.35, alpha = 0.35, lty = "22"
)
ggsave(file.path(fig_folder, "ratio-violins-both.pdf"), width = 3.5, height = 6, plot = violins)

violins <- ratios_ordered %>%
  ggplot(aes(x = region_ordered, y = ratio1)) +
  geom_violin(aes(fill = country, y = ratio2),
    colour = "grey40",
    lwd = 0.35, alpha = 0.35, lty = "22"
  ) +
  geom_hline(yintercept = 1, col = "grey70", lty = 2) +
  geom_violin(aes(fill = country), colour = "grey40", lwd = 0.35, alpha = 1) +
  coord_flip(ylim = c(0, 1.4), expand = FALSE) +
  ggsidekick::theme_sleek() +
  scale_fill_manual(values = cols) +
  theme(axis.title.y = element_blank(), legend.position = "none") +
  labs(fill = "Region", y = "Threshold ratio") +
  cowplot::draw_label("A",
    x = length(unique(ratios_ordered$region)) - 0.2, # coord_flip()!
    y = 0.2, hjust = 1, vjust = 0, fontface = "bold", size = 12, colour = "grey10"
  )

# Plot: ---------------------------------------------------------------

violin_order <- rev(levels(ratios_ordered$region_ordered))
stopifnot(identical(names(tidy_projections2), names(observed_data)))

plots <- pmap(
  list(fits[violin_order], tidy_projections2[violin_order], observed_data[violin_order]),
  fan_plot2
)
for (i in 1:12) {
  plots[[i]] <- plots[[i]] +
    theme(
      plot.margin = margin(t = 0, r = 0, b = -7, l = 0.5), axis.title.y.left = element_blank(),
      axis.text.y.left = element_text(size = rel(0.9), margin = margin(r = 0.25))
    ) +
    theme(axis.text.x.bottom = element_text(size = rel(0.9))) +
    add_label(
      letter = LETTERS[i + 1], violin_order[i],
      ymax = max(observed_data[violin_order][[i]]$value, na.rm = TRUE) * 2
    )
}
# non-bottom row:
for (i in 1:9) {
  plots[[i]] <- plots[[i]] +
    theme(axis.text.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
}
# non-left row:
for (i in c(2, 3, 5, 6, 8, 9, 11, 12)) {
  plots[[i]] <- plots[[i]] +
    theme(axis.title.y.left = element_blank())
}
plots[[12]] <- plots[[12]] + guides(fill = FALSE) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, lwd = 0.8), reverse = TRUE)) + theme(
  legend.position = c(0.81, 0.5),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 7),
  legend.key.size = unit(7, "pt"),
  legend.spacing.y = unit(2, "pt"), legend.background = element_rect(fill = NA, colour = NA)) +
  labs(colour = "Contact rate\nincrease", fill = "Contact rate\nincrease")
projections <- cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4) +
  theme(plot.margin = margin(t = 5, r = 6, b = 10, l = -900))
projections <- projections +
  cowplot::draw_text("Reported cases", x = -0.02, y = 0.5, angle = 90, size = 10, col = "grey30")

g1 <-
  violins + theme(
    plot.margin = margin(t = 0, r = -50, b = 65, l = 0),
    axis.text.x.bottom = element_text(size = rel(0.9)),
    axis.title.x.bottom = element_text(size = rel(0.9)),
    legend.position = c(0.5, -0.25), legend.key.size = unit(8, "pt"),
    legend.title = element_blank(),
  ) + guides(fill = guide_legend(nrow = 4, byrow = TRUE))

g <- cowplot::plot_grid(g1, projections, rel_widths = c(1, 4), align = "hv", axis = "t") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 3))

ggsave(file.path(fig_folder, "proj-fan.pdf"), width = 8.2, height = 4.3, plot = g)
ggsave(file.path(fig_folder, "proj-fan.png"), width = 8.2, height = 4.3, plot = g, dpi = 500)

future::plan(future::sequential)
