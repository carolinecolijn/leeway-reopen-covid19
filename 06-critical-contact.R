source("selfIsolationModel/contact-ratios/model-prep.R")
source("selfIsolationModel/contact-ratios/projection-prep.R")
future::plan(future::multisession)

ITER <- 1:300 # downsample for speed
thresholds <- map(fits, get_thresh, iter = ITER) # subroutine is parallel
future::plan(future::sequential)
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

# Violin plots: -------------------------------------------------------------

set.seed(1)
g <- ratios %>%
  left_join(country_lookup) %>%
  mutate(region_group = forcats::fct_shuffle(region_group)) %>%
  group_by(region) %>%
  mutate(mean_ratio = mean(ratio)) %>%
  ungroup() %>%
  ggplot(aes(x = forcats::fct_reorder(region, -mean_ratio), y = ratio)) +
  geom_hline(yintercept = 1, alpha = 0.4) +
  geom_violin(aes(fill = region_group), colour = "grey40", lwd = 0.35) +
  coord_flip(ylim = c(0, 1.4), expand = FALSE) +
  ggsidekick::theme_sleek() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y = element_blank(), legend.position = c(0.12, 0.15)) +
  labs(fill = "Region", y = "Threshold ratio")
ggsave(file.path(fig_folder, "ratio-violins.pdf"), width = 4, height = 5)
ggsave(file.path(fig_folder, "ratio-violins.png"), width = 4, height = 5)
