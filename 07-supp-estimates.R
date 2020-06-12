source("analysis/model-prep.R")
source("analysis/projection-prep.R")

posterior <- purrr::map2_df(fits, observed_data, function(x, y) {
  post_tidy <- tidybayes::gather_draws(x$fit, c(R0, i0, e, start_decline, end_decline)) %>% ungroup()
  post_tidy_f <- tidybayes::gather_draws(x$fit, f_s[f_seg]) %>%
    ungroup() %>%
    mutate(.variable = paste0(gsub("_s", "", .variable), f_seg)) %>%
    select(-f_seg)
  phi <- tidybayes::gather_draws(x$fit, phi[i]) %>%
    ungroup() %>%
    select(-i)
  bind_rows(post_tidy, post_tidy_f) %>%
    bind_rows(phi)
}, .id = "region")


m <- fits[[1]] # example; these values are all the same
getR0 <- function(R0b, fit) {
  k2 <- fit$stan_data$x_r[["k2"]]
  D <- fit$stan_data$x_r[["D"]]
  q <- fit$stan_data$x_r[["q"]]
  R0b * (  1/(q+1/D) + 1/k2 ) / ( D+1/k2)
}

posterior_nonR0 <- filter(posterior,!.variable == 'R0')
posterior_R0 <- filter(posterior,.variable == 'R0')
posterior_R0$.value <- getR0(posterior_R0$.value , m)

g <- bind_rows(posterior_nonR0, posterior_R0) %>%
  mutate(.variable = factor(.variable,
    levels = c("i0", "e", "R0", "start_decline",
    "end_decline", "f1", "f2", "phi"))) %>%
  ggplot(aes(.value)) +
  geom_histogram() +
  facet_grid(region ~ .variable, scales = "free") +
  ylab("Count") +
  xlab("Posterior parameter value") +
  coord_cartesian(expand = FALSE)

ggsave(file.path(fig_folder, "parameter-estimates.pdf"), width = 10, height = 10, plot = g)
ggsave(file.path(fig_folder, "parameter-estimates.png"), width = 10, height = 10, plot = g)
