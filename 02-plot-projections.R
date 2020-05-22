source("selfIsolationModel/contact-ratios/model-prep.R")
library(purrr)

files <- list.files(this_folder, pattern = "^01-") %>% sort()
files

source(file.path(this_folder, "01-fit-british-columbia.R"))
source(file.path(this_folder, "01-fit-alberta-twopart.R"))
source(file.path(this_folder, "01-fit-ontario.R"))
source(file.path(this_folder, "01-fit-quebec.R"))
source(file.path(this_folder, "01-fit-california.R"))
source(file.path(this_folder, "01-fit-washington.R"))
source(file.path(this_folder, "01-fit-new-york.R"))
source(file.path(this_folder, "01-fit-michigan.R"))

# files <- list.files(file.path(this_folder, "data-generated"),
#   pattern = "*fit*", full.names = TRUE) %>% sort()
# files

.names <- c("AB1", "AB2", "BC", "CA", "MI", "NY", "ON", "QC", "WA")

obj_files <- paste0("selfIsolationModel/contact-ratios/data-generated/", .names, "-fit.rds")

# .names <- list.files(file.path(this_folder, "data-generated"),
#   pattern = "*fit*") %>%
#   sort() %>%
#   gsub("-fit[0-9]*\\.rds", "", .)
# .names[.names == "AB"] <- paste0(.names[.names == "AB"], c(1, 2))
# .names
# .names <- c("AB1", "AB2", "BC", "CA", "MI", "NY", "ON", "QC", "WA")

fits <- map(obj_files, readRDS) %>% set_names(.names)
fits

# dat_files <- list.files(file.path(this_folder, "data-generated"), pattern = "*dat*", full.names = TRUE)
# dat_files

dat_files <- paste0("selfIsolationModel/contact-ratios/data-generated/", .names, "-dat.rds")
dat_files

observed_data <- map(dat_files, readRDS) %>% set_names(.names) %>%
  map(select, date, day, value)
observed_data

observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- .names[[.x]]
  temp
}) %>% set_names(.names)

observed_data_orig <- observed_data

observed_data$AB2$day <- max(observed_data$AB1$day) + observed_data$AB2$day
observed_data$AB <- bind_rows(observed_data$AB1, observed_data$AB2)
observed_data$AB1 <- NULL
observed_data$AB2 <- NULL
observed_data <- map(observed_data, mutate, region = ifelse(grepl("AB", region), "AB", region))

library(future)
plan(multisession)
projections <- furrr::future_map(names(fits), function(.x) {
  covidseir::project_seir(fits[[.x]], iter = 1:50,
    forecast_days = if (.x == "AB1") 0 else 30)
}) %>% set_names(.names)

plan(sequential)

saveRDS(projections,
  file = "selfIsolationModel/contact-ratios/data-generated/all-projections.rds")
projections <- readRDS("selfIsolationModel/contact-ratios/data-generated/all-projections.rds")

projections$AB2$day <- max(projections$AB1$day) + projections$AB2$day
projections$AB <- bind_rows(projections$AB1, projections$AB2)
projections$AB1 <- NULL
projections$AB2 <- NULL

tidy_projections <- map(projections, covidseir::tidy_seir)

# order
tidy_projections <- tidy_projections %>% .[order(names(.))]
observed_data <- observed_data %>% .[order(names(.))]

plots <- map2(tidy_projections, observed_data, function(x, y) {
  covidseir::plot_projection(pred_dat = x, obs_dat = y) +
    facet_null() +
    ggtitle(unique(y$region)) +
    ggsidekick::theme_sleek()
})

cowplot::plot_grid(plotlist = plots)


############
# p <- covidseir::project_seir(
#   fits[["ON"]],
#   iter = 1:25,
#   forecast_days = 60
# )
#
# days <- length(observed_data[["ON"]]$day)
# p <- covidseir::project_seir(
#   fits[["ON"]],
#   iter = 1:25,
#   forecast_days = 30,
#   f_fixed_start = days + 1,
#   f_fixed = rep(0.64, 30)
# )
# covidseir::tidy_seir(p) %>% plot_projection(observed_data[["ON"]])
#
# p <- covidseir::project_seir(
#   fits[["ON"]],
#   iter = 1:25,
#   forecast_days = 60,
#   f_fixed_start = days + 1,
#   f_multi = rep(1.2, 60)
# )
# covidseir::tidy_seir(p) %>% plot_projection(observed_data[["ON"]])
#################

# Multiplicative projection:

# library(future)
# plan(multisession)
# projections_multi <- furrr::future_map(names(fits), function(.x) {

PROJ <- 60
ITER_PROJ <- 1:40
projections_multi <- map(names(fits), function(.x) {
  print(.x)
  if (.x == "AB1") {
    day_total <- nrow(observed_data_orig[["AB1"]]) + nrow(observed_data_orig[["AB2"]])
    print(day_total)
    AB2_days <- nrow(observed_data_orig[["AB2"]])
    .forecast_days <- AB2_days + PROJ
    .f_fixed_start <- day_total + 1
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = .forecast_days,
      f_fixed_start = .f_fixed_start,
      f_multi = rep(1.2, PROJ)
    )
  } else if (.x %in% c("AB2", "BC", "ON", "QC")) {
    days <- length(observed_data_orig[[.x]]$day)
    print(days)
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = PROJ,
      f_fixed_start = days + 1,
      f_multi = rep(1.2, PROJ)
    )
  } else {
    covidseir::project_seir(
      fits[[.x]],
      iter = ITER_PROJ,
      forecast_days = 0
    )
  }
}) %>% set_names(.names)
# plan(sequential)

saveRDS(projections_multi,
  file = "selfIsolationModel/contact-ratios/data-generated/all-projections-multi.rds")
projections_multi <- readRDS("selfIsolationModel/contact-ratios/data-generated/all-projections-multi.rds")

# check:
tidy_projections <- map(projections_multi, covidseir::tidy_seir, resample_y_rep = 0)
tidy_projections <- tidy_projections %>% .[order(names(.))]
observed_data <- observed_data %>% .[order(names(.))]
observed_data_orig <- observed_data_orig %>% .[order(names(.))]
plots <- map2(tidy_projections, observed_data_orig, function(x, y) {
  covidseir::plot_projection(pred_dat = x, obs_dat = y) +
    facet_null() +
    ggtitle(unique(y$region))
})
cowplot::plot_grid(plotlist = plots)

ab1_look_up <- tibble(date = seq(
  min(observed_data_orig[["AB1"]]$date),
  min(observed_data_orig[["AB1"]]$date) + max(projections_multi$AB1$day),
  by = "1 day"),
  day = seq(1, max(projections_multi$AB1$day) + 1))
p_ab1 <- left_join(
  projections_multi$AB1,
  ab1_look_up
)
ab2_look_up <- tibble(date = seq(
  min(observed_data_orig[["AB2"]]$date),
  min(observed_data_orig[["AB2"]]$date) + max(projections_multi$AB2$day),
  by = "1 day"),
  day = seq(1, max(projections_multi$AB2$day) + 1))
p_ab2 <- left_join(
  projections_multi$AB2,
  ab2_look_up
)
p_ab <- bind_rows(p_ab1, p_ab2) %>%
  select(-day) %>%
  # filter(.iteration == 1) %>%
  # group_by(date, .iteration) %>%
  # summarise(
  #   mu = sum(mu, na.rm = TRUE),
  #   y_rep = sum(y_rep, na.rm = TRUE)
  # ) %>%

  group_by(.iteration) %>%
  arrange(date) %>%
  mutate(data_type = 1)
p_ab <- left_join(p_ab, ab1_look_up) %>%
  select(-date)

# projections_multi$AB2$day <- max(projections_multi$AB1$day) + projections_multi$AB2$day
# projections_multi$AB <- bind_rows(projections_multi$AB1, projections_multi$AB2)
# projections_multi$AB1 <- NULL
# projections_multi$AB2 <- NULL

projections_multi$AB <- p_ab
projections_multi$AB1 <- NULL
projections_multi$AB2 <- NULL

custom_tidy_seir <- function(x, resample_y_rep = 10, data_type_names = NULL) {
  if (resample_y_rep > 0) {
    x <- purrr::map_dfr(seq_len(resample_y_rep), function(i) {
      temp <- x
      temp$y_rep <- MASS::rnegbin(length(temp$y_rep),
        temp$mu,
        theta = temp$phi
      )
      temp$.obs_iteration <- i
      temp
    })
  }

  out <- dplyr::group_by(x, data_type, day, .obs_iteration, .iteration) %>%
    summarise(y_rep = sum(y_rep, na.rm = TRUE), mu = sum(mu, na.rm = TRUE))

  out <- dplyr::group_by(out, data_type, day)
  out <- dplyr::summarise(out,
    y_rep_0.05 = stats::quantile(y_rep, probs = 0.05),
    y_rep_0.25 = stats::quantile(y_rep, probs = 0.25),
    y_rep_mean = mean(y_rep),
    y_rep_0.50 = stats::quantile(y_rep, probs = 0.50),
    y_rep_0.75 = stats::quantile(y_rep, probs = 0.75),
    y_rep_0.95 = stats::quantile(y_rep, probs = 0.95),
    mu_0.05 = stats::quantile(mu, probs = 0.05),
    mu_0.25 = stats::quantile(mu, probs = 0.25),
    mu_mean = mean(mu),
    mu_0.50 = stats::quantile(mu, probs = 0.50),
    mu_0.75 = stats::quantile(mu, probs = 0.75),
    mu_0.95 = stats::quantile(mu, probs = 0.95),
    mu_0.5 = stats::quantile(mu, probs = 0.50)
  )
  if (!is.null(data_type_names)) {
    out$data_type <- names(data_type_names[as.numeric(out$data_type)])
  }
  out
}

tidy_projections <- map(projections_multi, custom_tidy_seir, resample_y_rep = 200)

# order
tidy_projections <- tidy_projections %>% .[order(names(.))]
observed_data <- observed_data %>% .[order(names(.))]

names(tidy_projections)
names(observed_data)

custom_plot <- function(pred_dat, obs_dat, col = "#377EB8") {
  g <- ggplot(pred_dat, aes_string(x = "day")) +
    geom_ribbon(aes_string(ymin = "y_rep_0.05", ymax = "y_rep_0.95"),
      alpha = 0.2, fill = col) +
    geom_ribbon(aes_string(ymin = "y_rep_0.25", ymax = "y_rep_0.75"),
      alpha = 0.2, fill = col) +
    geom_line(aes_string(y = "y_rep_mean"), lwd = 0.9, col = col) +
    facet_wrap(~data_type) +
    coord_cartesian(expand = FALSE, xlim = range(pred_dat$day)) +
    ylab("Cases") +
    theme(axis.title.x = element_blank())
  g <- g +
    geom_line(
      data = obs_dat,
      col = "black", inherit.aes = FALSE,
      aes_string(x = "day", y = "value"),
      lwd = 0.35, alpha = 0.9
    ) +
    geom_point(
      data = obs_dat,
      col = "grey30", inherit.aes = FALSE,
      aes_string(x = "day", y = "value"),
      pch = 21, fill = "grey95", size = 1.25
    )
  g
}


plots <- map2(tidy_projections, observed_data, function(x, y) {
  obs <- y %>%
    select(-day) %>%
    rename(day = date)

  pred <- left_join(ab1_look_up, x, by = "day") %>%
    select(-day) %>%
    rename(day = date)

  custom_plot(pred_dat = pred, obs_dat = obs) +
    facet_null() +
    ggtitle(unique(y$region)) +
    ggsidekick::theme_sleek() +
    theme(axis.title.x.bottom = element_blank())
})

cowplot::plot_grid(plotlist = plots[c("BC", "AB", "ON", "QC")])
