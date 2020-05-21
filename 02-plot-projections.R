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
