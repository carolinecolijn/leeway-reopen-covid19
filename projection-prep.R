dg_folder <- "selfIsolationModel/contact-ratios/data-generated/"
fig_folder <- "selfIsolationModel/contact-ratios/figs/"
dir.create(dg_folder, showWarnings = FALSE)
dir.create(fig_folder, showWarnings = FALSE)
REGIONS <- c("BC", "BE", "CA", "DE", "FL", "MI", "NY", "NZ", "ON", "QC", "UK", "WA", "SWE", "JP")
REGIONS <- sort(REGIONS)
N_ITER <- CHAINS * ITER / 2
PROJ_ITER <- 100
RESAMPLE_ITER <- 100

obj_files <- paste0(dg_folder, REGIONS, "-fit.rds")
obj_files

fits <- map(obj_files, readRDS) %>% set_names(REGIONS)
# walk(fits, print)

dat_files <- paste0(dg_folder, REGIONS, "-dat.rds")
# dat_files

observed_data <- map(dat_files, readRDS) %>%
  set_names(REGIONS) %>%
  map(select, date, day, value)
observed_data <- map(seq_along(observed_data), function(.x) {
  temp <- observed_data[[.x]]
  temp$region <- REGIONS[[.x]]
  temp
}) %>% set_names(REGIONS)
# observed_data

country_lookup <- tibble::tribble(
  ~region, ~region_group,
  "BC", "CAN",
  "BE", "EUR",
  "CA", "US",
  "DE", "EUR",
  # "DK", "EUR",
  "JP", "PAC",
  "FL", "US",
  "MI", "US",
  "NY", "US",
  "NZ", "PAC",
  "ON", "CAN",
  "QC", "CAN",
  "SWE", "EUR",
  "UK", "EUR",
  "WA", "US"
)
