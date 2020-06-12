dg_folder <- "data-generated/"
fig_folder <- "figs/"
dir.create(dg_folder, showWarnings = FALSE)
dir.create(fig_folder, showWarnings = FALSE)
REGIONS <- c("BC", "BE", "CA", "DE", "NY", "NZ", "ON", "QC", "UK", "WA", "SE", "JP")
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
  ~region, ~region_group, ~region_long, ~country,
  "BC", "CAN", "British Columbia", "CAN",
  "BE", "EUR", "Belgium", "BE",
  "CA", "US",  "California", "US",
  "DE", "EUR", "Germany", "DE",
  "JP", "PAC", "Japan", "JP",
  "FL", "US",  "Florida", "US",
  "MI", "US",  "Michigan", "US",
  "NY", "US",  "New York", "US",
  "NZ", "PAC", "New Zealand", "NZ",
  "ON", "CAN", "Ontario", "CAN",
  "QC", "CAN", "Québec", "CAN",
  "SE", "EUR", "Sweden", "SE",
  "UK", "EUR", "United Kingdom", "UK",
  "WA", "US",  "Washington", "US"
)
