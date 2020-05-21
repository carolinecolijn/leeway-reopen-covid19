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

files <- list.files(file.path(this_folder, "data-generated"), pattern = "*fit*", full.names = TRUE)
files
.names <- list.files(file.path(this_folder, "data-generated"), pattern = "*fit*") %>%
  gsub("-fit[0-9]*\\.rds", "", .)
.names[.names == "AB"] <- paste0(.names[.names == "AB"], c(1, 2))
fits <- map(files, readRDS) %>% set_names(.names)
fits

dat_files <- list.files(file.path(this_folder, "data-generated"), pattern = "*dat*", full.names = TRUE)
dat_files
.names <- list.files(file.path(this_folder, "data-generated"), pattern = "*dat*") %>%
  gsub("-dat[0-9]*\\.rds", "", .)
.names
.names[.names == "AB"] <- paste0(.names[.names == "AB"], c(1, 2))

observed_data <- map(dat_files, readRDS) %>% set_names(.names)
observed_data


projections <- map(fits, covidseir::project_seir, iter = 1:50, forecast_days = 30)

tidy_projections <- map(projections, covidseir::tidy_seir)

plots <- map(tidy_projections, covidseir::plot_projection, obs_dat = dat)
