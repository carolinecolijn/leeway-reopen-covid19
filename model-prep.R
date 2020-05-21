library(dplyr)
library(ggplot2)
library(covidseir)
if (packageVersion("covidseir") != "0.0.0.9002") {
  stop("Please install the latest 'est-i0' version of the model:\n`devtools::install_github('seananderson/covidseir', ref = 'est-i0')`")
}

wd <- getwd()
if (!grepl("CoronaModelsBC$", wd)) {
  stop("Please set your working directory to the `CoronaModelsBC` directory.",
    call. = FALSE)
}

options(mc.cores = parallel::detectCores() / 2) # Stan parallel processing
dir.create(here("selfIsolationModel/contact-ratios/data-generated/"),
  showWarnings = FALSE)
dir.create(here("selfIsolationModel/contact-ratios/figs/"),
  showWarnings = FALSE)
dir.create(here("selfIsolationModel/contact-ratios/data-raw/"),
  showWarnings = FALSE)

# For the threshold function; need to get it into the package:
source("selfIsolationModel/contact-ratios/functions.R")

this_folder <- "selfIsolationModel/contact-ratios/"

make_traceplot <- function(fit) {
  rstan::traceplot(fit$fit,
    pars = c("R0", "i0", "f_s", "start_decline", "end_decline", "phi", "e"))
}

ymd <- lubridate::ymd
dmy <- lubridate::dmy

ITER <- 200
CHAINS <- 4
