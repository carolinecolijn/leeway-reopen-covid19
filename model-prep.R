library(dplyr)
library(ggplot2)
library(covidseir)
library(purrr)
library(future)

if (packageVersion("covidseir") != "0.0.0.9004") {
  stop("packageVersion('covidseir') != '0.0.0.9004'. Please install the latest version of the model:\n`devtools::install_github('seananderson/covidseir')`")
}

wd <- getwd()
if (!grepl("CoronaModelsBC$", wd)) {
  stop("Please set your working directory to the `CoronaModelsBC` directory.",
    call. = FALSE)
}

options(mc.cores = parallel::detectCores() / 2) # Stan parallel processing
dir.create("selfIsolationModel/contact-ratios/data-generated/",
  showWarnings = FALSE)
dir.create("selfIsolationModel/contact-ratios/figs/",
  showWarnings = FALSE)
dir.create("selfIsolationModel/contact-ratios/data-raw/",
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

ITER <- 400
CHAINS <- 4
SAMP_FRAC <- 0.2
i0_PRIOR <- c(log(1), 1)

goog_dat <- readr::read_csv(
  "selfIsolationModel/contact-ratios/data-generated/start-end-google.csv")
get_google_start <- function(.region, .dat) {
  .s <- dplyr::filter(goog_dat, region == .region) %>%
    pull(start_date)
  as.numeric(.s - min(.dat$date))
}

get_google_end <- function(.region, .dat) {
  .s <- dplyr::filter(goog_dat, region == .region) %>%
    pull(end_date)
  as.numeric(.s - min(.dat$date))
}
