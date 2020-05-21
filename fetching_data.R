# Load some packages, functions, and global variables:
source(here::here("selfIsolationModel/contact-ratios/model-prep.R"))

# script to fetch Raw data and save copy on data-raw folder

## UK
location <- "data-raw/UK.csv"
linkRaw <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-totals-uk.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, here(this_folder, location))

## USA
location <- "data-raw/US.csv"
linkRaw <- "https://covidtracking.com/api/v1/states/daily.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, here(this_folder, location))

## CAN
location <- "data-raw/CAN.csv"
linkRaw <- "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, here(this_folder, location))

## BE
location <- "data-raw/BE.csv"
linkRaw <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, here(this_folder, location))

## EURO
linkRaw <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-20.xlsx"
data <- rio::import(linkRaw)
location <- "data-raw/EURO.csv"
readr::write_csv(data, here(this_folder, location))
