## How much leeway is there to relax COVID-19 control measures?

This repository contains code associated with a manuscript ([preprint here](https://www.medrxiv.org/content/10.1101/2020.06.12.20129833v1)) investigating how much leeway is there to relax COVID-19 control measures across a range of national and regional jurisdictions. The manuscript will be released shortly as a preprint.

The main statistical model is written in [Stan](https://mc-stan.org/) and fit with the package [covidseir](https://github.com/seananderson/covidseir).

The analysis can be reproduced by running the numbered R files in the [`analysis`](analysis) folder. Note that the model fits and some projections will take some time (~10 minutes per model and ~1 hour for all the projections). Only rerun `00-cache-data.R` and `00-mobility-segments.R` if you want to update the data. Our cached data is already available in this repository.

You will need the following packages installed:

```r
install.packages(
  c("tidyverse", "segmented", "future", "furrr", "cowplot", "remotes", "zoo")
)
remotes::install_github("seananderson/ggsidekick")
```

Follow the C++ compiler [installation instructions for rstan and covidseir](https://github.com/seananderson/covidseir) carefully first, then run:

```r
remotes::install_github("seananderson/covidseir", ref = "3fc1f8f")
```
