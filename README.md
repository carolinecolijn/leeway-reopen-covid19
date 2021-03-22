## How much leeway is there to relax COVID-19 control measures?

This repository contains code associated with the paper:
Anderson, S.C., Mulberry, N., Edwards, A.M., Stockdale, J.E., Iyaniwura, S.A., Falcao, R.C., Otterstatter, M.C., Janjua, N.Z., Coombs, D., and Colijn, C. 2021. How much leeway is there to relax COVID-19 control measures? Epidemics: 100453. doi:[10.1016/j.epidem.2021.100453](https://doi.org/10.1016/j.epidem.2021.100453).

The main statistical model is written in [Stan](https://mc-stan.org/) and fit with the package [covidseir](https://github.com/seananderson/covidseir).

The analysis can be reproduced by running the numbered R files in the [`analysis`](analysis) folder. Note that the model fits and some projections will take some time (~10--30 minutes per model and ~1 hour for all the projections). Only rerun `00-cache-data.R` and `00-mobility-segments.R` if you want to update the data. Our cached data is already available in this repository.

You will need the following packages installed:

```r
install.packages(
  c("tidyverse", "segmented", "future", "furrr", "cowplot", "remotes", "zoo")
)
remotes::install_github("seananderson/ggsidekick")
```

Follow the C++ compiler [installation instructions for rstan and covidseir](https://github.com/seananderson/covidseir) carefully first, then run:

```r
remotes::install_github("seananderson/covidseir", ref = "4de2abb")
```
