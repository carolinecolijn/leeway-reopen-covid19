### How much leeway is there to relax COVID-19 control measures?

This repository contains code associated with a manuscript investigating how much leeway is there to relax COVID-19 control measures across a range of national and regional jurisdictions. The manuscript will be released shortly as a preprint.

The main statistical model is written in [Stan](https://mc-stan.org/) and fit with the package [covidseir](https://github.com/seananderson/covidseir).

The analysis can be reproduced by running the numbered R files in the [`analysis`](analysis) folder. Note that the model fits and some projections will take some time.

You will need the following packages installed:

```r
install.packages(c("tidyverse", "segmented",
  "future", "furrr", "cowplot", "remotes", "zoo"))
remotes::install_github("seananderson/ggsidekick")
```

See the C++ compiler [installation instructions for covidseir](https://github.com/seananderson/covidseir) first, then:

```r
remotes::install_github("seananderson/covidseir", ref = "fc1f8f")
```
