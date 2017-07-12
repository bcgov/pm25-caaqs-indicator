<div id="devex-badge"><a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a></div>

---

# Fine Particulate Matter CAAQS Analysis for B.C. (2014-2016)

A set of R scripts to calculate the Canadian Ambient Air Quality Standards (CAAQS) for fine particulate matter (PM<sub>2.5</sub>) for 2014-2016. These scripts reproduce the 2017 analysis presented on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/air/fine_pm.html).

This analysis makes use of the [rcaaqs](https://github.com/bcgov/rcaaqs) package, and [air quality monitoring data](http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-verified-hourly-data-and-station-data) from the B.C. Ministry of Enviornment.

### Usage

There are four core scripts that are required for the analysis, they need to be run in order:

- `01_load.R` - downloads the data from DataBC
- `02_clean.R` - cleans and prepares data for analysis
- `03_analysis.R` - performs the analysis
- `04_output.R` - creates maps and graphs and saves outputs

The `run_all.R` script can be `source`ed to run it all at once.

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils), [rcaaqs](https://github.com/bcgov/rcaaqs) and [bcmaps](https://github.com/bcgov/bcmaps) using devtools:

```r
install.packages("devtools") # If you don't already have it installed

library(devtools)
install_github("bcgov/rcaaqs")
install_github("bcgov/bcmaps")
install_github("bcgov/envreportutils")
```

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/pm25-caaqs-analysis/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2015 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.