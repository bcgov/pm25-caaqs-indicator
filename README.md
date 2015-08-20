<div id="devex-badge">
<a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="http://bcdevexchange.org/badge/3.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>
</div>

---

# Fine Particulate Matter CAAQS Analysis for B.C. (2011-2013)

A set of R scripts to calculate the Canadian Ambient Air Quality Standards (CAAQS) for fine particulate matter (PM<sub>2.5</sub>) for 2011-2013. These scripts reproduce the 2015 analysis presented on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/air/fine_pm.html).

This analysis makes use of the [rcaaqs](https://github.com/bcgov/rcaaqs) package, and [air quality monitoring data](http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-verified-hourly-data-and-station-data) from the Ministry of Enviornment.

### Usage

There are four core scripts that are required for the analysis, they need to be run in order:

- `01_load.R` - downloads the data from DataBC
- `02_clean.R` - cleans and prepares data for analysis
- `03_analysis.R` - performs the analysis
- `04_output.R` - creates maps and graphs and saves outputs

The `run_all.R` script can be `source`ed to run it all at once.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/<repo-name>/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

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
