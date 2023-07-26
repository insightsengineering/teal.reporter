# teal.reporter

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.reporter/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.reporter/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.reporter/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.reporter/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.reporter/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.reporter/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.reporter?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/teal.reporter?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.reporter)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.reporter)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.reporter)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.reporter)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.reporter)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.reporter)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.reporter/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.reporter/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.reporter?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.reporter/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

`teal.reporter` enables `shiny` and `teal` applications to include reporting functionality.
The `shiny` app developer (or `teal` module developer) specifies the content of the report and the `teal.reporter` package handles everything else.

This package provides:

* A `shiny` module for adding cards (i.e. items) into a report
* A `shiny` module for previewing the report on-screen
* The ability to download a zip file containing the reports

## Installation

From July 2023 it is recommended to install packages from publicly available repositories.

```r
# stable versions
install.packages('teal.logger', repos = c('https://insightsengineering.r-universe.dev', 'https://cloud.r-project.org'))

# beta versions
install.packages('teal.logger', repos = c('https://pharmaverse.r-universe.dev', 'https://cloud.r-project.org'))
```

See package vignettes `browseVignettes(package = "teal.reporter")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.reporter.svg)](https://starchart.cc/insightsengineering/teal.reporter)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.reporter](https://reporoster.com/stars/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.reporter](https://reporoster.com/forks/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/network/members)
