# teal.reporter

<!-- start badges -->

[![CRAN Version](https://www.r-pkg.org/badges/version/teal.reporter?color=green)](https://cran.r-project.org/package=teal.reporter)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal.reporter?color=green)](https://cran.r-project.org/package=teal.reporter)
[![Last Month Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal.reporter?color=green)](https://cran.r-project.org/package=teal.reporter)
[![Last Week Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal.reporter?color=green)](https://cran.r-project.org/package=teal.reporter)

[![Check 🛠](https://github.com/insightsengineering/teal.reporter/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.reporter/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.reporter/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.reporter/latest-tag/)
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

`teal.reporter` empowers both `shiny` and `teal` applications to seamlessly integrate reporting functionality. Developers, whether working on a `shiny` app or a `teal` module, define the report's content, while `teal.reporter` takes care of the rest.

This package provides:

* A `shiny` module for adding cards (i.e. items) into a report
* A `shiny` module for previewing the report on-screen
* The ability to download a zip file containing the reports

## Installation

```r
# stable versions
install.packages('teal.reporter')

# install.packages("pak")
pak::pak("insightsengineering/teal.reporter@*release")
```

Alternatively, you might want to use the development version available on [r-universe](https://r-universe.dev/).

```r
# beta versions
install.packages('teal.reporter', repos = c('https://pharmaverse.r-universe.dev', getOption('repos')))

# install.packages("pak")
pak::pak("insightsengineering/teal.reporter")
```

## Usage

To understand how to use this package, please refer to the [Getting Started](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/teal-reporter.html) article, which provides multiple examples of code implementation.

![Showcase](https://github.com/insightsengineering/teal.reporter/blob/main/assets/img/showcase.gif)

## Getting help

If you encounter a bug or have a feature request, please file an issue. For questions, discussions, and staying up to date, please use the `teal` channel in the [`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.reporter.svg)](https://starchart.cc/insightsengineering/teal.reporter)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.reporter](http://reporoster.com/stars/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.reporter](http://reporoster.com/forks/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/network/members)
