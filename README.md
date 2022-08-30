# teal.reporter

`teal.reporter` enables `shiny` and `teal` applications to include reporting functionality.
The `shiny` app developer (or `teal` module developer) specifies the content of the report and the `teal.reporter` package handles everything else.

This package provides:

* A `shiny` module for adding cards (i.e. items) into a report
* A `shiny` module for previewing the report on-screen
* The ability to download a zip file containing the reports

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.reporter@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.reporter")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.reporter.svg)](https://starchart.cc/insightsengineering/teal.reporter)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.reporter](https://reporoster.com/stars/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.reporter](https://reporoster.com/forks/insightsengineering/teal.reporter)](https://github.com/insightsengineering/teal.reporter/network/members)
