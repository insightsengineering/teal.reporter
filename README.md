# teal.reporter

`teal.reporter` enables `shiny` and `teal` applications to include reporting functionality.
The `shiny` app developer (or `teal` module developer) specifies the content of the report and the `teal.reporter` package handles everything else.

This package provides:

* A `shiny` module for adding cards (i.e. items) into a report 
* A `shiny` module for previewing the report on-screen
* The ability to download a zip file containing the reports

## Installation

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.reporter@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "teal.reporter")` for usage of this package.
