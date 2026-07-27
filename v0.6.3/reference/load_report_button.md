# Load `Reporter` button module

Provides a button to upload `ReporterCard`(s) to the `Reporter`.

For more information, refer to the vignette:
`vignette("simpleReporter", "teal.reporter")`.

## Usage

``` r
report_load_ui(id, label = NULL)

report_load_srv(id, reporter)
```

## Arguments

- id:

  `character(1)` this `shiny` module's id.

- label:

  (`character(1)`) label of the button. By default it is empty.

- reporter:

  [`Reporter`](https://insightsengineering.github.io/teal.reporter/reference/Reporter.md)
  instance.

## Value

[`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)

[`shiny::moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
