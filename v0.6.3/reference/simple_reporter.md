# Simple reporter module

Module provides compact UI and server functions for managing a report in
a `shiny` app. This module combines functionalities for [adding cards to
a
report](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md),
[downloading the
report](https://insightsengineering.github.io/teal.reporter/reference/download_report_button.md),
and [resetting report
content](https://insightsengineering.github.io/teal.reporter/reference/reset_report_button.md).

For more details see the vignette:
`vignette("simpleReporter", "teal.reporter")`.

## Usage

``` r
simple_reporter_ui(id)

simple_reporter_srv(
  id,
  reporter,
  card_fun,
  global_knitr = getOption("teal.reporter.global_knitr"),
  rmd_output = getOption("teal.reporter.rmd_output"),
  rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args")
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- reporter:

  (`Reporter`) instance.

- card_fun:

  (`reactive` or `function`) which returns a
  [`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  or
  [`ReportCard`](https://insightsengineering.github.io/teal.reporter/reference/ReportCard.md)
  instance.

- global_knitr:

  (`list`) a global `knitr` parameters for customizing the rendering
  process.

- rmd_output:

  (`character`) vector with `rmarkdown` output types, by default all
  possible `pdf_document`, `html_document`, `powerpoint_presentation`,
  and `word_document`. If vector is named then those names will appear
  in the `UI`.

- rmd_yaml_args:

  (`named list`) with `Rmd` `yaml` header fields and their default
  values. This `list` will result in the custom subset of UI inputs for
  the download reporter functionality. Default
  `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document", toc = FALSE)`.
  The `list` must include at least `"output"` field. The default value
  for `"output"` has to be in the `rmd_output` argument.

## Value

`NULL`.

## Details

To access the default values for the `global_knitr` parameter, use
`getOption('teal.reporter.global_knitr')`. These defaults include:

- `echo = TRUE`

- `tidy.opts = list(width.cutoff = 60)`

- `tidy = TRUE` if `formatR` package is installed, `FALSE` otherwise

## Examples

``` r
if (interactive()) {
  library(shiny)

  shinyApp(
    ui = fluidPage(simple_reporter_ui("simple")),
    server = function(input, output, session) {
      simple_reporter_srv("simple", Reporter$new(), function(card) card)
    }
  )
}
```
