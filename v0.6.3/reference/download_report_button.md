# Download report button module

Provides a button that triggers downloading a report.

For more information, refer to the vignette:
`vignette("simpleReporter", "teal.reporter")`.

## Usage

``` r
download_report_button_ui(id, label = NULL)

download_report_button_srv(
  id,
  reporter,
  global_knitr = getOption("teal.reporter.global_knitr"),
  rmd_output = getOption("teal.reporter.rmd_output"),
  rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args")
)
```

## Arguments

- id:

  (`character(1)`) this `shiny` module's id.

- label:

  (`character(1)`) label of the button. By default it is empty.

- reporter:

  (`Reporter`) instance.

- global_knitr:

  (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`) for
  customizing the rendering process.

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
