# Report previewer module

**\[deprecated\]**

Module offers functionalities to visualize, manipulate, and interact
with report cards that have been added to a report. It includes a
previewer interface to see the cards and options to modify the report
before downloading.

Cards are saved by the `shiny` bookmarking mechanism.

For more details see the vignette:
`vignette("previewerReporter", "teal.reporter")`.

This function is deprecated and will be removed in the next release.
Please use
[`preview_report_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)
and
[`preview_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)
to create a preview button that opens a modal with the report preview.

## Usage

``` r
reporter_previewer_ui(id)

reporter_previewer_srv(
  id,
  reporter,
  global_knitr = getOption("teal.reporter.global_knitr"),
  rmd_output = getOption("teal.reporter.rmd_output"),
  rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
  previewer_buttons = c("download", "load", "reset")
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

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

- previewer_buttons:

  (`character`) set of modules to include with
  `c("download", "load", "reset")` possible values and `"download"` is
  required. Default `c("download", "load", "reset")`

## Value

`NULL`.

## Details

To access the default values for the `global_knitr` parameter, use
`getOption('teal.reporter.global_knitr')`. These defaults include:

- `echo = TRUE`

- `tidy.opts = list(width.cutoff = 60)`

- `tidy = TRUE` if `formatR` package is installed, `FALSE` otherwise
