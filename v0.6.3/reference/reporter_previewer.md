# Show report previewer button module

**\[experimental\]** Provides a button that triggers showing the report
preview in a modal.

For more details see the vignette:
`vignette("previewerReporter", "teal.reporter")`.

## Usage

``` r
preview_report_button_ui(id, label = "Preview Report")

preview_report_button_srv(id, reporter)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- label:

  (`character(1)`) label of the button. By default it is "Preview
  Report".

- reporter:

  (`Reporter`) instance.

## Value

`NULL`.
