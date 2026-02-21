# Reset report button module

Provides a button that triggers resetting the report content.

For more information, refer to the vignette:
`vignette("simpleReporter", "teal.reporter")`.

## Usage

``` r
reset_report_button_ui(id, label = NULL)

reset_report_button_srv(id, reporter)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- label:

  (`character(1)`) label of the button. By default `NULL`.

- reporter:

  (`Reporter`) instance.

## Value

`NULL`.
