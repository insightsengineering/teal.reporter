# Add card button module

Provides a button to add views/cards to a report.

For more details see the vignette:
`vignette("simpleReporter", "teal.reporter")`.

## Usage

``` r
add_card_button_ui(id, label = NULL)

add_card_button_srv(id, reporter, card_fun, card_title = "")
```

## Arguments

- id:

  (`character(1)`) this `shiny` module's id.

- label:

  (`character(1)`) label of the button. By default it is empty.

- reporter:

  (`Reporter`) instance.

- card_fun:

  (`function`) which returns a
  [`ReportCard`](https://insightsengineering.github.io/teal.reporter/reference/ReportCard.md)
  instance. See `Details`.

- card_title:

  (`character(1)`) default value for the card title input field. By
  default it is empty.

## Value

`NULL`.

## Details

The `card_fun` function is designed to create a new `ReportCard`
instance and optionally customize it:

- The `teal_card` parameter allows for specifying a custom or default
  `ReportCard` instance.

- Use the `comment` parameter to add a comment to the card via
  `card$append_text()` - if `card_fun` does not have the `comment`
  parameter, then `comment` from `Add Card UI` module will be added at
  the end of the content of the card.

- The `label` parameter enables customization of the card's name and its
  content through `card$append_text()`- if `card_fun` does not have the
  `label` parameter, then card name will be set to the name passed in
  `Add Card UI` module, but no text will be added to the content of the
  `teal_card`.

This module supports using a subclass of
[`ReportCard`](https://insightsengineering.github.io/teal.reporter/reference/ReportCard.md)
for added flexibility. A subclass instance should be passed as the
default value of the `teal_card` argument in the `card_fun` function.
See below:

    CustomReportCard <- R6::R6Class(
      classname = "CustomReportCard",
      inherit = teal.reporter::ReportCard
    )

    custom_function <- function(card = CustomReportCard$new()) {
      card
    }
