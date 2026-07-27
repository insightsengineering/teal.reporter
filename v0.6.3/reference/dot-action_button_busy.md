# Teal action button that is disabled while busy

Teal action button that is disabled while busy

## Usage

``` r
.action_button_busy(
  id,
  label,
  icon = NULL,
  type = "primary",
  outline = FALSE,
  additional_class = NULL
)
```

## Arguments

- id:

  (`character(1)`) the id of the button.

- label:

  (`character(1)`) the label of the button.

- icon:

  (`character(1)` or `NULL`) the name of the Bootstrap icon to be
  displayed on the button.

- type:

  One of the Bootstrap theme colors (`"primary"`, `"default"`,
  `"secondary"`, `"success"`, `"danger"`, `"warning"`, `"info"`,
  `"light"`, `"dark"`), or `NULL` to leave off the Bootstrap-specific
  button CSS classes altogether.

- additional_class:

  (`character(1)` or `NULL`) additional CSS class to be added to the
  button.

## Value

A `shiny` action button that is disabled while busy.
