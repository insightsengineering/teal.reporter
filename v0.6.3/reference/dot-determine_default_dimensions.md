# Determine default dimensions for report figures

Determine default dimensions for report figures

## Usage

``` r
.determine_default_dimensions(x, convert_to_inches = FALSE, dpi = 96)
```

## Arguments

- x:

  An object, typically a `recordedplot` or `ggplot`, that has an
  optional attributes `dev.width` and `dev.height` that override the
  default dims set as options `teal.reporter.dev.fig.width` and
  `teal.reporter.dev.fig.height`.

## Value

List with `width` and `height` elements.
