# Internal helper for \`teal_card“ input conversion

Converts input values to a format compatible with `teal_card`. This
function is used internally to handle common inputs, such as `ggplot`
objects, ensuring they are appropriately converted to an "evaluable
output" blocks that can be saved to `RDS` file efficiently.

## Usage

``` r
.convert_teal_card_input(x)
```

## Arguments

- x:

  (`object`) An object to be converted.

## Value

The processed object, possibly converted or left unchanged.

## Details

This function performs the following conversions:

- `ggplot` objects are converted to `recordedplot` objects.

If the R option `teal.reporter.disable_teal_card_conversion` is set to
`TRUE`, no conversion is applied.
