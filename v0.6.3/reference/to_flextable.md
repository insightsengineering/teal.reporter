# Convert content into a `flextable`

Converts supported table formats into a `flextable` for enhanced
formatting and presentation.

## Usage

``` r
to_flextable(content)
```

## Arguments

- content:

  Supported formats: `data.frame`, `rtables`, `TableTree`,
  `ElementaryTable`, `listing_df`

## Value

`flextable`.

## Details

Function merges cells with `colspan` \> 1, aligns columns to the center
and row names to the left, indents the row names by 10 times
indentation.
