# Builds `teal_card` from code and outputs in `qenv` object

Builds a `teal_card` from the code and outputs of a `teal_data` object,
preserving the order of code execution and output display.

## Usage

``` r
.code_to_card(x, code_block_opts = list())
```

## Arguments

- x:

  (`list`) object from `qenv@code`.

- code_block_opts:

  (`list`) Additional options for the R code chunk in R Markdown.

## Value

A `teal_card` built from the code and outputs in a `qenv` object.
