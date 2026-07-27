# Divide text block into smaller blocks

Split a text block into smaller blocks with a specified number of lines.

## Usage

``` r
split_text_block(x, n)
```

## Arguments

- x:

  (`character`) string containing the input block of text

- n:

  (`integer`) number of lines per block

## Value

List of character strings with up to `n` lines in each element.

## Details

A single character string containing a text block of multiple lines
(separated by `\n`) is split into multiple strings with n or less lines
each.
