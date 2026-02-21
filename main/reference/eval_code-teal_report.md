# Evaluate code in `qenv`

Evaluate code in `qenv`

## Usage

``` r
# S4 method for class 'teal_report'
eval_code(object, code, code_block_opts = list(), ...)
```

## Arguments

- object:

  (`teal_report`)

- code:

  (`character`, `language` or `expression`) code to evaluate. It is
  possible to preserve original formatting of the `code` by providing a
  `character` or an `expression` being a result of
  `parse(keep.source = TRUE)`.

- code_block_opts:

  (`list`) Additional options for the R code chunk in R Markdown.

- ...:

  ([`dots`](https://rdrr.io/r/base/dots.html)) additional arguments
  passed to future methods.

## Value

`teal_reporter` environment with the code evaluated and the outputs
added to the card or `qenv.error` if evaluation fails.

## Details

`eval_code()` evaluates given code in the `qenv` environment and appends
it to the `code` slot. Thus, if the `qenv` had been instantiated empty,
contents of the environment are always a result of the stored code.

## See also

[within.qenv](https://insightsengineering.github.io/teal.code/latest-tag/reference/within.qenv.html)

## Examples

``` r
td <- teal.data::teal_data()
td <- teal.code::eval_code(td, "iris <- iris")
tr <- as.teal_report(td)
tr <- teal.code::eval_code(tr, "a <- 1")
tr <- teal.code::eval_code(tr, "b <- 2L # with comment")
tr <- teal.code::eval_code(tr, quote(library(checkmate)))
tr <- teal.code::eval_code(tr, expression(assert_number(a)))
teal_card(tr)
#> $c8127f42
#> [1] "iris <- iris"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> $`910883e5`
#> [1] "a <- 1"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> $`71b592eb`
#> [1] "b <- 2L # with comment"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> $ee2bd366
#> [1] "library(checkmate)"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> $`0f64c888`
#> [1] "assert_number(a)"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> list()
```
