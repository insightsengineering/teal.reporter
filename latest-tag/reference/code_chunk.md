# Generate a code chunk

This function creates a `code_chunk` object that represents code to be
displayed in a report. It stores the code of any language (see `lang`
argument) and any specified chunk options (e.g., `echo`, `eval`).

## Usage

``` r
code_chunk(code, ..., lang = "R")
```

## Arguments

- code:

  (`character`) The code to be displayed in the code chunk.

- ...:

  Additional named parameters to be included as chunk options. These
  control how the chunk behaves when rendered (e.g., `echo = TRUE`,
  `eval = FALSE`, `message = FALSE`). See [`knitr`
  options](https://yihui.org/knitr/options/) for available options.

- lang:

  (`character(1)`) The language of the code chunk. Defaults to `"R"`.
  See
  [`knitr::knit_engines`](https://rdrr.io/pkg/knitr/man/knit_engines.html)
  for supported languages (e.g., "python", "bash").

## Value

An object of class `code_chunk`

## Details

**Important Notes:**

- The code is **not evaluated**; it is only stored as text with
  formatting attributes.

- When converted to output, `code_chunk` produces markdown code block
  syntax (```` ```{lang} ... ``` ````) or HTML
  `<pre><code>...</code></pre>` blocks.

- The document is **not rendered** using
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  The `code_chunk` is part of the `teal_card` API for building
  reproducible documents that are produced as-is.

**Typical Workflow:**

1.  Create a `code_chunk` object with your code and options

2.  Add it to a `teal_card` using
    [`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
    or [`c()`](https://rdrr.io/r/base/c.html)

3.  The card produces the formatted code block in the final document
    output

## See also

- [`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  for creating report cards that can contain `code_chunk` objects

## Examples

``` r
# Basic code chunk with options
code_chunk("x <- 1:10", echo = TRUE, message = FALSE)
#> [1] "x <- 1:10"
#> attr(,"params")
#> attr(,"params")$echo
#> [1] TRUE
#> 
#> attr(,"params")$message
#> [1] FALSE
#> 
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"

# Python code chunk
code_chunk("import pandas as pd", lang = "python", eval = FALSE)
#> [1] "import pandas as pd"
#> attr(,"params")
#> attr(,"params")$eval
#> [1] FALSE
#> 
#> attr(,"lang")
#> [1] "python"
#> attr(,"class")
#> [1] "code_chunk"

# Code chunk with multiple knitr options
code_chunk(
  "plot(mtcars$mpg, mtcars$hp)",
  echo = TRUE,
  eval = TRUE,
  fig.width = 7,
  fig.height = 5,
  warning = FALSE
)
#> [1] "plot(mtcars$mpg, mtcars$hp)"
#> attr(,"params")
#> attr(,"params")$echo
#> [1] TRUE
#> 
#> attr(,"params")$eval
#> [1] TRUE
#> 
#> attr(,"params")$fig.width
#> [1] 7
#> 
#> attr(,"params")$fig.height
#> [1] 5
#> 
#> attr(,"params")$warning
#> [1] FALSE
#> 
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
```
