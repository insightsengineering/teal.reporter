# Convert `ReporterCard`/`teal_card` content to `rmarkdown`

This is an S3 generic that is used to generate content in `rmarkdown`
format from various types of blocks in a `ReporterCard` or `teal_card`
object.

## Usage

``` r
to_rmd(block, ...)
```

## Arguments

- block:

  (`any`) content which can be represented in Rmarkdown syntax.

- ...:

  additional arguments passed to implemented methods for different
  classes.

## Value

`character(1)` containing a content or Rmarkdown document.

## Details

### Customize `to_rmd`

The methods for this S3 generic can be extended by the app developer or
even overwritten. For this a function with the name `to_rmd.<class>`
should be defined in the Global Environment or registered as an S3
method, where `<class>` is the class of the object to be converted.

For example, to override the default behavior for `code_chunk` class,
you can use:

    to_rmd.code_chunk <- function(block, ...) {
      # custom implementation
      sprintf("### A custom code chunk\n\n```{r}\n%s\n```\n", block)
    }

Alternatively, the S3 method can be registered using
`registerS3method("to_rmd", "<class>", fun)`

### Defaults

`teal.reporter` provides default `to_rmd` methods for several common
classes that returns the content in appropriate R Markdown syntax. These
include:

- `character`

- [`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md)
  objects

- `ggplot2` plots

- `data.frame`

- `flextable`

- `rtables` tables

- and others.

All of these defaults can be overridden by defining new `to_rmd.<class>`
methods. These methods are implemented internally using the helper
function `.to_rmd.<class>`.

## Examples

``` r
to_rmd(c("## This is a simple text block.", "", "With a paragraph break."))
#> [1] "## This is a simple text block." ""                               
#> [3] "With a paragraph break."        
to_rmd(code_chunk("summary(cars)"))
#> [1] "```{R}\nsummary(cars)\n```"
to_rmd(data.frame(x = 1:10, y = 21:30), folder_path = tempdir())
#> [1] "```{r echo = FALSE, eval = TRUE}\nreadRDS('/tmp/RtmppT681m/report_item_11ca6600425d.rds')\n```"

# Example with ggplot2 will create a temporary RDS file in the tempdir()
to_rmd(
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point(),
  folder_path = tempdir() # internal argument of ggplot2 method
)
#> [1] "```{r echo = FALSE, eval = TRUE, fig.width = 8.333333, fig.height = 6.250000}\nreadRDS('/tmp/RtmppT681m/report_item_11ca2166be45.rds')\n```"
```
