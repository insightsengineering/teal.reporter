# `ReportCard`: An `R6` class for building report elements

**\[deprecated\]**

This `R6` class that supports creating a report card containing text,
plot, table and metadata blocks that can be appended and rendered to
form a report output from a `shiny` app.

## Lifecycle

This class is deprecated. Use `teal_report` class instead for new
implementations. See
[`vignette("teal-report-class", "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/articles/teal-report-class.md)
for more information.

## Methods

### Public methods

- [`ReportCard$new()`](#method-ReportCard-new)

- [`ReportCard$append_table()`](#method-ReportCard-append_table)

- [`ReportCard$append_html()`](#method-ReportCard-append_html)

- [`ReportCard$append_plot()`](#method-ReportCard-append_plot)

- [`ReportCard$append_text()`](#method-ReportCard-append_text)

- [`ReportCard$append_rcode()`](#method-ReportCard-append_rcode)

- [`ReportCard$append_content()`](#method-ReportCard-append_content)

- [`ReportCard$get_content()`](#method-ReportCard-get_content)

- [`ReportCard$reset()`](#method-ReportCard-reset)

- [`ReportCard$get_metadata()`](#method-ReportCard-get_metadata)

- [`ReportCard$append_metadata()`](#method-ReportCard-append_metadata)

- [`ReportCard$get_name()`](#method-ReportCard-get_name)

- [`ReportCard$set_name()`](#method-ReportCard-set_name)

- [`ReportCard$set_content_names()`](#method-ReportCard-set_content_names)

- [`ReportCard$to_list()`](#method-ReportCard-to_list)

- [`ReportCard$from_list()`](#method-ReportCard-from_list)

- [`ReportCard$clone()`](#method-ReportCard-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a `ReportCard` object.

#### Usage

    ReportCard$new()

#### Returns

Object of class `ReportCard`, invisibly.

#### Examples

    card <- ReportCard$new()

------------------------------------------------------------------------

### Method `append_table()`

Appends a table to this `ReportCard`.

#### Usage

    ReportCard$append_table(table)

#### Arguments

- `table`:

  A (`data.frame` or `rtables` or `TableTree` or `ElementaryTable` or
  `listing_df`) that can be coerced into a table.

#### Returns

`self`, invisibly.

#### Examples

    card <- ReportCard$new()$append_table(iris)

------------------------------------------------------------------------

### Method `append_html()`

Appends a html content to this `ReportCard`.

#### Usage

    ReportCard$append_html(content)

#### Arguments

- `content`:

  An object that can be rendered as a HTML content.

#### Returns

`self`, invisibly.

#### Examples

    card <- ReportCard$new()$append_html(shiny::div("HTML Content"))

------------------------------------------------------------------------

### Method `append_plot()`

Appends a plot to this `ReportCard`.

#### Usage

    ReportCard$append_plot(plot, dim = NULL)

#### Arguments

- `plot`:

  (`ggplot` or `grob` or `trellis`) plot object.

- `dim`:

  (`numeric(2)`) width and height in pixels.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `append_text()`

Appends a text paragraph to this `ReportCard`.

#### Usage

    ReportCard$append_text(
      text,
      style = c("default", "header2", "header3", "verbatim")
    )

#### Arguments

- `text`:

  (`character`) The text content to add.

- `style`:

  (`character(1)`) the style of the paragraph.

#### Returns

`self`, invisibly.

#### Examples

    card <- ReportCard$new()$append_text("A paragraph of default text")

------------------------------------------------------------------------

### Method `append_rcode()`

Appends an `R` code chunk to `ReportCard`.

#### Usage

    ReportCard$append_rcode(text, ...)

#### Arguments

- `text`:

  (`character`) The `R` code to include.

- `...`:

  Additional `rmarkdown` parameters for formatting the `R` code chunk.

#### Returns

`self`, invisibly.

#### Examples

    card <- ReportCard$new()$append_rcode("2+2", echo = FALSE)

------------------------------------------------------------------------

### Method `append_content()`

Appends a generic content to this `ReportCard`.

#### Usage

    ReportCard$append_content(content)

#### Arguments

- `content`:

  (Object.)

#### Returns

`self`, invisibly.

#### Examples

    card <- ReportCard$new()$append_content(code_chunk("foo <- 2"))

------------------------------------------------------------------------

### Method `get_content()`

Get all content blocks from this `ReportCard`.

#### Usage

    ReportCard$get_content()

#### Returns

[`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
containing appended elements.

#### Examples

    card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")

    card$get_content()

------------------------------------------------------------------------

### Method `reset()`

Clears all content and metadata from `ReportCard`.

#### Usage

    ReportCard$reset()

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `get_metadata()`

Get the metadata associated with `ReportCard`.

#### Usage

    ReportCard$get_metadata()

#### Returns

`named list` list of elements.

#### Examples

    card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")

    card$get_metadata()

------------------------------------------------------------------------

### Method `append_metadata()`

Appends metadata to this `ReportCard`.

#### Usage

    ReportCard$append_metadata(key, value)

#### Arguments

- `key`:

  (`character(1)`) string specifying the metadata key.

- `value`:

  value associated with the metadata key.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `get_name()`

Get the name of the `ReportCard`.

#### Usage

    ReportCard$get_name()

#### Returns

`character` a card name.

#### Examples

    ReportCard$new()$set_name("NAME")$get_name()

------------------------------------------------------------------------

### Method `set_name()`

Set the name of the `ReportCard`.

#### Usage

    ReportCard$set_name(name)

#### Arguments

- `name`:

  (`character(1)`) a card name.

#### Returns

`self`, invisibly.

#### Examples

    ReportCard$new()$set_name("NAME")$get_name()

------------------------------------------------------------------------

### Method `set_content_names()`

Set content block names for compatibility with newer `teal_card`

#### Usage

    ReportCard$set_content_names(new_names)

#### Arguments

- `new_names`:

  (`character`) vector of new names.

------------------------------------------------------------------------

### Method `to_list()`

Convert the `ReportCard` to a list, including content and metadata.

#### Usage

    ReportCard$to_list(output_dir = lifecycle::deprecated())

#### Arguments

- `output_dir`:

  (`character`) with a path to the directory where files will be copied.

#### Returns

(`named list`) a `ReportCard` representation.

------------------------------------------------------------------------

### Method `from_list()`

Reconstructs the `ReportCard` from a list representation.

#### Usage

    ReportCard$from_list(card, output_dir = lifecycle::deprecated())

#### Arguments

- `card`:

  (`named list`) a `ReportCard` representation.

- `output_dir`:

  (`character`) with a path to the directory where a file will be
  copied.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReportCard$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(ggplot2)

card <- ReportCard$new()$append_plot(
  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
)
#> Warning: `ReportCard$new()` was deprecated in teal.reporter 0.6.0.
#> ℹ Please use `teal_card()` instead.
#> ℹ Use teal_report class instead. See vignette('teal-report-class',
#>   'teal.reporter') for more information.
#> ℹ The deprecated feature was likely used in the R6 package.
#>   Please report the issue at <https://github.com/r-lib/R6/issues>.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
library(ggplot2)

card <- ReportCard$new()$append_text("Some text")$append_plot(
  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
)$append_text("Some text")$append_metadata(key = "lm",
                  value = lm(Ozone ~ Solar.R, airquality))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
card$get_content()

#> $`5ef8c1da`
#> [1] "Some text"
#> 
#> $e343c6d5
#> 
#> $a0e636bc
#> [1] "Some text"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$lm
#> 
#> Call:
#> lm(formula = Ozone ~ Solar.R, data = airquality)
#> 
#> Coefficients:
#> (Intercept)      Solar.R  
#>     18.5987       0.1272  
#> 
#> 
card$get_metadata()
#> $lm
#> 
#> Call:
#> lm(formula = Ozone ~ Solar.R, data = airquality)
#> 
#> Coefficients:
#> (Intercept)      Solar.R  
#>     18.5987       0.1272  
#> 
#> 
library(ggplot2)

card <- ReportCard$new()$append_text("Some text")$append_plot(
  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
)$append_text("Some text")$append_metadata(key = "lm",
                  value = lm(Ozone ~ Solar.R, airquality))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
card$get_content()

#> $`70e3c325`
#> [1] "Some text"
#> 
#> $adc9fec1
#> 
#> $`5b4ae075`
#> [1] "Some text"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$lm
#> 
#> Call:
#> lm(formula = Ozone ~ Solar.R, data = airquality)
#> 
#> Coefficients:
#> (Intercept)      Solar.R  
#>     18.5987       0.1272  
#> 
#> 

card$to_list(tempdir())
#> Warning: The `output_dir` argument of `ReportCard$to_list()` is deprecated as of
#> teal.reporter 0.6.0.
#> $`70e3c325`
#> [1] "Some text"
#> 
#> $adc9fec1
#> 
#> $`5b4ae075`
#> [1] "Some text"
#> 
#> attr(,"metadata")
#> attr(,"metadata")$lm
#> 
#> Call:
#> lm(formula = Ozone ~ Solar.R, data = airquality)
#> 
#> Coefficients:
#> (Intercept)      Solar.R  
#>     18.5987       0.1272  
#> 
#> 
library(ggplot2)

card <- ReportCard$new()$append_text("Some text")$append_plot(
  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
)$append_text("Some text")$append_metadata(key = "lm",
                  value = lm(Ozone ~ Solar.R, airquality))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
card$get_content()

#> $ca10ada7
#> [1] "Some text"
#> 
#> $`0b73373b`
#> 
#> $f4de2946
#> [1] "Some text"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$lm
#> 
#> Call:
#> lm(formula = Ozone ~ Solar.R, data = airquality)
#> 
#> Coefficients:
#> (Intercept)      Solar.R  
#>     18.5987       0.1272  
#> 
#> 

ReportCard$new()$from_list(card$to_list(tempdir()), tempdir())

## ------------------------------------------------
## Method `ReportCard$new`
## ------------------------------------------------

card <- ReportCard$new()


## ------------------------------------------------
## Method `ReportCard$append_table`
## ------------------------------------------------

card <- ReportCard$new()$append_table(iris)


## ------------------------------------------------
## Method `ReportCard$append_html`
## ------------------------------------------------

card <- ReportCard$new()$append_html(shiny::div("HTML Content"))


## ------------------------------------------------
## Method `ReportCard$append_text`
## ------------------------------------------------

card <- ReportCard$new()$append_text("A paragraph of default text")


## ------------------------------------------------
## Method `ReportCard$append_rcode`
## ------------------------------------------------

card <- ReportCard$new()$append_rcode("2+2", echo = FALSE)


## ------------------------------------------------
## Method `ReportCard$append_content`
## ------------------------------------------------

card <- ReportCard$new()$append_content(code_chunk("foo <- 2"))


## ------------------------------------------------
## Method `ReportCard$get_content`
## ------------------------------------------------

card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")

card$get_content()
#> $`601f6ad5`
#> [1] "Some text"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$rc
#> [1] "a <- 2 + 2"
#> 



## ------------------------------------------------
## Method `ReportCard$get_metadata`
## ------------------------------------------------

card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")

card$get_metadata()
#> $rc
#> [1] "a <- 2 + 2"
#> 


## ------------------------------------------------
## Method `ReportCard$get_name`
## ------------------------------------------------

ReportCard$new()$set_name("NAME")$get_name()
#> [1] "NAME"

## ------------------------------------------------
## Method `ReportCard$set_name`
## ------------------------------------------------

ReportCard$new()$set_name("NAME")$get_name()
#> [1] "NAME"
```
