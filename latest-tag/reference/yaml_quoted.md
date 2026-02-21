# Mark strings for quotation in `yaml` serialization

This function is designed for use with the `yaml` package to explicitly,
It adds an attribute to character strings, indicating that they should
be serialized with double quotes.

## Usage

``` r
yaml_quoted(x)
```

## Arguments

- x:

  (`character`)

## Examples

``` r
library(yaml)
yaml_quoted <- getFromNamespace("yaml_quoted", "teal.reporter")
yaml <- list(
  author = yaml_quoted("NEST"),
  title = yaml_quoted("Report"),
  date = yaml_quoted("07/04/2019"),
  output = list(pdf_document = list(keep_tex = TRUE))
)
as.yaml(yaml)
#> [1] "author: \"NEST\"\ntitle: \"Report\"\ndate: \"07/04/2019\"\noutput:\n  pdf_document:\n    keep_tex: yes\n"
```
