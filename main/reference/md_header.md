# Create `markdown` header from `yaml` string

This function wraps a `yaml`-formatted string in Markdown header
delimiters.

## Usage

``` r
md_header(x)
```

## Arguments

- x:

  (`character`) `yaml` formatted string.

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
md_header <- getFromNamespace("md_header", "teal.reporter")
md_header(as.yaml(yaml))
#> [1] "---\nauthor: \"NEST\"\ntitle: \"Report\"\ndate: \"07/04/2019\"\noutput:\n  pdf_document:\n    keep_tex: yes\n---\n"
```
