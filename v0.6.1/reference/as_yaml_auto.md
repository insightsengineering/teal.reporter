# Parse a named list to `yaml` header for an `Rmd` file

Converts a named list into a `yaml` header for `Rmd`, handling output
types and arguments as defined in the `rmarkdown` package. This function
simplifies the process of generating `yaml` headers.

## Usage

``` r
as_yaml_auto(
  input_list,
  as_header = TRUE,
  convert_logi = TRUE,
  multi_output = FALSE,
  silent = FALSE
)
```

## Arguments

- input_list:

  (`named list`) non nested with slots names and their values compatible
  with `Rmd` `yaml` header.

- as_header:

  (`logical(1)`) optionally wrap with result with the internal
  [`md_header()`](https://insightsengineering.github.io/teal.reporter/reference/md_header.md),
  default `TRUE`.

- convert_logi:

  (`logical(1)`) convert a character values to logical, if they are
  recognized as quoted `yaml` logical values , default `TRUE`.

- multi_output:

  (`logical(1)`) multi `output` slots in the `input` argument, default
  `FALSE`.

- silent:

  (`logical(1)`) suppress messages and warnings, default `FALSE`.

## Value

`character` with `rmd_yaml_header` class, result of
[`yaml::as.yaml`](https://yaml.r-lib.org/reference/as.yaml.html),
optionally wrapped with internal
[`md_header()`](https://insightsengineering.github.io/teal.reporter/reference/md_header.md).

## Details

This function processes a non-nested (flat) named list into a `yaml`
header for an `Rmd` document. It supports all standard `Rmd` `yaml`
header fields, including `author`, `date`, `title`, `subtitle`,
`abstract`, `keywords`, `subject`, `description`, `category`, and
`lang`. Additionally, it handles `output` field types and arguments as
defined in the `rmarkdown` package.

## Note

Only non-nested lists are automatically parsed. Nested lists require
direct processing with
[`yaml::as.yaml`](https://yaml.r-lib.org/reference/as.yaml.html).

## Examples

``` r
# nested so using yaml::as.yaml directly
as_yaml_auto(
  list(author = "", output = list(pdf_document = list(toc = TRUE)))
)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#> ---

# auto parsing for a flat list, like shiny input
input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
as_yaml_auto(input)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---

as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "TRUE"))
#> The 'TRUE' value should be a logical, so it is automatically converted.
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---

as_yaml_auto(list(
  author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE,
  wrong = 2
))
#> Warning: Not recognized and skipped arguments: wrong
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---

as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = 2),
  silent = TRUE
)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: 2.0
#> ---

input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "True")
as_yaml_auto(input)
#> The 'True' value should be a logical, so it is automatically converted.
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---
as_yaml_auto(input, convert_logi = TRUE, silent = TRUE)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---
as_yaml_auto(input, silent = TRUE)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---
as_yaml_auto(input, convert_logi = FALSE, silent = TRUE)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: 'True'
#> ---

as_yaml_auto(
  list(
    author = "", output = "pdf_document",
    output = "html_document", toc = TRUE, keep_tex = TRUE
  ),
  multi_output = TRUE
)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#>   html_document:
#>     toc: yes
#> ---
as_yaml_auto(
  list(
    author = "", output = "pdf_document",
    output = "html_document", toc = "True", keep_tex = TRUE
  ),
  multi_output = TRUE
)
#> The 'True' value should be a logical, so it is automatically converted.
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#>   html_document:
#>     toc: yes
#> ---
```
