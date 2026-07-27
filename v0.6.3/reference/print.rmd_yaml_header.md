# Print method for the `yaml_header` class

Print method for the `yaml_header` class

## Usage

``` r
# S3 method for class 'rmd_yaml_header'
print(x, ...)
```

## Arguments

- x:

  (`rmd_yaml_header`) class object.

- ...:

  optional text.

## Value

`NULL`.

## Examples

``` r
input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
out <- as_yaml_auto(input)
out
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---
print(out)
#> ---
#> author: ''
#> output:
#>   pdf_document:
#>     toc: yes
#>     keep_tex: yes
#> ---
```
