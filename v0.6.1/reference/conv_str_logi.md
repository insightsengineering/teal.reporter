# Convert `yaml` representation of a boolean strings to logical Values

Converts a single `character` string representing a `yaml` boolean value
into a logical value in `R`.

## Usage

``` r
conv_str_logi(
  input,
  name = "",
  pos_logi = c("TRUE", "true", "True", "yes", "y", "Y", "on"),
  neg_logi = c("FALSE", "false", "False", "no", "n", "N", "off"),
  silent = TRUE
)
```

## Arguments

- input:

  (`character(1)`)

- name:

  (`charcter(1)`)

- pos_logi:

  (`character`) vector of `yaml` values which should be treated as
  `TRUE`.

- neg_logi:

  (`character`) vector of `yaml` values which should be treated as
  `FALSE`.

- silent:

  (`logical(1)`) if to suppress the messages and warnings.

## Value

`input` argument or the appropriate `logical` value.

## Examples

``` r
conv_str_logi <- getFromNamespace("conv_str_logi", "teal.reporter")
conv_str_logi("TRUE")
#> [1] TRUE
conv_str_logi("True")
#> [1] TRUE

conv_str_logi("off")
#> [1] FALSE
conv_str_logi("n")
#> [1] FALSE

conv_str_logi("sth")
#> [1] "sth"
```
