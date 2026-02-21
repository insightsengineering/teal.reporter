# Extract field from `yaml` text

Parses `yaml` text, extracting the specified field. Returns list names
if it's a list; otherwise, the field itself.

## Usage

``` r
get_yaml_field(yaml_text, field_name)
```

## Arguments

- yaml_text:

  (`rmd_yaml_header` or `character`) vector containing the `yaml` text.

- field_name:

  (`character`) the name of the field to extract.

## Value

If the field is a list, it returns the names of elements in the list;
otherwise, it returns the extracted field.
