# Access metadata from a `teal_card` or `ReportCard`

This function retrieves metadata from a `teal_card` or `ReportCard`
object. When `which` is `NULL`, it returns all metadata fields as a
list.

## Usage

``` r
metadata(object, which = NULL)

# S3 method for class 'teal_card'
metadata(object, which = NULL)

# S3 method for class 'ReportCard'
metadata(object, which = NULL)
```

## Arguments

- object:

  (`teal_card` or `ReportCard`) The object from which to extract
  metadata.

- which:

  (`character` or `NULL`) The name of the metadata field to extract.

## Value

A list of metadata fields or a specific field if `which` is provided.
