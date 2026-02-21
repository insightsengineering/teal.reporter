# Set metadata for a `teal_card` or `ReportCard`

This function allows you to set or modify metadata fields in a
`teal_card` or `ReportCard` object. It can be used to add new metadata
or update existing fields.

## Usage

``` r
metadata(object, which = NULL) <- value

# S3 method for class 'teal_card'
metadata(object, which = NULL) <- value

# S3 method for class 'ReportCard'
metadata(object, which) <- value
```

## Arguments

- object:

  (`teal_card` or `ReportCard`) The object to modify.

- which:

  (`character`) The name of the metadata field to set.

- value:

  The value to assign to the specified metadata field.

## Value

The modified object with updated metadata.

## Details

The `ReportCard` class only supports the `title` field in metadata.
