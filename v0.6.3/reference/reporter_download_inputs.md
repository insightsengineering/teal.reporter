# Get the custom list of UI inputs

Get the custom list of UI inputs

## Usage

``` r
reporter_download_inputs(rmd_yaml_args, rmd_output, showrcode, session)
```

## Arguments

- rmd_yaml_args:

  (`named list`) with `Rmd` `yaml` header fields and their default
  values. This `list` will result in the custom subset of UI inputs for
  the download reporter functionality. Default
  `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document", toc = FALSE)`.
  The `list` must include at least `"output"` field. The default value
  for `"output"` has to be in the `rmd_output` argument.

- rmd_output:

  (`character`) vector with `rmarkdown` output types, by default all
  possible `pdf_document`, `html_document`, `powerpoint_presentation`,
  and `word_document`. If vector is named then those names will appear
  in the `UI`.
