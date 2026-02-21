# Render the report

Render the report and zip the created directory.

## Usage

``` r
report_render_and_compress(
  reporter,
  rmd_yaml_args,
  global_knitr,
  file = tempfile()
)
```

## Arguments

- reporter:

  (`Reporter`) instance.

- rmd_yaml_args:

  (`named list`) with `Rmd` `yaml` header fields and their values.

- global_knitr:

  (`list`) a global `knitr` parameters, like echo. But if local
  parameter is set it will have priority.

- file:

  (`character(1)`) where to copy created zip file.

## Value

`file` argument, invisibly.
