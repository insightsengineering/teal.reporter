# Render `teal_card`

Render `teal_card`

## Usage

``` r
render(
  input,
  output_dir = getwd(),
  global_knitr = getOption("teal.reporter.global_knitr"),
  keep_rmd = TRUE,
  ...
)
```

## Arguments

- input:

  (`teal_report` or `teal_code`) object to render.

- output_dir:

  The output directory for the rendered `output_file`. This allows for a
  choice of an alternate directory to which the output file should be
  written (the default output directory of that of the input file). If a
  path is provided with a filename in `output_file` the directory
  specified here will take precedence. Please note that any directory
  path provided will create any necessary directories if they do not
  exist.

- global_knitr:

  (`list`) options to apply to every code chunk in a teal_card document.
  [Read more
  here](https://rmarkdown.rstudio.com/lesson-3.html#global-options).

- keep_rmd:

  (`logical(1)`) if `.Rmd` should be kept after rendering to desired
  `output_format`.

- ...:

  arguments passed to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Examples

``` r
report <- teal_report()
teal_card(report) <- c(
  teal_card(report),
  "## Document section",
  "Lorem ipsum dolor sit amet"
)
report <- within(report, a <- 2)
report <- within(report, plot(a))
metadata(teal_card(report)) <- list(
  title = "My Document",
  author = "NEST"
)
if (interactive()) {
  render(report, output_format = rmarkdown::pdf_document(), global_knitr = list(fig.width = 10))
}
```
