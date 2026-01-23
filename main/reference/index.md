# Package index

## `shiny` modules for adding content to reports

- [`add_card_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)
  [`add_card_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)
  : Add card button module

## `shiny` modules for viewing and downloading reports

- [`simple_reporter_ui()`](https://insightsengineering.github.io/teal.reporter/reference/simple_reporter.md)
  [`simple_reporter_srv()`](https://insightsengineering.github.io/teal.reporter/reference/simple_reporter.md)
  : Simple reporter module

## Supporting `shiny` modules

- [`download_report_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/download_report_button.md)
  [`download_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/download_report_button.md)
  : Download report button module

- [`reset_report_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/reset_report_button.md)
  [`reset_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/reset_report_button.md)
  : Reset report button module

- [`report_load_ui()`](https://insightsengineering.github.io/teal.reporter/reference/load_report_button.md)
  [`report_load_srv()`](https://insightsengineering.github.io/teal.reporter/reference/load_report_button.md)
  :

  Load `Reporter` button module

- [`ui_editor_block()`](https://insightsengineering.github.io/teal.reporter/reference/srv_editor_block.md)
  [`srv_editor_block()`](https://insightsengineering.github.io/teal.reporter/reference/srv_editor_block.md)
  : UI and Server functions for editing report document blocks

- [`add_card_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)
  [`add_card_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)
  : Add card button module

- [`preview_report_button_ui()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)
  [`preview_report_button_srv()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)
  **\[experimental\]** : Show report previewer button module

## `yaml` and rmd utility functions

- [`to_rmd()`](https://insightsengineering.github.io/teal.reporter/reference/to_rmd.md)
  :

  Convert `ReporterCard`/`teal_card` content to `rmarkdown`

- [`as_yaml_auto()`](https://insightsengineering.github.io/teal.reporter/reference/as_yaml_auto.md)
  :

  Parse a named list to `yaml` header for an `Rmd` file

- [`print(`*`<rmd_yaml_header>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/print.rmd_yaml_header.md)
  :

  Print method for the `yaml_header` class

- [`rmd_output_arguments()`](https://insightsengineering.github.io/teal.reporter/reference/rmd_output_arguments.md)
  :

  Get document output arguments from the `rmarkdown` package

- [`rmd_outputs()`](https://insightsengineering.github.io/teal.reporter/reference/rmd_outputs.md)
  :

  Get document output types from the `rmarkdown` package

## Classes used inside package

- [`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`` `teal_card<-`() ``](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`as.teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`c(`*`<teal_card>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`` `[`( ``*`<teal_card>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  **\[experimental\]** :

  `teal_card`: An `S3` class for managing `teal` reports

- [`teal_report()`](https://insightsengineering.github.io/teal.reporter/reference/teal_report.md)
  [`as.teal_report()`](https://insightsengineering.github.io/teal.reporter/reference/teal_report.md)
  **\[stable\]** :

  Comprehensive data integration function for `teal` applications

- [`ReportCard`](https://insightsengineering.github.io/teal.reporter/reference/ReportCard.md)
  **\[deprecated\]** :

  `ReportCard`: An `R6` class for building report elements

- [`Reporter`](https://insightsengineering.github.io/teal.reporter/reference/Reporter.md)
  :

  `Reporter`: An `R6` class for managing reports

## Utility functions for `teal_card` object

- [`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`` `teal_card<-`() ``](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`as.teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`c(`*`<teal_card>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  [`` `[`( ``*`<teal_card>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  **\[experimental\]** :

  `teal_card`: An `S3` class for managing `teal` reports

- [`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md)
  : Generate a code chunk

- [`metadata()`](https://insightsengineering.github.io/teal.reporter/reference/metadata.md)
  :

  Access metadata from a `teal_card` or `ReportCard`

- [`` `metadata<-`() ``](https://insightsengineering.github.io/teal.reporter/reference/metadata-set.md)
  :

  Set metadata for a `teal_card` or `ReportCard`

- [`render()`](https://insightsengineering.github.io/teal.reporter/reference/render.md)
  :

  Render `teal_card`

- [`toHTML(`*`<default>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/toHTML.default.md)
  **\[experimental\]** : Convert report objects to HTML

## Utility functions for `teal_report` object

- [`c(`*`<teal_report>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/c.teal_report.md)
  :

  Concatenate `teal_report` objects

- [`eval_code(`*`<teal_report>`*`)`](https://insightsengineering.github.io/teal.reporter/reference/eval_code-teal_report.md)
  :

  Evaluate code in `qenv`

## Deprecated functions

- [`reporter_previewer_ui()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer_deprecated.md)
  [`reporter_previewer_srv()`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer_deprecated.md)
  **\[deprecated\]** : Report previewer module
