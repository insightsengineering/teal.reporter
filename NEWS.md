# teal.reporter 0.5.0.9017

### New features

* Adds editing capabilities to report cards in the previewer modal for the title and text contents.
* Adds `teal_card` class that replaces `ReportCard` R6 class. It retains all its functionalities in a S4 with less complexity. It allows for easier modifications and insertion of new report content.
* Adds `teal_report` class that extends from `teal.data::teal_data` that allows to keep contents of the report card.
* `teal_report` is updated automatically with executed code and its outputs are stored as well.
* Supports `flextable` and `gtsummary` objects.
* Soft deprecated `ReportCard`.

# teal.reporter 0.5.0

### Breaking changes

* The functions `reporter_previewer_ui()` and `reporter_previewer_srv()` are deprecated and will be removed in a future release. Please migrate to using the underlying shiny modules independently:
  - **UI**: `report_load_ui()`, `download_report_button_ui()`, `reset_report_button_ui()`, and `preview_report_button_ui()`
  - **Server**: `report_load_srv()`, `download_report_button_srv()`, `reset_report_button_srv()`, and `preview_report_button_srv()`

### Enhancements

* Moves `finalize()` methods to private in R6 classes.
* Improved the layout and appearance of the reporter with new UI design.
* Added `preview_report_button_ui` and `preview_report_button_srv` to create a shiny module that creates a button to open the report previewer in a modal.
* Improve error message when reporter zip file is not named correctly (#365)

# teal.reporter 0.4.0

### Enhancements

* Reports can now be reloaded. A zip file containing the report can be uploaded to restore the state of the Previewer.
* Report cards are now included in bookmarks. When using the `shiny` bookmarking mechanism, existing report cards will be available in the restored application.
* HTML content can now be added to the report.

# teal.reporter 0.3.1

### Enhancements

* Added blocking of "Download" buttons while report is rendering, using the `shinybusy` package.

# teal.reporter 0.3.0

### Enhancements

* `add_card_button_srv` allows to specify `card_fun` with `label` parameter for card's title & content customization.
* Supports automatic `Rcode` formatting using the suggested `formatR` package in reports.
* Improve output on PDF file format.

# teal.reporter 0.2.1

### Miscellaneous

* Specify minimal version of dependent packages.
* Updated `TableBlock` to convert tables into `flextables` to show in rendered report.
* Fixed CRAN requirements for the first CRAN submission.
* Removed manual pages for non-exported objects.
* Fixed CRAN requirements for the first CRAN submission.

# teal.reporter 0.2.0

### New features

* Added the new `RcodeBlock` block for a custom `rmarkdown` r chunk.
* Added the collapsible panel for the Show R Code in the previewer.
* Added additional input when needed to decide if "Show R Code" should be part of the rendered report.
* Added the possibility to add a personalized card name when adding a card.
* Added support for custom inputs for download and previewer modules.

### Breaking changes

* Simplified `reporter_previewer_ui` to contain just `id` argument.

### Enhancements

* Changed modal button colors to align with their purpose and with other modals.
* Updated the `AddCardModule` comment input to have an active cursor when adding a card.
* Updated report previewer to support preview of multiline comment.
* Added support for a table of contents for reporter documents.
* Added support for global `knitr` options in the render method in the Render class.
* Improved look of the remove card modal in the previewer module.

### Miscellaneous
* Updated `append_src` method of `TealReportCard` to not add additional "R Code" Subtitle.

# teal.reporter 0.1.1

### New features
* Added the `Archiver` class which enables saving and loading back the `Report` in the `shiny` session.

### Breaking changes
* Updated `append_fs` method in the `TealReportCard` to accept a `list` object.

### Enhancements
* Added support for the `ElementaryTree` class in the `append_table` method of `ReportCard`.
* Added additional validation for the `card_fun` evaluation.
* Added support for more arguments setup for a `card_fun` function in the `add_card_button_srv` module, the `card_fun` could have any subset of the possible arguments now.
* Added the optional `dim` argument to the `append_plot` method in `ReportCard`.
* Added support for `Heatmap` plots in `PictureBlock`.
* Updated `append_encodings`, `append_src` and `append_fs` to automatically add titles.
* Updated vignettes and README content.
* Made the document type names more user friendly when downloading the report.
* Improved the add reporter card button to be disabled when clicked.

### Bug fixes
* Fixed how `trellis` plots are caught by the `set_content` method in the `PictureBlock`.

### Miscellaneous
* Added `to_list` and `from_list` methods to all content related classes.
* Decoupled the `metadata` and `content` in the `ReportCard`.

# teal.reporter 0.1.0

* Initialize the package.
