# teal.reporter 0.1.1.9000

### New features
* Added additional input when needed to decided if Show R Code should be part of the report.
* Added support for global knitr options in the render method in the Render class.

### Breaking changes
* Simplified `reporter_previewer_ui` to contain just `id` argument.

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
* Fixed how `trellis` plots are catched by the `set_content` method in the `PictureBlock`.

### Miscellaneous
* Added `to_list` and `from_list` methods to all content related classes.
* Decoupled the `metadata` and `content` in the `ReportCard`.

# teal.reporter 0.1.0

* Initialize the package.
