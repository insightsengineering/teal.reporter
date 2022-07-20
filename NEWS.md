# teal.reporter 0.1.0.9022

* Added the `Archiver` class which enables saving and loading back the `Report` in the `shiny` session.
* Added `to_list` and `from_list` methods to all content related classes.
* Added support for the `ElementaryTree` class in the `append_table` method of `ReportCard`.
* Decoupled the `metadata` and `content` in the `ReportCard`.
* Added additional validation for the `card_fun` evaluation.
* Added support for more arguments setup for a `card_fun` function in the `add_card_button_srv` module, specifically the `card_fun` could have any subset of the possible arguments.
* Added the optional `dim` argument to the `append_plot` method in `ReportCard`. 
* Fixed how `trellis` plots are catched by the `set_content` method in the `PictureBlock`.
* Updated `append_fs` method in the `TealReportCard`, to accept a `list` object.
* Updated support for `Heatmap` plots in `PictureBlock`.

# teal.reporter 0.1.0

* Initialize the package.
