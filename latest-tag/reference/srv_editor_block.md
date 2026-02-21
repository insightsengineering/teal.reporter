# UI and Server functions for editing report document blocks

These functions provide a user interface and server logic for editing
and extending the editor functionality to support new data types.

## Usage

``` r
ui_editor_block(id, value, ...)

srv_editor_block(id, value, ...)
```

## Arguments

- id:

  (`character(1)`) A unique identifier for the module.

- value:

  The content of the block to be edited. It can be a character string or
  other types.

- ...:

  Additional arguments passed to dispatch functions.

## Details

The methods for this S3 generic can be extended by the app developer to
new classes or even overwritten. For this a function with the name
`srv_editor_block.<class>` and/or `ui_editor_block.<class>` should be
defined in the Global Environment, where `<class>` is the class of the
object to be used in the method.

For example, to override the default behavior for `character` class, you
can use:

    ui_editor_block.character <- function(id, value) {
      # custom implementation
      shiny::tagList(
        shiny::tags$h6(shiny::icon("pencil", class = "text-muted"), "Editable CUSTOM markdown block"),
        shiny::textAreaInput(ns("content"), label = NULL, value = value, width = "100%")
      )
    }
    srv_editor_block.character <- function(id, value) {
     # custom implementation
     # ...
    }

Alternatively, you can register the S3 method using
`registerS3method("ui_editor_block", "<class>", fun)` and
`registerS3method("srv_editor_block", "<class>", fun)`.

### Optional arguments of `ui_editor_block`

- `cached_html`: (`shiny.tag` or `shiny.tag.list`) Cached HTML content
  to display in the UI that is rendered at the time the card is being
  added. Default is `NULL`.

  `teal` will call on `ui_editor_block` with the contents of each
  element of the card (`value` argument) and an optional parameter
  `cached_html`. This parameter is not part of the S3 generic as it is
  optional when overriding the method.

  The usage of this argument improve the UI performance by avoiding
  re-rendering on the fly. When overriding this method, the usage of
  this argument is optional, hence it is not part of the S3 generic.
