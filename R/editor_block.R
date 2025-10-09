#' @rdname srv_editor_block
#' @export
ui_editor_block <- function(id, value, cached_html) {
  UseMethod("ui_editor_block", value)
}

#' UI and Server functions for editing report document blocks
#'
#' These functions provide a user interface and server logic for editing and extending
#' the editor functionality to support new data types.
#'
#' @details
#' The methods for this S3 generic can be extended by the app developer to new classes
#' or even overwritten.
#' For this a function with the name `srv_editor_block.<class>` and/or `ui_editor_block.<class>`
#' should be defined in the Global Environment, where `<class>` is the class of
#' the object to be used in the method.
#'
#' For example, to override the default behavior for `character` class, you can use:
#'
#' ```r
#' ui_editor_block.character <- function(id, value) {
#'   # custom implementation
#'   shiny::tagList(
#'     shiny::tags$h6(shiny::icon("pencil", class = "text-muted"), "Editable CUSTOM markdown block"),
#'     shiny::textAreaInput(ns("content"), label = NULL, value = value, width = "100%")
#'   )
#' }
#' srv_editor_block.character <- function(id, value) {
#'  # custom implementation
#'  # ...
#' }
#' ```
#'
#' Alternatively, you can register the S3 method using
#' `registerS3method("ui_editor_block", "<class>", fun)` and
#' `registerS3method("srv_editor_block", "<class>", fun)`.
#'
#' @param id (`character(1)`) A unique identifier for the module.
#' @param value The content of the block to be edited. It can be a character string or other types.
#' @param cached_html (`shiny.tag` or `shiny.tag.list`) Cached HTML content to display in the UI.
#' @export
srv_editor_block <- function(id, value) {
  UseMethod("srv_editor_block", value)
}

#' @export
ui_editor_block.default <- function(id, value, cached_html) {
  .ui_editor_block(id, value, cached_html)
}

#' @export
srv_editor_block.default <- function(id, value) {
  .srv_editor_block(id, value)
}

#' @keywords internal
.ui_editor_block <- function(id, value, cached_html) {
  UseMethod(".ui_editor_block", value)
}

#' @keywords internal
.srv_editor_block <- function(id, value) {
  UseMethod(".srv_editor_block", value)
}

#' @method .ui_editor_block default
.ui_editor_block.default <- function(id, value, cached_html) {
  shiny::tags$div(
    shiny::tags$h6(
      shiny::tags$span(
        class = "fa-stack small text-muted",
        # style = "width: 2em;", # necessary to avoid extra space after icon
        shiny::icon("pencil", class = "fa-stack-1x"),
        shiny::icon("ban", class = "fa-stack-2x fa-inverse text-black-50")
      ),
      "Non-editable block"
    ),
    if (is.null(cached_html)) {
      tools::toHTML(value)
    } else {
      cached_html
    }
  )
}

#' @method .srv_editor_block default
.srv_editor_block.default <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) result <- NULL) # No input being changed, skipping update
}

#' @method .ui_editor_block character
.ui_editor_block.character <- function(id, value, cached_html) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h6(shiny::icon("pencil", class = "text-muted"), "Editable markdown block"),
    shiny::textAreaInput(ns("content"), label = NULL, value = value, width = "100%")
  )
}

#' @method .srv_editor_block character
.srv_editor_block.character <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) result <- shiny::reactive(input$content))
}
