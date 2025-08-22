#' @rdname srv_editor_block
#' @export
ui_editor_block <- function(id, value) {
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
#' @export
srv_editor_block <- function(id, value) {
  UseMethod("srv_editor_block", value)
}

#' @export
ui_editor_block.default <- function(id, value) {
  .ui_editor_block(id, value)
}

#' @export
srv_editor_block.default <- function(id, value) {
  .srv_editor_block(id, value)
}

#' @keywords internal
.ui_editor_block <- function(id, value) {
  UseMethod(".ui_editor_block", value)
}

#' @keywords internal
.srv_editor_block <- function(id, value) {
  UseMethod(".srv_editor_block", value)
}

#' @method .ui_editor_block default
.ui_editor_block.default <- function(id, value) {
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
    tools::toHTML(value)
  )
}

#' @method .srv_editor_block default
.srv_editor_block.default <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) NULL) # No input being changed, skipping update
}

#' @method .ui_editor_block character
.ui_editor_block.character <- function(id, value) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h6(shiny::icon("pencil", class = "text-muted"), "Editable markdown block"),
    shiny::textAreaInput(ns("content"), label = NULL, value = value, width = "100%")
  )
}

#' @method .srv_editor_block character
.srv_editor_block.character <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) shiny::reactive(input$content))
}

ui_doc_editor <- function(id, value) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(value), function(block_name) {
        ui_editor_block(shiny::NS(ns("blocks"), block_name), value = value[[block_name]])
      })
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

srv_doc_editor <- function(id, card_r) {
  shiny::moduleServer(id, function(input, output, session) {
    blocks_inputs_rvs <- shiny::reactiveValues() # Store input names for snapshot
    blocks_queue_rv <- shiny::reactiveVal()

    shiny::observeEvent(card_r(), { # Reset on card change
      for (name in names(blocks_inputs_rvs)) blocks_inputs_rvs[[name]] <- NULL
      blocks_queue_rv(NULL) # Force retriggering
      blocks_queue_rv(names(card_r()))
    })

    shiny::observeEvent(blocks_queue_rv(), {
      lapply(blocks_queue_rv(), function(block_name) {
        new_block_id <- shiny::NS("blocks", block_name)
        block_content <- card_r()[[block_name]] %||% "" # Initialize as empty string
        blocks_inputs_rvs[[block_name]] <- srv_editor_block(new_block_id, value = block_content)

        if (!block_name %in% names(card_r())) { # Only adds UI if not already rendered
          new_block_ui <- ui_editor_block(session$ns(new_block_id), value = block_content)
          shiny::insertUI(sprintf("#%s", session$ns("blocks")), where = "beforeEnd", ui = new_block_ui)
        }
      })
    })

    shiny::observeEvent(input$add_block, {
      new_name <- utils::tail(make.unique(c(names(blocks_inputs_rvs), "block"), sep = "_"), 1)
      blocks_queue_rv(new_name)
    })

    blocks_inputs_rvs
  })
}
