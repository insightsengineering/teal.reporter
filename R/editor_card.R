#' Card editor module
#'
#' Card editor module
#'
#' @param id (`character(1)`) A unique identifier for the module.
#' @param value (`named list`) The content of the blocks to be edited. It can be a character string or other types.
#' @param cached_html (`named list` of `shiny.tag` or `shiny.tag.list`) Cached HTML content to display in the UI.
#' @name module_editor_card
#' @return
#' - `ui_editor_card` returns `shiny.tag`
#' - `srv_editor_card` returns `reactivevalues` named after name of the block element
NULL

#' @rdname module_editor_card
#' @export
ui_editor_card <- function(id, value, cached_html = NULL) {
  checkmate::assert_string(id)
  checkmate::assert_multi_class(cached_html, c("shiny.tag", "shiny.tag.list", "character"), null.ok = TRUE)
  UseMethod("ui_editor_card")
}

#' @rdname module_editor_card
#' @export
srv_editor_card <- function(id, card_r) {
  checkmate::assert_string(id)
  checkmate::assert_class(card_r, "reactive")
  UseMethod("srv_editor_card")
}

#' @rdname module_editor_card
#' @export
ui_editor_card.default <- function(id, value, cached_html) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(value), function(block_name) {
        ui_editor_block(
          shiny::NS(ns("blocks"), block_name),
          value = value[[block_name]],
          cached_html = cached_html[[block_name]]
        )
      })
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

#' @export
srv_editor_card.default <- function(id, card_r) {
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
        block_content <- card_r()[[block_name]] %||% "" # because block_content = NULL can't have a S3 method
        blocks_inputs_rvs[[block_name]] <- srv_editor_block(new_block_id, value = block_content)

        if (!block_name %in% names(card_r())) { # Only adds UI if not already rendered
          new_block_ui <- ui_editor_block(
            session$ns(new_block_id),
            value = block_content,
            cached_html = NULL
          )
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
