ui_card_editor <- function(id, value, cached_html) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(value), function(block_name) {
        ui_editor_block(
          shiny::NS(ns("blocks"), block_name),
          value = value[[block_name]],
          cached_html = cached_html
        )
      })
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

srv_card_editor <- function(id, card_r) {
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
