#' @rdname srv_editor_block
#' @export
ui_editor_block <- function(id, value) {
  UseMethod("ui_editor_block", value)
}

#' UI and Server functions for editing report document blocks
#'
#' These functions provide a user interface and server logic for editing and extending
#' the editor functionality to support new data types.
#' @param id (`character(1)`) A unique identifier for the module.
#' @param value The content of the block to be edited. It can be a character string or other types.
#' @export
srv_editor_block <- function(id, value) {
  UseMethod("srv_editor_block", value)
}

#' @export
ui_editor_block.default <- function(id, value) {
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
    toHTML(value)
  )
}

#' @export
srv_editor_block.default <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) NULL) # No input being changed, skipping update
}

#' @export
ui_editor_block.character <- function(id, value) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h6(shiny::icon("pencil", class = "text-muted"), "Editable markdown block"),
    shiny::textAreaInput(ns("content"), label = NULL, value = value, width = "100%")
  )
}

#' @export
srv_editor_block.character <- function(id, value) {
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

ui_previewer_card_actions <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionLink(
      inputId = ns("edit_action"),
      class = "btn btn-primary btn-sm float-end p-3",
      label = NULL,
      title = "Edit card",
      icon = shiny::icon("edit")
    ),
    shiny::actionLink(
      inputId = ns("remove_action"),
      class = "btn btn-danger btn-sm float-end p-3",
      label = NULL,
      icon = shiny::icon("trash-alt"),
    )
  )
}

srv_previewer_card_actions <- function(id, card_r, card_id, reporter) {
  shiny::moduleServer(id, function(input, output, session) {
    new_card_rv <- shiny::reactiveVal()

    shiny::observeEvent(input$edit_action, {
      template_card <- card_r()
      names(template_card) <- make.unique(rep("block", length(template_card)), sep = "_")
      new_card_rv(template_card)
      title <- metadata(template_card, "title")

      if (isFALSE(nzchar(title))) {
        title <- shiny::tags$span(class = "text-muted", "(empty title)")
      }

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$span(
            class = "edit_title_container",
            "Editing Card:",
            shiny::tags$span(id = session$ns("static_title"), title),
            shiny::actionButton(
              session$ns("edit_title"),
              label = shiny::tags$span(shiny::icon("pen-to-square"), "edit title"),
              class = "fs-6",
              title = "Edit title"
            ),
            shinyjs::hidden(
              shiny::textInput(
                session$ns("new_title"),
                label = NULL, value = metadata(template_card, "title")
              )
            )
          ),
          size = "l",
          easyClose = TRUE,
          shiny::tagList(
            ui_doc_editor(session$ns("editor"), value = template_card),
            shiny::uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("edit_save"), label = "Save"),
            shiny::modalButton("Close")
          )
        )
      )
    })

    block_input_names_rvs <- srv_doc_editor("editor", new_card_rv)

    shiny::observeEvent(input$edit_title, {
      shinyjs::hide("edit_title")
      shinyjs::hide("static_title")
      shinyjs::show("new_title")
      shinyjs::js$jumpToFocus(session$ns("new_title"))
    })

    # Handle
    shiny::observeEvent(input$edit_save, {
      new_card <- new_card_rv()
      input_r <- Filter(Negate(is.null), shiny::reactiveValuesToList(block_input_names_rvs))
      for (name in names(input_r)) {
        new_card[[name]] <- shiny::isolate(input_r[[name]]())
      }
      if (isFALSE(is.null(input$new_title))) {
        metadata(new_card, "title") <- input$new_title
      }
      if (isFALSE(identical(new_card, card_r()))) {
        tryCatch(
          {
            reporter$replace_card(card = new_card, card_id = card_id)
            new_card_rv(NULL)
            shiny::removeModal()
          },
          error = function(err) {
            shiny::showNotification(
              sprintf(
                "A card with the name '%s' already exists. Please use a different name.",
                metadata(new_card, "title")
              ),
              type = "error",
              duration = 5
            )
            shinyjs::enable("edit_save")
          }
        )
      } else {
        new_card_rv(NULL)
        shiny::removeModal() # Doing nothing
      }
    })

    # Handle remove button
    shiny::observeEvent(input$remove_action, reporter$remove_cards(ids = card_id))

    shiny::observeEvent( # Hide button for deprecated objects
      card_r(),
      once = TRUE,
      handlerExpr = {
        if (!inherits(card_r(), "card")) {
          shiny::removeUI(sprintf("#%s", session$ns("edit_action")))
        }
      }
    )
  })
}
