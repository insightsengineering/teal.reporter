ui_editor_block <- function(id, x) {
  UseMethod("ui_editor_block", x)
}

srv_editor_block <- function(id, x) {
  UseMethod("srv_editor_block", x)
}

#' @export
ui_editor_block.default <- function(id, x) {
  shiny::tags$div(
    class = "expandable-container",
    shiny::tags$h6(
      tags$span(
        class = "fa-stack small text-muted",
        # style = "width: 2em;", # necessary to avoid extra space after icon
        shiny::icon("pencil", class = "fa-stack-1x"),
        shiny::icon("ban", class = "fa-stack-2x fa-inverse text-black-50")
      ),
      "Non-editable block"
    ),
    shiny::tags$div(
      class = "expandable-content",
      toHTML(x)
    )
  )
}

#' @export
srv_editor_block.default <- function(id, x) {
  shiny::moduleServer(id, function(input, output, session) {
    NULL # No input being changed
  })
}

#' @export
ui_editor_block.character <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h6(
      shiny::icon("pencil", class = "text-muted"),
      "Editable markdown block"
    ),
    shiny::textAreaInput(ns("content"), label = NULL, value = x, width = "100%")
  )
}

#' @export
srv_editor_block.character <- function(id, x) {
  shiny::moduleServer(id, function(input, output, session) session$ns("content"))
}

ui_report_document_editor <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(x), function(block_name) ui_editor_block(shiny::NS(ns("blocks"), block_name), x = x[[block_name]]))
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

srv_report_document_editor <- function(id, card_r) {
  shiny::moduleServer(id, function(input, output, session) {
    blocks_input_names_rvs <- shiny::reactiveValues() # Store input names for snapshot
    blocks_new_value_rvs <- shiny::reactiveValues() # Store initial values for new blocks
    blocks_queue_rv <- shiny::reactiveVal()

    shiny::observeEvent(card_r(), { # Reset on card change
      lapply(names(blocks_input_names_rvs), function(ix) blocks_input_names_rvs[[ix]] <- NA)
      lapply(names(blocks_new_value_rvs), function(ix) blocks_new_value_rvs[[ix]] <- NA)
      blocks_queue_rv(names(card_r()))
    })

    shiny::observeEvent(blocks_queue_rv(), {
      lapply(blocks_queue_rv(), function(block_name) {
        new_block_id <- shiny::NS("blocks", block_name)
        block_content <- card_r()[[block_name]] %||% blocks_new_value_rvs[[block_name]] # Initialize as empty
        input_name <- srv_editor_block(new_block_id, x = block_content)
        if (isFALSE(is.null(input_name))) {
          blocks_input_names_rvs[[block_name]] <- input_name
        }

        if (!block_name %in% names(card_r())) { # Only adds UI if not already rendered
          new_block_ui <- ui_editor_block(session$ns(new_block_id), x = block_content)
          insertUI(sprintf("#%s", session$ns("blocks")), where = "beforeEnd", ui = new_block_ui)
        }
        NULL
      })
    })

    shiny::observeEvent(input$add_block, {
      new_name <- utils::tail(make.unique(c(names(card_r()), names(blocks_new_value_rvs), "block"), sep = "_"), 1)
      blocks_new_value_rvs[[new_name]] <- ""
      blocks_queue_rv(new_name)
    })

    blocks_input_names_rvs
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

srv_previewer_card_actions <- function(id, card_r, reporter) {
  moduleServer(id, function(input, output, session) {
    new_card_rv <- shiny::reactiveVal()

    shiny::observeEvent(input$edit_action, {
      template_card <- card_r()
      names(template_card) <- make.unique(rep("block", length(template_card)), sep = "_")
      new_card_rv(template_card)

      shiny::showModal(
        shiny::modalDialog(
          title = tags$span(
            class = "edit_title_container",
            "Editing Card:",
            shiny::tags$span(id = session$ns("static_title"), metadata(template_card, "title")),
            shiny::actionLink(session$ns("edit_title"), label = "(edit title)", class = "text-muted"),
            shinyjs::hidden(shiny::textInput(session$ns("new_title"), label = NULL, value = metadata(template_card, "title")))
          ),
          size = "l",
          easyClose = TRUE,
          shiny::tagList(
            ui_report_document_editor(session$ns("editor"), x = template_card),
            shiny::uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("edit_save"), label = "Save"),
            shiny::modalButton("Close")
          )
        )
      )
    })

    block_input_names_rvs <- srv_report_document_editor("editor", new_card_rv)

    observeEvent(input$edit_title, {
      shinyjs::hide("edit_title")
      shinyjs::hide("static_title")
      shinyjs::show("new_title")
      shinyjs::runjs(
        sprintf(
          "
          const input = document.getElementById('%s');
          input.focus();
          input.setSelectionRange(input.value.length, input.value.length);
          ",
          session$ns("new_title")
        )
      )
    })

    # Handle
    shiny::observeEvent(input$edit_save, {
      new_card <- new_card_rv()
      block_input_names <- Filter(Negate(is.na), shiny::reactiveValuesToList(block_input_names_rvs))
      for (name in names(block_input_names)) { # Save snapshot of current inputs
        input_ix <- sub(session$ns(""), "", block_input_names[[name]])
        new_card[[name]] <- isolate(input[[input_ix]])
      }
      if (isFALSE(is.null(input$new_title))) {
        metadata(new_card, "title") <- input$new_title
      }
      if (isFALSE(identical(new_card, card_r()))) {
        tryCatch(
          {
            reporter$replace_card(card = new_card)
            new_card_rv(NULL)
            shiny::removeModal()
          },
          error = function(err) {
            shiny::showNotification(
              sprintf("A card with the name '%s' already exists. Please use a different name.", metadata(new_card, "title")),
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
    shiny::observeEvent(input$remove_action, {
      reporter$remove_cards(ids = metadata(card_r(), "id"))
    })

    observeEvent( # Hide button for deprecated objects
      card_r(),
      once = TRUE,
      handlerExpr = {
        if (!inherits(card_r(), "ReportDocument")) {
          shiny::removeUI(sprintf("#%s", session$ns("edit_action")))
        }
      }
    )
  })
}
