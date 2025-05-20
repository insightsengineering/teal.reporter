editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x) {
  UseMethod("editor_srv", x)
}

#' @export
editor_ui.default <- function(id, x) {
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
editor_srv.default <- function(id, x) {
  shiny::moduleServer(id, function(input, output, session) {
    NULL # No input being changed
  })
}

#' @export
editor_ui.character <- function(id, x) {
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
editor_srv.character <- function(id, x) {
  shiny::moduleServer(id, function(input, output, session) session$ns("content"))
}

ui_report_document_editor <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(x), function(block_name) editor_ui(shiny::NS(ns("blocks"), block_name), x = x[[block_name]]))
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

srv_report_document_editor <- function(id, template_card) {
  shiny::moduleServer(id, function(input, output, session) {
    # observer calls observer but in a limited scope - only for new items child observers are created
    #  - we can also keep them in a list in order to kill them when we need.

    blocks_input_names <- shiny::reactiveValues()
    blocks_new_value <- shiny::reactiveValues()
    blocks_queue_r <- shiny::reactiveVal()

    shiny::observeEvent(blocks_queue_r(), {
      queue <- blocks_queue_r()
      lapply(queue, function(block_name) {
        blocks_ns <- shiny::NS(session$ns("blocks"))
        block_content <- template_card[[block_name]] %||% blocks_new_value[[block_name]] # Initialize as empty
        input_name <- editor_srv(shiny::NS("blocks", block_name), x = block_content)
        if (isFALSE(is.null(input_name))) {
          blocks_input_names[[block_name]] <- input_name
        }

        if (!block_name %in% names(template_card)) { # Only adds UI if not already rendered
          insertUI(
            sprintf("#%s", session$ns("blocks")),
            where = "beforeEnd",
            ui = editor_ui(blocks_ns(block_name), x = block_content)
          )
        }
        NULL
      })
    })

    blocks_queue_r(names(template_card)) # Initialize the already rendered blocks

    shiny::observeEvent(input$add_block, {
      new_name <- utils::tail(make.unique(c(names(template_card), names(blocks_new_value), "block"), sep = "_"), 1)
      blocks_new_value[[new_name]] <- ""
      blocks_queue_r(new_name)
    })

    blocks_input_names
  })
}

ui_edit_button <- function(id) {
  shiny::actionLink(
    inputId = NS(id, "button"),
    class = "btn btn-primary btn-sm float-end p-3",
    label = NULL,
    title = "Edit card",
    icon = shiny::icon("edit")
  )
}

srv_edit_button <- function(id, card_r, reporter) {
  moduleServer(id, function(input, output, session) {
    new_card_r <- shiny::reactiveVal()
    block_input_names_r <- shiny::reactiveVal()

    shiny::observeEvent(input$button, {
      template_card <- card_r()
      names(template_card) <- make.unique(rep("block", length(template_card)), sep = "_")
      new_card_r(template_card)

      random_editor_id <- sprintf("editor_%s", rlang::hash(list(rnorm(1), Sys.time())))
      shiny::showModal(
        shiny::modalDialog(
          title = tags$span(
            class = "edit_title_container",
            "Editing Card:",
            shiny::uiOutput(session$ns("title")),
            shiny::actionLink(session$ns("edit_title"), label = "(edit title)", class = "text-muted")
          ),
          size = "l",
          easyClose = TRUE,
          shiny::tagList(
            ui_report_document_editor(session$ns(random_editor_id), x = template_card),
            shiny::uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("edit_save"), label = "Save"),
            shiny::modalButton("Close")
          )
        )
      )

      result <- srv_report_document_editor(random_editor_id, template_card)
      block_input_names_r(result)
    })

    output$title <- shiny::renderUI({
      title <- label(card_r())
      if (!is.null(input$edit_title) && input$edit_title > 0) {
        shinyjs::hide("edit_title")
        shiny::textInput(session$ns("new_title"), label = NULL, value = title)
      } else {
        shiny::tags$span(title)
      }
    })

    shiny::observeEvent(input$edit_save, {
      # TODO: add check on card validity (duplicate title)
      new_card <- new_card_r()
      block_input_names <- shiny::reactiveValuesToList(block_input_names_r())
      # Save snapshot of current inputs
      for (name in names(block_input_names)) {
        input_ix <- sub(session$ns(""), "", block_input_names[[name]])
        new_card[[name]] <- isolate(input[[input_ix]])
      }

      if (isFALSE(is.null(input$new_title))) {
        label(new_card) <- input$new_title
      }
      if (isFALSE(identical(new_card, card_r()))) {
        tryCatch(
          {
            reporter$replace_card(card = new_card)
            shiny::removeModal()
          },
          error = function(err) {
            shiny::showNotification(
              sprintf("A card with the name '%s' already exists. Please use a different name.", label(new_card)),
              type = "error",
              duration = 5
            )
            shinyjs::enable("edit_save")
          }
        )
      } else {
        shiny::removeModal() # Doing nothing
      }
    })

    # Hide button for deprecated objects
    observeEvent(card_r(),
      {
        if (!inherits(card_r(), "ReportDocument")) {
          shiny::removeUI(sprintf("#%s", session$ns("button")))
        }
      },
      once = TRUE
    )
  })
}
