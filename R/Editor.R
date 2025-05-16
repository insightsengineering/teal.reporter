editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x, x_reactive = x) {
  checkmate::assert_class(x_reactive, "reactiveVal")
  UseMethod("editor_srv", x)
}

#' @export
editor_ui.reactiveVal <- function(id, x) {
  ns <- shiny::NS(id)
  editor_ui(ns("editor"), shiny::isolate(x()))
}

#' @export
editor_srv.reactiveVal <- function(id, x, x_reactive = x) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(x(), ignoreNULL = TRUE, once = TRUE, {
      editor_srv("editor", x(), x)
    })
    x
  })
}

#' @export
editor_ui.ReportDocument <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::uiOutput(ns("blocks")),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

#' @export
editor_srv.ReportDocument <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$blocks <- shiny::renderUI({ # Rendered once at the beginning
      shiny::tagList(
        lapply(names(x), function(block_name) editor_ui(session$ns(block_name), x = x[[block_name]]))
      )
    })

    # observer calls observer but in a limited scope - only for new items child observers are created
    #  - we can also keep them in a list in order to kill them when we need.
    blocks_called <- shiny::reactiveVal()
    blocks_new <- shiny::reactive(setdiff(names(x_reactive()), blocks_called()))
    shiny::observeEvent(blocks_new(), {
      if (length(blocks_new())) {
        new_blocks <- sapply(blocks_new(), function(block_name) {
          reactive_block <- shiny::reactiveVal(x_reactive()[[block_name]])
          editor_srv(block_name, x = x_reactive()[[block_name]], x_reactive = reactive_block)

          insertUI(
            sprintf("#%s", session$ns("blocks")),
            where = "beforeEnd",
            ui = editor_ui(session$ns(block_name), x = x_reactive()[[block_name]])
          )

          shiny::observeEvent(reactive_block(), ignoreNULL = FALSE, {
            new_x <- x_reactive()
            new_x[[block_name]] <- reactive_block()
            x_reactive(new_x)
          })
        })
        blocks_called(c(blocks_called(), blocks_new()))
      }
    })

    shiny::observeEvent(input$add_block, {
      # because only new names will be called (see blocks_new)
      new_name <- utils::tail(
        make.unique(
          c(
            blocks_called(),
            "block"
          )
        ),
        1
      )
      x_reactive(
        modifyList(x_reactive(), stats::setNames(list(""), new_name)) # Preserve attributes
      )
    })
  })
}

#' @export
editor_ui.default <- function(id, x) {
  toHTML(x)
}

#' @export
editor_srv.default <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    x_reactive
  })
}

#' @export
editor_ui.character <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::textAreaInput(ns("content"), label = NULL, value = x)
}

#' @export
editor_srv.character <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    debounced_content <- shiny::debounce(shiny::reactive(input$content), millis = 10000)

    shiny::observeEvent(debounced_content(), {
      x_reactive(debounced_content())
    })
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

srv_edit_button <- function(id, original_card, card_r, reporter) {
  moduleServer(id, function(input, output, session) {
    new_card <- editor_srv("editor", x = card_r)

    shiny::observeEvent(input$button, {
      shiny::showModal(
        shiny::modalDialog(
          title = paste("Editing Card:", id),
          size = "l", easyClose = TRUE,
          shiny::tagList(
            editor_ui(session$ns("editor"), x = card_r),
            shiny::uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("edit_save"), label = "Save"),
            shiny::modalButton("Close")
          )
        )
      )
    })

    shiny::observeEvent(input$edit_save, {
      if (!identical(new_card(), card_r())) {
        reporter$replace_card(id = attr(new_card(), "label", exact = TRUE), card = new_card)
        card_r(new_card())
      }
      shiny::removeModal()
    })

    card_r
  })
}
