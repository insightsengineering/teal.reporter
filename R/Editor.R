editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x, x_reactive = x) {
  checkmate::assert_class(x_reactive, "reactiveVal")
  UseMethod("editor_srv", x)
}

#' @export
editor_ui.reactiveVal <- function(id, x) {
  ns <- NS(id)
  editor_ui(ns("editor"), isolate(x()))
}

#' @export
editor_srv.reactiveVal <- function(id, x, x_reactive = x) {
  moduleServer(id, function(input, output, session) {
    observeEvent(x(), ignoreNULL = TRUE, once = TRUE, {
      editor_srv("editor", x(), x)
    })
    x
  })
}

#' @export
editor_ui.ReportDocument <- function(id, x) {
  ns <- NS(id)
  tags$div(
    uiOutput(ns("blocks")),
    actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

#' @export
editor_srv.ReportDocument <- function(id, x, x_reactive) {
  moduleServer(id, function(input, output, session) {
    output$blocks <- renderUI({
      tagList(
        lapply(names(x_reactive()), function(block_name) {
          editor_ui(session$ns(block_name), x = x_reactive()[[block_name]])
        })
      )
    })

    # observer calls observer but in a limited scope - only for new items child observers are created
    #  - we can also keep them in a list in order to kill them when we need.
    blocks_called <- reactiveVal()
    blocks_new <- reactive(setdiff(names(x_reactive()), blocks_called()))
    observeEvent(blocks_new(), {
      if (length(blocks_new())) {
        new_blocks <- sapply(blocks_new(), function(block_name) {
          reactive_block <- reactiveVal(x_reactive()[[block_name]])
          editor_srv(block_name, x = x_reactive()[[block_name]], x_reactive = reactive_block)
          observeEvent(reactive_block(), ignoreNULL = FALSE, {
            new_x <- x_reactive()
            new_x[[block_name]] <- reactive_block()
            x_reactive(new_x)
          })
        })
        blocks_called(c(blocks_called(), blocks_new()))
      }
    })

    observeEvent(input$add_block, {
      # because only new names will be called (see blocks_new)
      new_name <- tail(
        make.unique(
          c(
            blocks_called(),
            "block"
          )
        ),
        1
      )
      x_reactive(c(x_reactive(), setNames(list(""), new_name)))
    })
  })
}

#' @export
editor_ui.default <- function(id, x) {
  toHTML(x)
}

#' @export
editor_srv.default <- function(id, x, x_reactive) {
  moduleServer(id, function(input, output, session) {
    x_reactive
  })
}

#' @export
editor_ui.character <- function(id, x) {
  ns <- NS(id)
  shiny::textAreaInput(ns("content"), label = NULL, value = x)
}

#' @export
editor_srv.character <- function(id, x, x_reactive) {
  moduleServer(id, function(input, output, session) {
    debounced_content <- shiny::debounce(reactive(input$content), millis = 10000)

    observeEvent(debounced_content(), {
      x_reactive(debounced_content())
    })
  })
}
