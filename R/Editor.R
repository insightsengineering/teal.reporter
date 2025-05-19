editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x, x_reactive = x) {
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
  shiny::tagList(
    # shiny::tags$div(id = ns("blocks")),
    shiny::tags$div(
      id = ns("blocks"),
      lapply(names(x), function(block_name) editor_ui(shiny::NS(ns("blocks"), block_name), x = x[[block_name]]))
    ),
    shiny::actionButton(ns("add_block"), label = "Add text block", icon = shiny::icon("plus"))
  )
}

#' @export
editor_srv.ReportDocument <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    # observer calls observer but in a limited scope - only for new items child observers are created
    #  - we can also keep them in a list in order to kill them when we need.
    blocks_called <- shiny::reactiveVal()
    blocks_new <- shiny::reactive(setdiff(names(x_reactive()), blocks_called()))

    shiny::observeEvent(blocks_new(), {
      if (length(blocks_new())) {
        lapply(blocks_new(), function(block_name) {
          reactive_block <- shiny::reactiveVal(x_reactive()[[block_name]])
          blocks_ns <- shiny::NS(session$ns("blocks"))
          editor_srv(shiny::NS("blocks", block_name), x = x_reactive()[[block_name]], x_reactive = reactive_block)
          if (!block_name %in% names(x)) {
            insertUI(
              sprintf("#%s", session$ns("blocks")),
              where = "beforeEnd",
              ui = editor_ui(blocks_ns(block_name), x = x_reactive()[[block_name]])
            )
          }

          shiny::observeEvent(reactive_block(),
            ignoreNULL = FALSE,
            {
              new_x <- x_reactive()
              new_x[[block_name]] <- reactive_block()
              x_reactive(new_x)
            },
            ignoreInit = TRUE
          )

          NULL
        })
        blocks_called(c(blocks_called(), blocks_new()))
      }
    })

    shiny::observeEvent(input$add_block, {
      # because only new names will be called (see blocks_new)
      new_name <- utils::tail(make.unique(c(blocks_called(), "block"), sep = "_"), 1)
      x_reactive(
        modifyList(x_reactive(), stats::setNames(list(""), new_name)) # Preserve attributes
      )
    })
  })
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
editor_srv.default <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    x_reactive
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
editor_srv.character <- function(id, x, x_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    debounced_content <- shiny::debounce(shiny::reactive(input$content), millis = 1000)

    shiny::observeEvent(debounced_content(), x_reactive(debounced_content()))
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
    card_local <- shiny::reactiveVal()
    new_card <- editor_srv("editor", x = card_local)

    shiny::observeEvent(input$button, {
      template_card <- card_r()
      names(template_card) <- make.unique(rep("block", length(template_card)), sep = "_")
      card_local(template_card)

      shiny::showModal(
        shiny::modalDialog(
          title = tags$span(
            class = "edit_title",
            "Editing Card:",
            shiny::uiOutput(session$ns("title")),
            shiny::actionLink(
              session$ns("edit_title"),
              label = "(edit title)",
              class = "text-muted"
            )
          ),
          size = "l",
          easyClose = TRUE,
          shiny::tagList(
            editor_ui(session$ns("editor"), x = card_local),
            shiny::uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("edit_save"), label = "Save"),
            shiny::modalButton("Close")
          )
        )
      )
    })

    observeEvent(card_r(), {
      if (!inherits(card_r(), "ReportDocument")) {
        shiny::removeUI(sprintf("#%s", session$ns("button")))
      }
    })

    output$title <- shiny::renderUI({
      title <- label(card_r())
      if (!is.null(input$edit_title) && input$edit_title > 0) {
        shiny::textInput(session$ns("new_title"), label = NULL, value = title)
        # shinyjs::hide("edit_title")
      } else {
        shiny::tags$span(title)
      }
    })

    shiny::observeEvent(input$edit_save, {
      # TODO: add check on card validity (duplicate title)
      new_card_obj <- new_card()
      if (!is.null(input$new_title)) {
        attr(new_card_obj, "label") <- input$new_title
      }
      if (!identical(new_card_obj, card_r())) {
        reporter$replace_card(card = new_card_obj)
      }
      shiny::removeModal()
    })
  })
}
