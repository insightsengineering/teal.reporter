editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x) {
  UseMethod("editor_srv", x)
}

#' @export
editor_ui.reactiveVal <- function(id, x) {
  ns <- NS(id)
  uiOutput(ns("content"))
}

#' @export
editor_srv.reactiveVal <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    output$content <- renderUI(editor_ui(session$ns("editor"), x()))
    eventReactive(x(), {
      editor_srv("editor", x())()
    })
  })
}

#' @export
editor_ui.ReportDocument <- function(id, x) {
  ns <- NS(id)
  tagList(
    # todo: add text button
    lapply(seq_along(x), function(i) editor_ui(ns(i), x[[i]]))
  )
}

#' @export
editor_srv.ReportDocument <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    new_content <- lapply(seq_along(x), function(i) editor_srv(i, x[[i]]))

    reactive({
      # todo: it needs to return report_document, not just list
      structure(lapply(new_content, function(reactive_block) reactive_block()), class = "ReportDocument")
    })
  })
}

#' @export
editor_ui.default <- function(id, x) {
  toHTML(x)
}

#' @export
editor_srv.default <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    reactive(x)
  })
}

#' @export
editor_ui.character <- function(id, x) {
  ns <- NS(id)
  shiny::textAreaInput(ns("content"), label = NULL, value = x)
}

#' @export
editor_srv.character <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$content, input$content)
  })
}
