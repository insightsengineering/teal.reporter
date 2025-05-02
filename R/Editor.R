editor_ui <- function(id, x) {
  UseMethod("editor_ui", x)
}

editor_srv <- function(id, x) {
  UseMethod("editor_srv", x)
}

editor_ui.ReportDocument <- function(id, x) {
  ns <- NS(id)
  tagList(
    # todo: add text button
    lapply(seq_along(x), function(i) editor_ui(ns(i), x[[i]]))
  )
}

editor_srv.ReportDocument <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    new_content <- lapply(seq_along(x), function(i) editor_srv(i, x[[i]]))
    reactive(lapply(new_content, function(reactive_block) reactive_block()))
  })
}

editor_ui.default <- function(id, x) {
  shinyjs::disabled(toHTML(x))
}

editor_srv.default <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    reactive(x)
  })
}

editor_ui.character <- function(id, x) {
  ns <- NS(id)
  shiny::textAreaInput(ns("content"), label = NULL, value = x)
}

editor_srv.character <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    reactive(input$content)
  })
}
