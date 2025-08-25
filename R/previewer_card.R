previewer_card_ui <- function(id, card_id) {
  ns <- shiny::NS(id)
  accordion_item <- bslib::accordion_panel(
    value = card_id,
    title = shiny::tags$label(shiny::uiOutput(ns("title"))),
    icon = bslib::tooltip(
      bsicons::bs_icon("arrows-move"),
      "Move card"
    ),
    shiny::tags$h6(id = ns("loading_placeholder"), class = "text-muted", "Loading the report..."),
    shiny::uiOutput(ns("card_content"))
  )
  accordion_item <- shiny::tagAppendAttributes(accordion_item, "data-rank-id" = card_id)

  accordion_item <- shiny::tagAppendAttributes(
    tag = accordion_item,
    .cssSelector = ".accordion-header",
    class = "d-flex",
  )

  accordion_item <- htmltools::tagAppendChildren(
    tag = accordion_item,
    .cssSelector = ".accordion-header",
    ui_previewer_card_actions(ns("actions"))
  )
}

previewer_card_srv <- function(id, card_r, card_id, reporter) {
  # todo: card_name should be only on the server side
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderUI({
      title <- metadata(shiny::req(card_r()), "title")
      if (is.null(title) || isFALSE(nzchar(title))) {
        title <- shiny::tags$span("(Empty title)", class = "text-muted")
      }
      title
    })
    output$card_content <- shiny::renderUI({
      result <- reporter$get_cached_html(card_id)
      shiny::removeUI(sprintf("#%s", session$ns("loading_placeholder")))
      result
    })

    srv_previewer_card_actions("actions", card_r, card_id, reporter)
  })
}
