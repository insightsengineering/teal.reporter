teal_reporter_previewer <- function(label, ...) {
  args <- list(...)
  structure(
    list(
      label = label,
      server = reporter_previewer_srv, ui = reporter_previewer_ui,
      server_args = list(), ui_args = list()
    ),
    class = "teal_module"
  )
}


#' Reporter Previewer User Interface
#' @description reporter previewer user interface to manipulate the already added report Cards
#' @param id `character`
#' @param rmd_output `character` vector with `rmarkdown` output types,
#' by default all possible `c("pdf_document", "html_document", "powerpoint_presentation", "word_document")`.
#' @param rmd_yaml_args `named list` vector with `Rmd` `yaml` header fields and their default values.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document")`.
#' Please update only values at this moment.
#' @export
reporter_previewer_ui <- function(id, rmd_output = c(
                                    "html_document", "pdf_document",
                                    "powerpoint_presentation", "word_document"
                                  ),
                                  rmd_yaml_args = list(
                                    author = "NEST", title = "Report",
                                    date = as.character(Sys.Date()), output = "html_document"
                                  )) {
  ns <- shiny::NS(id)
  encoding <- shiny::tagList(
    shiny::tags$h3("Download the Report"),
    shiny::tags$hr(),
    shiny::textInput(ns("author"), label = "Author:", value = rmd_yaml_args$author),
    shiny::textInput(ns("title"), label = "Title:", value = rmd_yaml_args$title),
    shiny::dateInput(ns("date"), "Date:", value = rmd_yaml_args$date),
    shiny::tags$div(
      shinyWidgets::pickerInput(
        inputId = ns("output"),
        label = "Choose a document type: ",
        choices = rmd_output,
        selected = rmd_yaml_args$output
      )
    ),
    shiny::tags$a(
      id = ns("download_data_prev"),
      class = "btn btn-primary shiny-download-link",
      href = "",
      target = "_blank",
      download = NA,
      shiny::icon("download"),
      "Download Report"
    )
  )

  shiny::fluidRow(
    add_previewer_js(ns),
    add_previewer_css(),
    shiny::tags$div(
      shiny::tags$div(
        class = "col-md-3",
        shiny::tags$div(class = "well", encoding)
      ),
      shiny::tags$div(
        class = "col-md-9",
        shiny::uiOutput(ns("pcards"))
      )
    )
  )
}

#' Reporter Previewer User Interface
#' @description reporter previewer user interface to manipulate the already added report Cards
#' @param id `character`
#' @param reporter `Reporter` instance
#' @param notification `logical`
#' @param rmd_yaml_args `named list` vector with `Rmd` `yaml` header fields and their default values.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document")`.
#' Please update only values at this moment.
#' @export
reporter_previewer_srv <- function(id, reporter, notification = TRUE, rmd_yaml_args = list(
                                     author = "NEST", title = "Report",
                                     date = as.character(Sys.Date()), output = "html_document"
                                   )) {
  checkmate::assert_class(reporter, "Reporter")
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$pcards <- shiny::renderUI({
        reporter$get_reactive_add_card()
        input$card_remove_id
        input$card_down_id
        input$card_up_id

        cards <- reporter$get_cards()
        cards_names <- names(cards)

        if (length(cards)) {
        shiny::tags$div(
          class = "panel-group", id = "accordion",
          lapply(seq_along(cards), function(ic) {
            shiny::tags$div(
              id = paste0("panel_card_", ic),
              class = "panel panel-default",
              shiny::tags$div(
                class = "panel-heading", style = "overflow:auto;",
                shiny::tags$h4(
                  class = "panel-title",
                  shiny::tags$span(
                    shiny::tags$span(
                      class = "preview_card_control",
                      nav_previewer_icon("card_remove_id", "remove", ic),
                      nav_previewer_icon("card_up_id", "arrow-up", ic),
                      nav_previewer_icon("card_down_id", "arrow-down", ic)
                    ),
                    shiny::tags$a(
                      class = "accordion-toggle",
                      style = "display: block;padding: 10px 15px;margin: -10px -15px;",
                      `data-toggle` = "collapse", `data-parent` = "#accordion", href = paste0("#collapse", ic),
                      shiny::tags$h4(paste0("Card ", ic, ": ", cards[[ic]]$get_name()), shiny::icon("caret-down"))
                    )
                  )
                )
              ),
              shiny::tags$div(
                id = paste0("collapse", ic), class = "panel-collapse collapse out",
                shiny::tags$div(
                  class = "panel-body",
                  shiny::tags$div(
                    id = paste0("card", ic),
                    lapply(
                      cards[[ic]]$get_content(),
                      function(b) {
                        resolve_block_to_html(b, cards_names[ic])
                      }
                    )
                  )
                )
              )
            )
          })
        )
        } else {
          shiny::tags$p("No Cards added")
        }
      })

      shiny::observeEvent(input$card_remove_id, {
        reporter$remove_cards(input$card_remove_id)
      })

      shiny::observeEvent(input$card_up_id, {
        if (input$card_up_id > 1) {
          reporter$swap_cards(
            as.integer(input$card_up_id),
            as.integer(input$card_up_id - 1)
          )
        }
      })

      shiny::observeEvent(input$card_down_id, {
        if (input$card_down_id < length(reporter$get_cards())) {
          reporter$swap_cards(
            as.integer(input$card_down_id),
            as.integer(input$card_down_id + 1)
          )
        }
      })

      output$download_data_prev <- shiny::downloadHandler(
        filename = function() {
          paste("report_", format(Sys.time(), "%y%m%d%H%M%S"), ".zip", sep = "")
        },
        content = function(file) {
          if (notification) {
            shiny::showNotification(sprintf("Rendering and Downloading a document."))
          }
          input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
          names(input_list) <- names(rmd_yaml_args)
          report_render_and_compress(reporter, input_list, file)
        },
        contentType = "application/zip"
      )
    }
  )
}

resolve_block_to_html <- function(b, name) {
  block_class <- class(b)[1]
  b_content <- b$get_content()
  switch(block_class,
    TextBlock = {
      switch(b$get_style(),
        header1 = shiny::tags$h1(b_content),
        header2 = shiny::tags$h2(b_content),
        header3 = shiny::tags$h3(b_content),
        header4 = shiny::tags$h4(b_content),
        verbatim = shiny::tags$pre(b_content),
        b_content
      )
    },
    PictureBlock = shiny::tags$img(src = knitr::image_uri(b_content)),
    TableBlock = {
      b_table <- readRDS(b_content)
      shiny::tags$pre(
        paste(utils::capture.output(print(b_table)), collapse = "\n")
      )
    },
    NewpageBlock = shiny::tags$br(),
    ""
  )
}

add_previewer_css <- function() {
  shiny::tags$head(shiny::tags$style("
                      span.preview_card_control  i:hover {
                        color: blue;
                      }
                       "))
}

add_previewer_js <- function(ns) {
  shiny::tags$head(shiny::tags$script(
    sprintf('
          $(document).ready(function(event) {
            $("body").on("click", "span.card_remove_id", function() {
              var val = $(this).data("cardid");
              let msg_confirm = "Do you really want to remove the card " + val + " from the Report?";
              var answer = confirm(msg_confirm);
              if (answer) {
                Shiny.setInputValue("%s", val, {priority: "event"});
                $("#panel_card_" + val).remove();
              }
             });

            $("body").on("click", "span.card_up_id", function() {
              var val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
              });

             $("body").on("click", "span.card_down_id", function() {
              var val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
             });
         })
         ', ns("card_remove_id"), ns("card_up_id"), ns("card_down_id"))
  ))
}

nav_previewer_icon <- function(name, icon_name, idx) {
  shiny::tags$span(
    class = name, `data-cardid` = idx,
    style = "float:right;margin-left:10px;margin-right:10px;margin-top:10px;color:#337ab7;",
    shiny::icon(icon_name, "fa-2x", verify_fa = FALSE)
  )
}
