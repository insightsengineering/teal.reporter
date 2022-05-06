teal_reporter_previewer <- function(label) {
  structure(
    list(
      label = label,
      server = reporter_previewer_srv, ui = reporter_previewer_ui,
      server_args = list(), ui_args = list()
    ),
    class = "teal_module"
  )
}

#' @export
reporter_previewer_ui <- function(id, rmd_output = c(
  "html_document", "pdf_document",
  "powerpoint_presentation", "word_document"
),
rmd_yaml_args = list(
  author = "NEST", title = "Report",
  date = as.character(Sys.Date()), output = "html_document"
), ...) {
  args <- list(...)
  ns <- NS(id)
  encoding <- tagList(
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

  forms <- NULL
  fluidRow(
    add_previewer_js(ns),
    add_previewer_css(),
    div(
      shiny::tags$div(
        class = "col-md-3",
        div(class = "well", encoding),
        div(class = "form-group", forms)
      ),
      shiny::tags$div(
        class = "col-md-9",
        uiOutput(ns("pcards"))
      )
    )
  )
}

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

      output$pcards <- renderUI({
        # Add card
        reporter$reactiveV()

        input$card_remove_id
        input$card_down_id
        input$card_up_id

        cards <- reporter$get_cards()
        cards_names <- names(cards)
        tags$div(
          class = "panel-group", id = "accordion",
          lapply(seq_along(cards), function(ic) {
            tags$div(
              id = paste0("panel_card_", ic),
              class = "panel panel-default",
              tags$div(
                class = "panel-heading", style = "overflow:auto;",
                tags$h4(
                  class = "panel-title",
                  tags$span(
                    tags$span(
                      class = "preview_card_control",
                      tags$span(class = "card_remove_id", `data-cardid` = ic,
                                style = "float:right;margin-left:10px;margin-right:10px;margin-top:10px;color:#337ab7;",
                                shiny::icon("remove", "fa-2x", verify_fa = FALSE)),
                      tags$span(class = "card_up_id", `data-cardid` = ic,
                                style = "float:right;margin-left:10px;margin-right:10px;margin-top:10px;color:#337ab7;",
                                shiny::icon("arrow-up", "fa-2x", verify_fa = FALSE)),
                      tags$span(class = "card_down_id", `data-cardid` = ic,
                                style = "float:right;margin-left:10px;margin-right:10px;margin-top:10px;color:#337ab7;",
                                shiny::icon("arrow-down", "fa-2x", verify_fa = FALSE))
                    ),
                    tags$a(style = "display: block;padding: 10px 15px;margin: -10px -15px;",
                      `data-toggle` = "collapse", `data-parent` = "#accordion", href = paste0("#collapse", ic),
                      tags$h4(paste0("Card ", ic, ": ", cards[[ic]]$get_content()[[1]]$get_content()), shiny::icon("caret-down"))
                    )
                  )
                )
              ),
              tags$div(
                id = paste0("collapse", ic), class = "panel-collapse collapse out",
                tags$div(
                  class = "panel-body",
                  shiny::tags$div(
                    id = paste0("card", ic),
                    lapply(
                      cards[[ic]]$get_content(),
                      function(b) {
                        resolveBlock2Html(b, cards_names[ic])
                      }
                    )
                  )
                )
              )
            )
          })
        )
      })

      observeEvent(input$card_remove_id, {
        reporter$remove_cards(input$card_remove_id)
      })

      observeEvent(input$card_up_id, {
        if (input$card_up_id > 1) {
          reporter$swap_cards(as.integer(input$card_up_id),
                              as.integer(input$card_up_id - 1))
        }
      })

      observeEvent(input$card_down_id, {
        if (input$card_down_id < length(reporter$get_cards())) {
          reporter$swap_cards(as.integer(input$card_down_id),
                              as.integer(input$card_down_id + 1))
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

resolveBlock2Html <- function(b, name) {
  block_class <- class(b)[1]
  b_content <- b$get_content()
  switch(block_class,
         TextBlock = {
         switch(b$get_style(),
                            header1 = tags$h1(b_content),
                            header2 = tags$h2(b_content),
                            header3 = tags$h3(b_content),
                            header4 = tags$h4(b_content),
                            b_content
         )
           },
         PictureBlock = tags$img(src = knitr::image_uri(b_content)),
         TableBlock = {
           r_table <- readRDS(b_content)
           if (inherits(r_table, "ElementaryTable") || inherits(r_table, "TableTree")) {
             rtables::as_html(r_table)
           } else if (inherits(r_table, "data.frame")) {
             knitr::kable(r_table, "html")
           } else {
             tagList(
               lapply(
                 capture.output(print(r_table)),
                 function(x) tags$p(x)
               )
             )
           }
         },
         NewpageBlock = tags$br(),
         ""
  )
}

#' @export
add_previewer_css <- function() {
  tags$head(tags$style("
                      span.preview_card_control  i:hover {
                        color: green;
                      }
                       "))
}

#' @export
add_previewer_js <- function(ns) {
  tags$head(tags$script(
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
