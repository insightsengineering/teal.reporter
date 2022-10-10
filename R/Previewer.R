#' Reporter Previewer User Interface
#' @description `r lifecycle::badge("experimental")`
#' reporter previewer user interface to visualize and manipulate the already added report Cards
#' @param id `character(1)` this `shiny` module's id.
#' @param rmd_output `character` vector with `rmarkdown` output types,
#' by default all possible `c("pdf_document", "html_document", "powerpoint_presentation", "word_document")`.
#' If vector is named then those names will appear in the `UI`.
#' @param rmd_yaml_args `named list` vector with `Rmd` `yaml` header fields and their default values.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document")`.
#' Please update only values at this moment.
#' @export
reporter_previewer_ui <- function(id, rmd_output = c(
                                    "html" = "html_document", "pdf" = "pdf_document",
                                    "powerpoint" = "powerpoint_presentation",
                                    "word" = "word_document"
                                  ),
                                  rmd_yaml_args = list(
                                    author = "NEST", title = "Report",
                                    date = as.character(Sys.Date()), output = "html_document"
                                  )) {
  checkmate::assert_list(rmd_yaml_args)

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
      shiny::tags$span("Download Report", shiny::icon("download"))
    ),
    teal.reporter::reset_report_button_ui(ns("resetButtonPreviewer"), label = "Reset Report")
  )

  shiny::fluidRow(
    add_previewer_js(ns),
    add_previewer_css(),
    shiny::tagList(
      shiny::tags$div(
        class = "col-md-3",
        shiny::tags$div(class = "well", encoding)
      ),
      shiny::tags$div(
        class = "col-md-9",
        shiny::tags$div(
          id = "reporter_previewer",
          shiny::uiOutput(ns("pcards"))
        )
      )
    )
  )
}

#' Reporter Previewer Server
#' @description `r lifecycle::badge("experimental")`
#' server supporting the functionalities of the reporter previewer
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter `Reporter` instance
#' @param rmd_yaml_args `named list` vector with `Rmd` `yaml` header fields and their default values.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document")`.
#' Please update only values at this moment.
#' @export
reporter_previewer_srv <- function(id, reporter, rmd_yaml_args = list(
                                     author = "NEST", title = "Report",
                                     date = as.character(Sys.Date()), output = "html_document"
                                   )) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_list(rmd_yaml_args)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      teal.reporter::reset_report_button_srv("resetButtonPreviewer", reporter)

      output$pcards <- shiny::renderUI({
        reporter$get_reactive_add_card()
        input$card_remove_id
        input$card_down_id
        input$card_up_id

        cards <- reporter$get_cards()

        if (length(cards)) {
          shiny::tags$div(
            class = "panel-group accordion",
            id = "reporter_previewer_panel",
            lapply(seq_along(cards), function(ic) {
              previewer_collapse_item(ic, cards[[ic]]$get_name(), cards[[ic]]$get_content())
            })
          )
        } else {
          shiny::tags$div(
            id = "reporter_previewer_panel_no_cards",
            shiny::tags$p(
              class = "text-danger mt-4",
              shiny::tags$strong("No Cards added")
            )
          )
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
          shiny::showNotification("Rendering and Downloading the document.")
          input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
          names(input_list) <- names(rmd_yaml_args)
          report_render_and_compress(reporter, input_list, file)
        },
        contentType = "application/zip"
      )
    }
  )
}

#' @keywords internal
block_to_html <- function(b) {
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

#' @keywords internal
add_previewer_css <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/Previewer.css", package = "teal.reporter")))
    ),
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    )
  )
}

#' @keywords internal
add_previewer_js <- function(ns) {
  shiny::singleton(
    shiny::tags$head(shiny::tags$script(
      shiny::HTML(sprintf('
          $(document).ready(function(event) {
            $("body").on("click", "span.card_remove_id", function() {
              let val = $(this).data("cardid");
              let msg_confirm = "Do you really want to remove the card " + val + " from the Report?";
              let answer = confirm(msg_confirm);
              if (answer) {
                Shiny.setInputValue("%s", val, {priority: "event"});
                $("#panel_card_" + val).remove();
              }
            });

            $("body").on("click", "span.card_up_id", function() {
              let val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
            });

             $("body").on("click", "span.card_down_id", function() {
              let val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
             });

             $("body").on("DOMSubtreeModified", "#reporter_previewer", function() {
              let accor = $(this).find("#reporter_previewer_panel");
              let down_button = $("#%s");
              if (accor && (accor.length === 0)) {
                down_button.addClass("disabled");
              } else {
                down_button.removeClass("disabled");
              }
             });

          });
         ', ns("card_remove_id"), ns("card_up_id"), ns("card_down_id"), ns("download_data_prev")))
    ))
  )
}

#' @keywords internal
nav_previewer_icon <- function(name, icon_name, idx, size = 1L) {
  checkmate::assert_string(name)
  checkmate::assert_string(icon_name)
  checkmate::assert_int(size)

  shiny::tags$span(
    class = paste(name, "icon_previewer"),
    # data field needed to record clicked card on the js side
    `data-cardid` = idx,
    shiny::icon(icon_name, sprintf("fa-%sx", size))
  )
}

#' @keywords internal
nav_previewer_icons <- function(idx, size = 1L) {
  shiny::tags$span(
    class = "preview_card_control",
    nav_previewer_icon(name = "card_remove_id", icon_name = "xmark", idx = idx, size = size),
    nav_previewer_icon(name = "card_up_id", icon_name = "arrow-up", idx = idx, size = size),
    nav_previewer_icon(name = "card_down_id", icon_name = "arrow-down", idx = idx, size = size)
  )
}

#' @keywords internal
previewer_collapse_item <- function(idx, card_name, card_blocks) {
  shiny::tags$div(.renderHook = function(x) {
    # get bs version
    version <- get_bs_version()

    if (version == "3") {
      shiny::tags$div(
        id = paste0("panel_card_", idx),
        class = "panel panel-default",
        shiny::tags$div(
          class = "panel-heading overflow-auto",
          shiny::tags$div(
            class = "panel-title",
            shiny::tags$span(
              nav_previewer_icons(idx = idx),
              shiny::tags$a(
                class = "accordion-toggle block py-3 px-4 -my-3 -mx-4",
                `data-toggle` = "collapse",
                `data-parent` = "#reporter_previewer_panel",
                href = paste0("#collapse", idx),
                shiny::tags$h4(paste0("Card ", idx, ": ", card_name), shiny::icon("caret-down"))
              )
            )
          )
        ),
        shiny::tags$div(
          id = paste0("collapse", idx), class = "collapse out",
          shiny::tags$div(
            class = "panel-body",
            shiny::tags$div(
              id = paste0("card", idx),
              lapply(
                card_blocks,
                function(b) {
                  block_to_html(b)
                }
              )
            )
          )
        )
      )
    } else {
      shiny::tags$div(
        id = paste0("panel_card_", idx),
        class = "card",
        shiny::tags$div(
          class = "overflow-auto",
          shiny::tags$div(
            class = "card-header",
            shiny::tags$span(
              nav_previewer_icons(idx = idx),
              shiny::tags$a(
                class = "accordion-toggle block py-3 px-4 -my-3 -mx-4",
                # bs4
                `data-toggle` = "collapse",
                # bs5
                `data-bs-toggle` = "collapse",
                href = paste0("#collapse", idx),
                shiny::tags$h4(
                  paste0("Card ", idx, ": ", card_name),
                  shiny::icon("caret-down")
                )
              )
            )
          )
        ),
        shiny::tags$div(
          id = paste0("collapse", idx),
          class = "collapse out",
          # bs4
          `data-parent` = "#reporter_previewer_panel",
          # bs5
          `data-bs-parent` = "#reporter_previewer_panel",
          shiny::tags$div(
            class = "card-body",
            shiny::tags$div(
              id = paste0("card", idx),
              lapply(
                card_blocks,
                function(b) {
                  block_to_html(b)
                }
              )
            )
          )
        )
      )
    }
  })
}
