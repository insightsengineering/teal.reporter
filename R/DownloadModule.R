#' Download Button Reporter User Interface
#' @description button for downloading the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character`
#' @return `shiny::tagList`
#' @export
download_report_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$button(
      id = ns("download_button"),
      type = "button",
      class = "btn btn-primary action-button",
      `data-val` = shiny::restoreInput(id = ns("download_button"), default = NULL),
      NULL,
      "Download Report"
    )
  )
}

#' Download Button Server
#' @description server for downloading the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character`
#' @param reporter `Reporter` instance.
#' @param notification `logical` whether to add a shiny notification about the download process. Default `TRUE`.
#' @param output_types `character` vector with `rmarkdown` output types,
#' by default `c("pdf_document", "html_document", "powerpoint_presentation", "word_document")`.
#' @return `shiny::moduleServer`
#' @export
download_report_button_srv <- function(id,
                                       reporter,
                                       notification = TRUE,
                                       output_types = c(
                                         "pdf_document", "html_document",
                                         "powerpoint_presentation", "word_document"
                                       )) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      download_modal <- function(failed = FALSE) {
        nr_cards <- length(reporter$get_cards())
        downb <- shiny::tags$a(
          id = ns("download_data"),
          class = paste("btn btn-primary shiny-download-link", if (nr_cards) NULL else "disabled"),
          style =  if (nr_cards) NULL else "pointer-events: none;",
          href = "",
          target = "_blank",
          download = NA,
          shiny::icon("download"),
          "Download Report"
        )
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Download the Report"),
          shiny::tags$hr(),
          if (length(reporter$get_cards()) == 0) {
            shiny::tags$div(
              shiny::tags$p(shiny::tags$strong("No Cards Added"), style = "color: red; margin-bottom:15px;")
            )
          } else {
            shiny::tags$div(
              style = "color: green; margin-bottom:15px;",
              shiny::tags$p(
                shiny::tags$strong(paste("Number of cards: ", nr_cards))
              ),
            )
          },
          shiny::textInput(ns("author"), label = "Author:", value = "NEST"),
          shiny::textInput(ns("title"), label = "Title:", value = "NEST Report"),
          shiny::tags$div(
            shinyWidgets::pickerInput(
              inputId = ns("output"),
              label = "Choose a document type: ",
              choices = output_types
            )
          ),
          if (failed) {
            shiny::tags$div(shiny::tags$b("Invalid", style = "color: red;"))
          },
          footer = shiny::tagList(
            shiny::tags$button(
              id = ns("reset_reporter"),
              type = "button",
              style = "float: left;",
              class = "btn btn-danger action-button",
              `data-val` = shiny::restoreInput(id = ns("reset_reporter"), default = NULL),
              NULL,
              "Reset Reporter"
            ),
            shiny::tags$button(
              type = "button",
              class = "btn btn-primary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            downb
          )
        )
      }

      shiny::observeEvent(input$download_button, {
        shiny::showModal(download_modal())
      })

      shiny::observeEvent(input$reset_reporter, {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tags$h3("Reset the Report"),
            shiny::tags$hr(),
            shiny::tags$strong(shiny::tags$p("Are you sure you want to reset the report?")),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("reset_reporter_ok"), "Reset")
            )
          )
        )
      })

      shiny::observeEvent(input$reset_reporter_ok, {
        reporter$reset()
        shiny::removeModal()
      })

      output$download_data <- shiny::downloadHandler(
        filename = function() {
          paste("report_", format(Sys.time(), "%y%m%d%H%M%S"), ".zip", sep = "")
        },
        content = function(file) {
          if (notification) {
            shiny::showNotification(sprintf("Rendering and Downloading a document."))
          }

          render_report(reporter, input, file)
        },
        contentType = "application/zip"
      )
    }
  )
}

#' Render the Report
#' @description render the report and zip the created directory.
#' @param reporter `Reporter` instance.
#' @param input `reactivevalues` shiny input.
#' @param file `character` where to copy the returned directory.
#' @return `file` argument
#' @keywords internal
render_report <- function(reporter, input, file = tempdir()) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_class(input, "reactivevalues")
  checkmate::assert_string(file)

  yaml <- list(
    author = input$author,
    title = input$title,
    date = as.character(Sys.Date())
  )
  if (!is.null(input$output)) {
    yaml[["output"]] <- input$output
  }
  yaml_header <- md_header(yaml::as.yaml(yaml))

  renderer <- Renderer$new()
  renderer$render(reporter$get_blocks(), yaml_header)

  temp_zip_file <- tempfile(fileext = ".zip")
  zip::zipr(temp_zip_file, renderer$get_output_dir())
  file.copy(temp_zip_file, file)

  rm(renderer)
  file
}
