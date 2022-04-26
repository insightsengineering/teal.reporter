#' Download Button Reporter User Interface
#' @description button for downloading the Report. Part of the simple Reporter user interface.
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
#' @description server for downloading the Report. Part of the simple Reporter.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character`
#' @param reporter `Reporter` instance.
#' @param notification logical whether to add shiny notification about the download process.
#' @return `shiny::moduleServer`
#' @export
download_report_button_srv <- function(id, reporter, notification = TRUE) {
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
          shiny::textInput(ns("docAuthor"), label = "Author:", value = "NEST"),
          shiny::textInput(ns("docTitle"), label = "Title:", value = "NEST Report"),
          shiny::tags$div(
            shinyWidgets::pickerInput(
              inputId = ns("docType"),
              label = "Choose a document type: ",
              choices = c("pdf document", "html document", "powerpoint presentation", "word document")
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
          renderer <- Renderer$new()
          yaml <- list(
            author = yaml_quoted(input$docAuthor),
            title = yaml_quoted(input$docTitle),
            date = yaml_quoted(as.character(Sys.Date()))
          )

          yaml[["output"]] <- gsub(" ", "_", input$docType)

          yaml_header <- md_header(yaml::as.yaml(yaml))

          if (notification) {
            shiny::showNotification(sprintf("Rendering and Downloading\n%s.", input$docType))
          }

          renderer$render(reporter$get_blocks(), yaml_header)
          temp_zip_file <- tempfile(fileext = ".zip")
          zip::zipr(temp_zip_file, renderer$get_output_dir())
          file.copy(temp_zip_file, file)
          rm(renderer)
        },
        contentType = "application/zip"
      )
    }
  )
}
