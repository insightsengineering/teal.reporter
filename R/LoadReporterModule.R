#' User Interface to Load `Reporter`
#' @description `r lifecycle::badge("experimental")`
#' Button to upload `ReporterCard`(s) to the `Reporter`.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param label (`character(1)`) label of the button. By default it is empty.
#' @return `shiny::tagList`
#' @export
report_load_ui <- function(id, label = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    .outline_button(
      ns("reporter_load"),
      label = label,
      icon = "upload"
    )
  )
}

#' Server to Load `Reporter`
#' @description `r lifecycle::badge("experimental")`
#' Server to load `ReporterCard`(s) to the `Reporter`
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#'
#' @return `shiny::moduleServer`
#' @export
report_load_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::setBookmarkExclude(c("reporter_load_main", "reporter_load"))
      ns <- session$ns

      archiver_modal <- function() {
        nr_cards <- length(reporter$get_cards())
        shiny::div(
          class = "teal-reporter reporter-modal",
          shiny::modalDialog(
            easyClose = TRUE,
            shiny::tags$h3("Load the Report"),
            shiny::tags$hr(),
            shiny::fileInput(ns("archiver_zip"), "Choose saved Reporter file to Load (a zip file)",
              multiple = FALSE,
              accept = c(".zip")
            ),
            footer = shiny::div(
              shiny::tags$button(
                type = "button",
                class = "btn btn-outline-secondary",
                `data-bs-dismiss` = "modal",
                NULL,
                "Dismiss"
              ),
              shinyjs::disabled(
                shiny::tags$button(
                  id = ns("reporter_load_main"),
                  type = "button",
                  class = "btn btn-primary action-button",
                  NULL,
                  "Load"
                )
              )
            )
          )
        )
      }

      shiny::observeEvent(input$archiver_zip, {
        shinyjs::enable(id = "reporter_load_main")
      })

      shiny::observeEvent(input$reporter_load, {
        shiny::showModal(archiver_modal())
      })

      shiny::observeEvent(input$reporter_load_main, {
        load_json_report(reporter, input$archiver_zip[["datapath"]], input$archiver_zip[["name"]])
        shiny::removeModal()
      })
    }
  )
}

#' @keywords internal
load_json_report <- function(reporter, zip_path, filename) {
  tmp_dir <- tempdir()
  output_dir <- file.path(tmp_dir, sprintf("report_load_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
  dir.create(path = output_dir)
  if (!is.null(zip_path) && grepl("report_", filename)) {
    tryCatch(
      expr = zip::unzip(zip_path, exdir = output_dir, junkpaths = TRUE),
      warning = function(cond) {
        print(cond)
        shiny::showNotification(
          ui = "Unzipping folder warning!",
          action = "Please contact app developer",
          type = "warning"
        )
      },
      error = function(cond) {
        print(cond)
        shiny::showNotification(
          ui = "Unzipping folder error!",
          action = "Please contact app developer",
          type = "error"
        )
      }
    )
    tryCatch(
      reporter$from_jsondir(output_dir),
      warning = function(cond) {
        print(cond)
        shiny::showNotification(
          ui = "Loading reporter warning!",
          action = "Please contact app developer",
          type = "warning"
        )
      },
      error = function(cond) {
        print(cond)
        shiny::showNotification(
          ui = "Loading reporter error!",
          action = "Please contact app developer",
          type = "error"
        )
      }
    )
  } else {
    shiny::showNotification("Failed to load the Reporter file.", type = "error")
  }
}
