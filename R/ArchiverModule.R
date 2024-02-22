#' Archiver User Interface
#' @description `r lifecycle::badge("experimental")`
#' button for adding views/cards to the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny::tagList`
#' @export
archiver_load_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$button(
      id = ns("archiver_reporter_load"),
      type = "button",
      class = "simple_report_button btn btn-primary action-button",
      title = "Load",
      `data-val` = shiny::restoreInput(id = ns("archiver_reporter_load"), default = NULL),
      NULL,
      shiny::tags$span(
        shiny::icon("upload")
      )
    )
  )
}

#' Archiver Server
#' @description `r lifecycle::badge("experimental")`
#' server for saving and loading to the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#'
#' @return `shiny::moduleServer`
#' @export
archiver_load_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      archiver_modal <- function() {
        nr_cards <- length(reporter$get_cards())
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Load the Report"),
          shiny::tags$hr(),
          shiny::fileInput(ns("archiver_zip"), "Choose Archiver File to Load (a zip file)",
            multiple = FALSE,
            accept = c(".zip")
          ),
          footer = shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-danger",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::tags$button(
              id = ns("load_archiver"),
              type = "button",
              class = "btn btn-primary action-button",
              `data-val` = shiny::restoreInput(id = ns("load_archiver"), default = NULL),
              NULL,
              "Load"
            )
          )
        )
      }

      shiny::observeEvent(input$archiver_reporter_load, {
        shiny::showModal(archiver_modal())
      })

      shiny::observeEvent(input$load_archiver, {
        switch("JSON",
          JSON = load_json_archiver(reporter, input$archiver_zip[["datapath"]], input$archiver_zip[["name"]]),
          stop("The provided archiver format is not supported")
        )

        shiny::removeModal()
      })
    }
  )
}

#' @keywords internal
load_json_archiver <- function(reporter, zip_path, filename) {
  tmp_dir <- tempdir()
  output_dir <- file.path(tmp_dir, sprintf("archiver_load_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
  dir.create(path = output_dir)
  if (!is.null(zip_path) && grepl("report_", filename)) {
    tryCatch(
      expr = zip::unzip(zip_path, exdir = output_dir, junkpaths = TRUE),
      warning = function(cond) {
        shiny::showNotification(
          ui = "Unzipping folder warning!",
          action = "Please contact app developer",
          type = "warning"
        )
      },
      error = function(cond) {
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
    shiny::showNotification("Failed to load the archiver file.", type = "error")
  }
}
