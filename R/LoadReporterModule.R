#' User Interface to Load `Reporter`
#' @description `r lifecycle::badge("experimental")`
#' Button to upload `ReporterCard`(s) to the `Reporter`.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny::tagList`
#' @export
report_load_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    # shiny::actionButton(
    #   ns("reporter_load"),
    #   class = "teal-reporter simple_report_button btn-primary",
    #   label = "Load Report",
    #   icon = shiny::icon("upload")
    # )
    shiny::fileInput(
      ns("archiver_zip"), "Choose file (.zip)",
      multiple = FALSE,
      accept = c(".zip")
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

      shiny::observeEvent(input$archiver_zip, {
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
