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
          shiny::tags$h3("Load the Archiver"),
          shiny::tags$hr(),
          shiny::selectInput(ns("archiver_format"), label = "Archiver Format", choices = "JSON", selected = "JSON"),
          shiny::fileInput(ns("archiver_zip"), "Choose Archiver File to Load (a zip file)",
                    multiple = FALSE,
                    accept = c(".zip")),
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
        tmp_dir <- tempdir()
        output_dir <- file.path(tmp_dir, sprintf("archiver_load_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
        dir.create(path = output_dir)
        if (!is.null(input$archiver_zip[["datapath"]])) {
          tryCatch(
            expr = zip::unzip(input$archiver_zip[["datapath"]], exdir = output_dir, junkpaths = TRUE),
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
          reporter$from_jsondir(output_dir)
          shiny::removeModal()
        }
      })
    }
  )
}

#' Archiver User Interface
#' @description `r lifecycle::badge("experimental")`
#' button for adding views/cards to the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny::tagList`
#' @export
archiver_save_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$button(
      id = ns("archiver_reporter_save"),
      type = "button",
      class = "simple_report_button btn btn-primary action-button",
      title = "Save",
      `data-val` = shiny::restoreInput(id = ns("archiver_reporter_save"), default = NULL),
      NULL,
      shiny::tags$span(
        shiny::icon("save")
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
archiver_save_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      archiver_modal <- function() {
        nr_cards <- length(reporter$get_cards())
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Save the Archiver"),
          shiny::tags$hr(),
          shiny::tags$h5("Save Your Work for Later"),
          shiny::selectInput(ns("archiver_format"), label = "Archiver Format", choices = "JSON", selected = "JSON"),
          footer = shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-danger",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::tags$a(
              id = ns("save_archiver"),
              class = paste("btn btn-primary shiny-download-link", if (nr_cards) NULL else "disabled"),
              style = if (nr_cards) NULL else "pointer-events: none;",
              href = "",
              target = "_blank",
              download = NA,
              shiny::icon("download"),
              "Save"
            )
          )
        )
      }

      shiny::observeEvent(input$archiver_reporter_save, {
        shiny::showModal(archiver_modal())
      })

      output$save_archiver <- shiny::downloadHandler(
        filename = function() {
          paste("archiver_", format(Sys.time(), "%y%m%d%H%M%S"), ".zip", sep = "")
        },
        content = function(file) {
          shiny::showNotification("Compressing and Downloading the Archive.")
          tmp_dir <- tempdir()
          output_dir <- file.path(tmp_dir, sprintf("archiver_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
          dir.create(path = output_dir)
          archiver_dir <- reporter$to_jsondir(output_dir)
          temp_zip_file <- tempfile(fileext = ".zip")
          tryCatch(
            expr = zip::zipr(temp_zip_file, archiver_dir),
            warning = function(cond) {
              shiny::showNotification(
                ui = "Zipping folder warning!",
                action = "Please contact app developer",
                type = "warning"
              )
            },
            error = function(cond) {
              shiny::showNotification(
                ui = "Zipping folder error!",
                action = "Please contact app developer",
                type = "error"
              )
            }
          )

          tryCatch(
            expr = file.copy(temp_zip_file, file),
            warning = function(cond) {
              shiny::showNotification(
                ui = "Copying file warning!",
                action = "Please contact app developer",
                type = "warning"
              )
            },
            error = function(cond) {
              shiny::showNotification(
                ui = "Copying file error!",
                action = "Please contact app developer",
                type = "error"
              )
            }
          )
        },
        contentType = "application/zip"
      )
    }
  )
}
