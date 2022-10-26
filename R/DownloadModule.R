#' Download Button Reporter User Interface
#' @description `r lifecycle::badge("experimental")`
#' button for downloading the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny::tagList`
#' @export
download_report_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$button(
      id = ns("download_button"),
      type = "button",
      class = "simple_report_button btn btn-primary action-button",
      title = "Downlaod",
      `data-val` = shiny::restoreInput(id = ns("download_button"), default = NULL),
      NULL,
      shiny::tags$span(
        shiny::icon("download")
      )
    )
  )
}

#' Download Button Server
#' @description `r lifecycle::badge("experimental")`
#' server for downloading the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#' @param rmd_output `character` vector with `rmarkdown` output types,
#' by default all possible `c("pdf_document", "html_document", "powerpoint_presentation", "word_document")`.
#' `pdflatex` has to be installed to use the `"pdf_document"` output. If vector is named then the names
#' will appear in the `UI`.
#' @param rmd_yaml_args `named list` vector with `Rmd` `yaml` header fields and their default values.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document")`.
#' Please update only values at this moment.
#' @return `shiny::moduleServer`
#' @export
download_report_button_srv <- function(id,
                                       reporter,
                                       rmd_output = c(
                                         "html" = "html_document", "pdf" = "pdf_document",
                                         "powerpoint" = "powerpoint_presentation", "word" = "word_document"
                                       ),
                                       rmd_yaml_args = list(
                                         author = "NEST", title = "Report",
                                         date = as.character(Sys.Date()), output = "html_document"
                                       )) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(rmd_output, c(
    "html_document", "pdf_document",
    "powerpoint_presentation", "word_document"
  ))
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_true(all(c("author", "title", "date", "output") %in% names(rmd_yaml_args)))

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      download_modal <- function() {
        nr_cards <- length(reporter$get_cards())
        downb <- shiny::tags$a(
          id = ns("download_data"),
          class = paste("btn btn-primary shiny-download-link", if (nr_cards) NULL else "disabled"),
          style =  if (nr_cards) NULL else "pointer-events: none;",
          href = "",
          target = "_blank",
          download = NA,
          shiny::icon("download"),
          "Download"
        )
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Download the Report"),
          shiny::tags$hr(),
          if (length(reporter$get_cards()) == 0) {
            shiny::tags$div(
              class = "mb-4",
              shiny::tags$p(
                class = "text-danger",
                shiny::tags$strong("No Cards Added")
              )
            )
          } else {
            shiny::tags$div(
              class = "mb-4",
              shiny::tags$p(
                class = "text-success",
                shiny::tags$strong(paste("Number of cards: ", nr_cards))
              ),
            )
          },
          shiny::textInput(ns("author"), label = "Author:", value = rmd_yaml_args$author),
          shiny::textInput(ns("title"), label = "Title:", value = rmd_yaml_args$title),
          shiny::dateInput(ns("date"), "Date:", value = rmd_yaml_args$date),
          if (any_rcode_fun(reporter)) {
            shiny::checkboxInput(
              ns("showrcode"),
              label = "Include Show R Code",
              value = FALSE
            )
          },
          shiny::tags$div(
            shinyWidgets::pickerInput(
              inputId = ns("output"),
              label = "Choose a document type: ",
              choices = rmd_output,
              selected = rmd_yaml_args$output
            )
          ),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-danger",
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

      output$download_data <- shiny::downloadHandler(
        filename = function() {
          paste("report_", format(Sys.time(), "%y%m%d%H%M%S"), ".zip", sep = "")
        },
        content = function(file) {
          shiny::showNotification("Rendering and Downloading the document.")
          input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
          names(input_list) <- names(rmd_yaml_args)
          if (!is.null(input$showrcode)) input_list$params <- list(showrcode = isTRUE(input$showrcode))
          report_render_and_compress(reporter, input_list, file)
        },
        contentType = "application/zip"
      )
    }
  )
}

#' Render the Report
#' @description render the report and zip the created directory.
#' @param reporter [`Reporter`] instance.
#' @param input_list `list` like shiny input converted to a regular named list.
#' @param file `character` where to copy the returned directory.
#' @return `file` argument, invisibly.
#' @keywords internal
report_render_and_compress <- function(reporter, input_list, file = tempdir()) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_list(input_list, names = "named")
  checkmate::assert_string(file)

  if (identical("pdf_document", input_list$output) &&
    inherits(try(system2("pdflatex", "--version", stdout = TRUE), silent = TRUE), "try-error")) {
    shiny::showNotification(
      ui = "pdflatex is not available so the pdf_document could not be rendered. Please use other output type.",
      action = "Please contact app developer",
      type = "error"
    )
    stop("pdflatex is not available so the pdf_document could not be rendered.")
  }

  yaml_header <- as_yaml_auto(input_list)
  renderer <- Renderer$new()

  tryCatch(
    renderer$render(reporter$get_blocks(), yaml_header),
    warning = function(cond) {
      shiny::showNotification(
        ui = "Render document warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      shiny::showNotification(
        ui = "Render document error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  temp_zip_file <- tempfile(fileext = ".zip")
  tryCatch(
    expr = zip::zipr(temp_zip_file, renderer$get_output_dir()),
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

  rm(renderer)
  invisible(file)
}


#' @keywords internal
any_rcode_fun <- function(reporter){
  any(
    vapply(
      Filter(
        function(e) inherits(e, "TextBlock"),
        reporter$get_blocks()
      ),
      function(b) b$get_style() == "rcode",
      logical(1)
    )
  )
}

