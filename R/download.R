#' Download report button module
#'
#' @description
#'
#' Provides a button that triggers downloading a report.
#'
#' For more information, refer to the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name download_report_button
#'
#' @param id (`character(1)`) this `shiny` module's id.
#' @param label (`character(1)`) label before the icon. By default `NULL`.
#' @param reporter (`Reporter`) instance.
#' @param label (`character(1)`) label of the button. By default it is empty.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname download_report_button
#' @export
download_report_button_ui <- function(id, label = NULL) {
  checkmate::assert_string(label, null.ok = TRUE)
  .outline_button(shiny::NS(id, "download_button"), label = label, icon = "download")
}

#' @rdname download_report_button
#' @export
download_report_button_srv <- function(id,
                                       reporter,
                                       global_knitr = getOption("teal.reporter.global_knitr"),
                                       rmd_output = getOption("teal.reporter.rmd_output"),
                                       rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args")) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c(
      "html_document", "pdf_document",
      "powerpoint_presentation", "word_document"
    ),
    empty.ok = FALSE
  )
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_names(
    names(rmd_yaml_args),
    subset.of = c("author", "title", "date", "output", "toc"),
    must.include = "output"
  )
  checkmate::assert_true(rmd_yaml_args[["output"]] %in% rmd_output)

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("download_button"))

    ns <- session$ns

    download_modal <- function() {
      nr_cards <- length(reporter$get_cards())
      downb <- shiny::downloadButton(
        outputId = ns("download_data"),
        label = "Download",
        class = c(
          "btn", "teal-reporter", "download-ok", "btn-primary", "shiny-download-link",
          if (nr_cards == 0) "disabled"
        ),
        icon = shiny::icon("download")
      )

      shiny::tags$div(
        class = "teal-reporter reporter-modal",
        .custom_css_dependency(),
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Download the Report"),
          shiny::tags$hr(),
          if (length(reporter$get_cards()) == 0) {
            shiny::tags$div(
              shiny::tags$p(
                class = "text-danger",
                shiny::tags$strong("No Cards Added")
              ),
              shiny::tags$br()
            )
          } else {
            shiny::tags$div(
              shiny::tags$p(
                class = "text-success",
                shiny::tags$strong(paste("Number of cards: ", nr_cards))
              ),
              shiny::tags$br()
            )
          },
          reporter_download_inputs(
            rmd_yaml_args = rmd_yaml_args,
            rmd_output = rmd_output,
            showrcode = any_rcode_block(reporter),
            session = session
          ),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              `data-bs-dismiss` = "modal",
              NULL,
              "Dismiss"
            ),
            shiny::tags$a(
              id = ns("download_data"),
              class = "btn btn-primary shiny-download-link",
              href = "",
              target = "_blank",
              download = NA,
              shiny::icon("download"),
              "Download"
            )
          )
        )
      )
    }

    shiny::observeEvent(reporter$get_cards(), {
      shinyjs::toggleState(length(reporter$get_cards()) > 0, id = "download_button")
    })

    shiny::observeEvent(input$download_button, shiny::showModal(download_modal()))

    output$download_data <- shiny::downloadHandler(
      filename = function() paste0(.report_identifier(reporter), ".zip"),
      content = function(file) {
        shiny::showNotification("Rendering and Downloading the document.")
        shinybusy::block(id = ns("download_data"), text = "", type = "dots")
        rmd_yaml_with_inputs <- lapply(names(rmd_yaml_args), function(x) input[[x]])
        names(rmd_yaml_with_inputs) <- names(rmd_yaml_args)
        if (is.logical(input$showrcode)) global_knitr[["echo"]] <- input$showrcode
        report_render_and_compress(
          reporter = reporter,
          rmd_yaml_args = rmd_yaml_with_inputs,
          global_knitr = global_knitr,
          file = file
        )
        shinybusy::unblock(id = ns("download_data"))
      },
      contentType = "application/zip"
    )
  })
}

#' Render the report
#'
#' Render the report and zip the created directory.
#'
#' @param reporter (`Reporter`) instance.
#' @param rmd_yaml_args (`named list`) with `Rmd` `yaml` header fields and their values.
#' @param global_knitr (`list`) a global `knitr` parameters, like echo.
#' But if local parameter is set it will have priority.
#' @param file (`character(1)`) where to copy created zip file.
#'
#' @return `file` argument, invisibly.
#'
#' @keywords internal
report_render_and_compress <- function(reporter, rmd_yaml_args, global_knitr, file = tempfile()) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_string(file)

  tmp_dir <- file.path(tempdir(), .report_identifier(reporter))

  cards_combined <- reporter$get_blocks()
  metadata(cards_combined) <- utils::modifyList(metadata(cards_combined), rmd_yaml_args)

  tryCatch(
    render(
      input = cards_combined,
      output_dir = tmp_dir,
      global_knitr = global_knitr,
      quiet = TRUE
    ),
    warning = function(cond) message("Render document warning: ", cond),
    error = function(cond) {
      message("Render document error: ", cond)
      do.call("return", args = list(), envir = parent.frame(2))
    }
  )

  tryCatch(
    reporter$to_jsondir(tmp_dir),
    warning = function(cond) message("Archive document warning: ", cond),
    error = function(cond) message("Archive document error: ", cond)
  )

  tryCatch(
    reporter$write_figures(tmp_dir),
    warning = function(cond) message("Save reporter images warning: ", cond),
    error = function(cond) message("Save reporter images error: ", cond)
  )

  temp_zip_file <- tempfile(fileext = ".zip")
  tryCatch(
    zip::zipr(temp_zip_file, tmp_dir),
    warning = function(cond) message("Zipping folder warning: ", cond),
    error = function(cond) message("Zipping folder error: ", cond)
  )

  tryCatch(
    {
      file.copy(temp_zip_file, file)
      unlink(tmp_dir, recursive = TRUE)
    },
    warning = function(cond) message("Copying file warning: ", cond),
    error = function(cond) message("Copying file error: ", cond)
  )
  invisible(file)
}

#' Get the custom list of UI inputs
#'
#' @param rmd_output (`character`) vector with `rmarkdown` output types,
#' by default all possible `pdf_document`, `html_document`, `powerpoint_presentation`, and `word_document`.
#' If vector is named then those names will appear in the `UI`.
#' @param rmd_yaml_args (`named list`) with `Rmd` `yaml` header fields and their default values.
#' This `list` will result in the custom subset of UI inputs for the download reporter functionality.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document", toc = FALSE)`.
#' The `list` must include at least `"output"` field.
#' The default value for `"output"` has to be in the `rmd_output` argument.
#'
#' @keywords internal
reporter_download_inputs <- function(rmd_yaml_args, rmd_output, showrcode, session) {
  shiny::tagList(
    lapply(names(rmd_yaml_args), function(e) {
      switch(e,
        author = shiny::textInput(session$ns("author"), label = "Author:", value = rmd_yaml_args$author),
        title = shiny::textInput(session$ns("title"), label = "Title:", value = rmd_yaml_args$title),
        date = shiny::dateInput(session$ns("date"), "Date:", value = rmd_yaml_args$date),
        output = shiny::tags$div(
          shinyWidgets::pickerInput(
            inputId = session$ns("output"),
            label = "Choose a document type: ",
            choices = rmd_output,
            selected = rmd_yaml_args$output
          )
        ),
        toc = shiny::checkboxInput(session$ns("toc"), label = "Include Table of Contents", value = rmd_yaml_args$toc)
      )
    }),
    if (showrcode) {
      shiny::checkboxInput(
        session$ns("showrcode"),
        label = "Include R Code",
        value = FALSE
      )
    }
  )
}

#' @noRd
#' @keywords internal
any_rcode_block <- function(reporter) {
  any(vapply(reporter$get_blocks(), inherits, logical(1), what = "code_chunk"))
}

.report_identifier <- function(reporter) {
  id <- paste0("_", reporter$get_id()) %||% ""
  timestamp <- format(Sys.time(), "_%y%m%d%H%M%S")
  sprintf("reporter%s%s", id, timestamp)
}
