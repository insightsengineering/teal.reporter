#' Download report button module
#'
#' @description `r lifecycle::badge("experimental")`
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
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname download_report_button
#' @export
download_report_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    actionButton(
      ns("download_button"),
      class = "teal-reporter simple_report_button btn-primary",
      title = "Download",
      `data-val` = shiny::restoreInput(id = ns("download_button"), default = NULL),
      shiny::tags$span(
        shiny::icon("download")
      )
    )
  )
}

#' @rdname download_report_button
#' @export
download_report_button_srv <- function(id,
                                       reporter,
                                       global_knitr = getOption("teal.reporter.global_knitr"),
                                       rmd_output = c(
                                         "html" = "html_document", "pdf" = "pdf_document",
                                         "powerpoint" = "powerpoint_presentation", "word" = "word_document"
                                       ),
                                       rmd_yaml_args = list(
                                         author = "NEST", title = "Report",
                                         date = as.character(Sys.Date()), output = "html_document",
                                         toc = FALSE
                                       )) {
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
      downb <- shiny::tags$a(
        id = ns("download_data"),
        class = paste("btn btn-primary shiny-download-link", if (nr_cards) NULL else "disabled"),
        style = if (nr_cards) NULL else "pointer-events: none;",
        href = "",
        target = "_blank",
        download = NA,
        shiny::icon("download"),
        "Download"
      )
      div(
        class = "teal-widgets reporter-modal",
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
          reporter_download_inputs(
            rmd_yaml_args = rmd_yaml_args,
            rmd_output = rmd_output,
            showrcode = any_rcode_block(reporter),
            session = session
          ),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            downb
          )
        )
      )
    }

    shiny::observeEvent(input$download_button, {
      shiny::showModal(download_modal())
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "report_",
          if (reporter$get_id() == "") NULL else paste0(reporter$get_id(), "_"),
          format(Sys.time(), "%y%m%d%H%M%S"),
          ".zip"
        )
      },
      content = function(file) {
        shiny::showNotification("Rendering and Downloading the document.")
        shinybusy::block(id = ns("download_data"), text = "", type = "dots")
        input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
        names(input_list) <- names(rmd_yaml_args)
        if (is.logical(input$showrcode)) global_knitr[["echo"]] <- input$showrcode
        report_render_and_compress(reporter, input_list, global_knitr, file)
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
#' @param input_list (`list`) like `shiny` input converted to a regular named list.
#' @param global_knitr (`list`) a global `knitr` parameters, like echo.
#' But if local parameter is set it will have priority.
#' @param file (`character(1)`) where to copy the returned directory.
#'
#' @return `file` argument, invisibly.
#'
#' @keywords internal
report_render_and_compress <- function(reporter, input_list, global_knitr, file = tempdir()) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_list(input_list, names = "named")
  checkmate::assert_string(file)

  if (
    identical("pdf_document", input_list$output) &&
      inherits(try(system2("pdflatex", "--version", stdout = TRUE), silent = TRUE), "try-error")
  ) {
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
    renderer$render(reporter$get_blocks(), yaml_header, global_knitr),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Render document warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Render document error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  output_dir <- renderer$get_output_dir()

  # tryCatch(
  #   archiver_dir <- reporter$to_jsondir(output_dir),
  #   warning = function(cond) {
  #     print(cond)
  #     shiny::showNotification(
  #       ui = "Archive document warning!",
  #       action = "Please contact app developer",
  #       type = "warning"
  #     )
  #   },
  #   error = function(cond) {
  #     print(cond)
  #     shiny::showNotification(
  #       ui = "Archive document error!",
  #       action = "Please contact app developer",
  #       type = "error"
  #     )
  #   }
  # )

  temp_zip_file <- tempfile(fileext = ".zip")
  tryCatch(
    expr = zip::zipr(temp_zip_file, output_dir),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Zipping folder warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
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
      print(cond)
      shiny::showNotification(
        ui = "Copying file warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
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
  cards <- reporter$get_cards()
  if (all(vapply(cards, inherits, logical(1), "ReportCard"))) {
    any(
      vapply(
        reporter$get_blocks(),
        function(e) inherits(e, "RcodeBlock"),
        logical(1)
      )
    )
  } else {
    FALSE
  }
}
