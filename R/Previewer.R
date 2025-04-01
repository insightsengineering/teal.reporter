#' Report previewer module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Module offers functionalities to visualize, manipulate,
#' and interact with report cards that have been added to a report.
#' It includes a previewer interface to see the cards and options to modify the report before downloading.
#'
#' Cards are saved by the `shiny` bookmarking mechanism.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name reporter_previewer
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @param previewer_buttons (`character`) set of modules to include with `c("download", "load", "reset")` possible
#' values and `"download"` is required.
#' Default `c("download", "load", "reset")`
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname reporter_previewer
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::tagList(
      shiny::tags$div(
        class = "col-md-3",
        shiny::tags$div(class = "well", shiny::uiOutput(ns("encoding")))
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

#' @rdname reporter_previewer
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = c(
                                     "html" = "html_document", "pdf" = "pdf_document",
                                     "powerpoint" = "powerpoint_presentation",
                                     "word" = "word_document"
                                   ),
                                   rmd_yaml_args = list(
                                     author = "NEST", title = "Report",
                                     date = as.character(Sys.Date()), output = "html_document",
                                     toc = FALSE
                                   ),
                                   previewer_buttons = c("download", "load", "reset")) {
  checkmate::assert_subset(previewer_buttons, c("download", "load", "reset"), empty.ok = FALSE)
  checkmate::assert_true("download" %in% previewer_buttons)
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
    shiny::setBookmarkExclude(c(
      "card_remove_id", "card_down_id", "card_up_id", "remove_card_ok", "showrcode", "download_data_prev",
      "load_reporter_previewer", "load_reporter"
    ))

    session$onBookmark(function(state) {
      reporterdir <- file.path(state$dir, "reporter")
      dir.create(reporterdir)
      reporter$to_jsondir(reporterdir)
    })
    session$onRestored(function(state) {
      reporterdir <- file.path(state$dir, "reporter")
      reporter$from_jsondir(reporterdir)
    })

    ns <- session$ns

    reset_report_button_srv("resetButtonPreviewer", reporter)

    output$encoding <- shiny::renderUI({
      reporter$get_reactive_add_card()
      nr_cards <- length(reporter$get_cards())

      previewer_buttons_list <- list(
        download = htmltools::tagAppendAttributes(
          shiny::downloadButton(
            ns("download_data_prev"),
            label = "Download Report",
            icon = shiny::icon("download")
          ),
          class = if (nr_cards) "" else "disabled"
        ),
        load = shiny::actionButton(
          ns("load_reporter_previewer"),
          class = "teal-reporter simple_report_button",
          `data-val` = shiny::restoreInput(id = ns("load_reporter_previewer"), default = NULL),
          shiny::tags$span(
            "Load Report", shiny::icon("upload")
          )
        ),
        reset = reset_report_button_ui(ns("resetButtonPreviewer"), label = "Reset Report")
      )

      shiny::tags$div(
        id = "previewer_reporter_encoding",
        shiny::tags$h3("Download the Report"),
        shiny::tags$hr(),
        reporter_download_inputs(
          rmd_yaml_args = rmd_yaml_args,
          rmd_output = rmd_output,
          showrcode = any_rcode_block(reporter),
          session = session
        ),
        shiny::tags$div(
          id = "previewer_reporter_buttons",
          class = "previewer_buttons_line",
          lapply(previewer_buttons_list[previewer_buttons], shiny::tags$div)
        )
      )
    })

    output$pcards <- shiny::renderUI({
      reporter$get_reactive_add_card()
      input$card_remove_id
      input$card_down_id
      input$card_up_id

      cards <- reporter$get_cards()

      if (length(cards)) {
        tags$div(
          tags$div(
            class = "panel-group accordion",
            id = "reporter_previewer_panel",
            setNames(
              lapply(names(cards), function(card_name) {
                if (inherits(cards[[card_name]], "ReportCard")) {
                  previewer_collapse_item(card_name, cards[[card_name]]$get_content())
                } else if (inherits(cards[[card_name]], "ReportDocument")) {
                  previewer_collapse_item(card_name, cards[[card_name]], ns)
                }
              }),
              names(cards)
            )
          ),
          sortable::sortable_js(
            "reporter_previewer_panel",
            options = sortable::sortable_options(
              group = list(
                name = "reporter_cards",
                put = TRUE
              ),
              sort = TRUE,
              handle = ".accordion-header",
              onSort = sortable::sortable_js_capture_input(ns("reporter_cards_orders"))
            )
          )
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

    observeEvent(input$reporter_cards_orders, {
      reporter$reorder_cards(input$reporter_cards_orders)
    })

    shiny::observeEvent(input$load_reporter_previewer, {
      nr_cards <- length(reporter$get_cards())
      shiny::showModal(
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Load the Reporter"),
          shiny::tags$hr(),
          shiny::fileInput(ns("archiver_zip"), "Choose Reporter File to Load (a zip file)",
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
              id = ns("load_reporter"),
              type = "button",
              class = "btn btn-primary action-button",
              `data-val` = shiny::restoreInput(id = ns("load_reporter"), default = NULL),
              NULL,
              "Load"
            )
          )
        )
      )
    })

    shiny::observeEvent(input$load_reporter, {
      switch("JSON",
        JSON = load_json_report(reporter, input$archiver_zip[["datapath"]], input$archiver_zip[["name"]]),
        stop("The provided Reporter file format is not supported")
      )

      shiny::removeModal()
    })

    shiny::observeEvent(input$card_remove_id, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Remove the Report Card",
          shiny::tags$p(
            shiny::HTML(
              sprintf(
                "Do you really want to remove <strong>the card %s</strong> from the Report?",
                input$card_remove_id
              )
            )
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
            shiny::actionButton(ns("remove_card_ok"), "OK", class = "btn-danger")
          )
        )
      )
    })

    # Implement remove card using a custom delete icon on the accordion
    shiny::observeEvent(input$remove_card_ok, {
      reporter$remove_cards(input$card_remove_id)
      shiny::removeModal()
    })

    output$download_data_prev <- shiny::downloadHandler(
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
        shinybusy::block(id = ns("download_data_prev"), text = "", type = "dots")

        yaml_header <- lapply(names(rmd_yaml_args), function(x) input[[x]])
        names(yaml_header) <- names(rmd_yaml_args)
        if (is.logical(input$showrcode)) global_knitr[["echo"]] <- input$showrcode

        if (identical("pdf_document", yaml_header$output) &&
            inherits(try(system2("pdflatex", "--version", stdout = TRUE), silent = TRUE), "try-error")) {
          shiny::showNotification(
            ui = "pdflatex is not available so the pdf_document could not be rendered. Please use other output type.",
            action = "Please contact app developer",
            type = "error"
          )
          stop("pdflatex is not available so the pdf_document could not be rendered.")
        }
        yaml_content <- as_yaml_auto(yaml_header)

        tryCatch(
          output_dir <- report_render(reporter, yaml_content, global_knitr),
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

        tryCatch(
          archiver_dir <- reporter$to_jsondir(output_dir),
          warning = function(cond) {
            print(cond)
            shiny::showNotification(
              ui = "Archive document warning!",
              action = "Please contact app developer",
              type = "warning"
            )
          },
          error = function(cond) {
            print(cond)
            shiny::showNotification(
              ui = "Archive document error!",
              action = "Please contact app developer",
              type = "error"
            )
          }
        )

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

        shinybusy::unblock(id = ns("download_data_prev"))
      },
      contentType = "application/zip"
    )

  })
}

#' @noRd
#' @keywords internal
block_to_html <- function(b, ...) {
  UseMethod("block_to_html")
}

#' @method block_to_html default
#' @keywords internal
block_to_html.default <- function(b, ...) {
  shiny::HTML(commonmark::markdown_html(b, extensions = TRUE))
}

#' @method block_to_html ContentBlock
#' @keywords internal
block_to_html.ContentBlock <- function(b, ...) {
  b_content <- b$get_content()

  UseMethod("block_to_html", b) # Further dispatch for subclasses
}

#' @method block_to_html TextBlock
#' @keywords internal
block_to_html.TextBlock <- function(b, ...) {
  b_content <- b$get_content()
  switch(
    b$get_style(),
    header1 = shiny::tags$h1(b_content),
    header2 = shiny::tags$h2(b_content),
    header3 = shiny::tags$h3(b_content),
    header4 = shiny::tags$h4(b_content),
    verbatim = shiny::tags$pre(b_content),
    shiny::tags$pre(b_content)
  )
}

#' @method block_to_html RcodeBlock
#' @keywords internal
block_to_html.RcodeBlock <- function(b, ...) {
  panel_item("R Code", shiny::tags$pre(b$get_content()))
}

#' @method block_to_html PictureBlock
#' @keywords internal
block_to_html.PictureBlock <- function(b, ...) {
  shiny::tags$img(src = knitr::image_uri(b$get_content()))
}

#' @method block_to_html TableBlock
#' @keywords internal
block_to_html.TableBlock <- function(b, ...) {
  b_table <- readRDS(b$get_content())
  shiny::tags$pre(flextable::htmltools_value(b_table))
}

#' @method block_to_html NewpageBlock
#' @keywords internal
block_to_html.NewpageBlock <- function(b, ...) {
  shiny::tags$br()
}

#' @method block_to_html HTMLBlock
#' @keywords internal
block_to_html.HTMLBlock <- function(b, ...) {
  b$get_content()
}

#' @method block_to_html rtables
#' @keywords internal
block_to_html.rtables <- function(b, ...) {
  shiny::tags$pre(flextable::htmltools_value(to_flextable(b)))
}

#' @method block_to_html gg
#' @keywords internal
block_to_html.gg <- function(b, ...) {
  tmpfile <- tempfile(fileext = ".png")
  ggsave(tmpfile, plot = b, width = 5, height = 4, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method block_to_html code_chunk
#' @keywords internal
block_to_html.code_chunk <- function(b, ...) {
  shiny::tags$pre(b)
}

#' @method block_to_html TableTree
#' @keywords internal
block_to_html.TableTree <- block_to_html.rtables

#' @method block_to_html ElementaryTable
#' @keywords internal
block_to_html.ElementaryTable <- block_to_html.rtables

#' @method block_to_html rlisting
#' @keywords internal
block_to_html.rlisting <- block_to_html.rtables

#' @method block_to_html data.frame
#' @keywords internal
block_to_html.data.frame <- block_to_html.rtables


#' @noRd
#' @keywords internal
previewer_collapse_item <- function(card_name, card_blocks, ns = NULL, open = FALSE) {
  tags$div(
    `data-rank-id` = card_name,
    bslib::accordion(
      open = open,
      bslib::accordion_panel(
        title = card_name,
        if (!is.null(ns)) {
          tagList(
            tags$div(
              style = "display: flex; justify-content: flex-end; align-items: center;",
              actionButton(
                inputId = ns(paste0("edit_card_", card_name)),
                label = "Edit",
                icon = shiny::icon("edit"),
                class = "btn btn-warning btn-sm"
              )
            ),
            tags$hr()
          )
        },
        tags$div(
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



