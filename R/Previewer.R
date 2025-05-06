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
      sortable::sortable_js(
        css_id = ns("reporter_cards"),
        options = sortable::sortable_options(
          onSort = sortable::sortable_js_capture_input(input_id = ns("reporter_cards_orders"))
        )
      ),
      reporter_previewer_encoding_ui(ns("encoding_panel")),
      shiny::tags$div(
        class = "col-md-9",
        shiny::tags$div(
          id = "reporter_previewer",
          bslib::accordion(id = ns("reporter_cards"), open = FALSE)
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

    reporter_previewer_encoding_srv(
      id = "encoding_panel",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args,
      previewer_buttons = previewer_buttons
    )

    current_cards <- reactiveVal()
    insert_cards <- reactiveVal()
    remove_cards <- reactiveVal()
    observeEvent(reporter$get_reactive_add_card(), {
      to_add <- reporter$get_cards()[!reporter$get_cards() %in% current_cards()] # because setdiff loses names
      to_remove <- current_cards()[!current_cards() %in% reporter$get_cards()]
      if (length(to_add)) insert_cards(to_add)
      if (length(to_remove)) remove_cards(to_remove)
      current_cards(reporter$get_cards())
    })

    observeEvent(insert_cards(), {
      cards <- insert_cards()
      lapply(names(cards), function(card_name) {
        bslib::accordion_panel_insert(
          id = "reporter_cards",
          reporter_previewer_card_ui(id = session$ns(card_name), card_name = card_name)
        )
        reporter_previewer_card_srv(
          id = card_name,
          reporter = reporter,
          card = cards[[card_name]]
        )
      })
    })

    observeEvent(remove_cards(), {
      cards <- remove_cards()
      lapply(names(cards), function(card_name) {
        bslib::accordion_panel_remove(id = "reporter_cards", target = card_name)
      })
    })

    shiny::observeEvent(input$reporter_cards_orders, {
      # todo: handle "" added by sortable::sortable_js_capture_input
      reporter$reorder_cards(setdiff(input$reporter_cards_orders, "")) # "" is added by sortable::sortable_js_capture_input
    })
  })
}

reporter_previewer_encoding_ui <- function(id) {
  ns <- NS(id)
  shiny::tags$div(
    class = "col-md-3",
    shiny::tags$div(class = "well", shiny::uiOutput(ns("encoding")))
  )
}

reporter_previewer_encoding_srv <- function(id,
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
  moduleServer(id, function(input, output, session) {
    output$encoding <- shiny::renderUI({
      reporter$get_reactive_add_card()
      nr_cards <- length(reporter$get_cards())

      previewer_buttons_list <- list(
        download = htmltools::tagAppendAttributes(
          shiny::downloadButton(
            session$ns("download_data_prev"),
            label = "Download Report",
            icon = shiny::icon("download")
          ),
          class = if (nr_cards) "" else "disabled"
        ),
        load = shiny::actionButton(
          session$ns("load_reporter_previewer"),
          class = "teal-reporter simple_report_button",
          `data-val` = shiny::restoreInput(id = session$ns("load_reporter_previewer"), default = NULL),
          shiny::tags$span(
            "Load Report", shiny::icon("upload")
          )
        ),
        reset = reset_report_button_ui(session$ns("resetButtonPreviewer"), label = "Reset Report")
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

reporter_previewer_card_ui <- function(id, card_name) {
  ns <- NS(id)
  bslib::accordion_panel(
    value = card_name,
    title = tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      tags$span(card_name),
        actionButton(
          inputId = ns("edit"),
          label = NULL,
          icon = shiny::icon("edit"),
          class = "btn btn-warning btn-sm",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("edit_card_clicked"),
            card_name
          )
        ),
        actionButton(
          inputId = ns("remove"),
          label = NULL,
          icon = shiny::icon("trash-alt"),
          class = "btn btn-danger btn-sm",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("delete_card_clicked"),
            card_name
        )
      )
    ),
    uiOutput(ns("card_content"))
  )
}

#' @param id (`character(1)`) card name
reporter_previewer_card_srv <- function(id, reporter, card) {
  # todo: card_name should be only on the server side
  moduleServer(id, function(input, output, session) {
    # to react to the changes in the card
    card_reactive <- reactiveVal(card)

    output$card_content <- renderUI(toHTML(card_reactive()))
    if (inherits(card, "ReportCard")) {
      shinyjs::hide("edit")
    }

    # editor
    editor_ui <- editor_ui(session$ns("editor"), x = card_reactive)
    new_card <- editor_srv("editor", x = card_reactive)

    output$add_text_element_button_ui <- renderUI({
      if (inherits(card_reactive(), "ReportDocument")) {
        actionButton(
          session$ns("add_text_element_action"),
          "Add Empty Text Element",
          class = "btn btn-info btn-sm mb-2"
        )
      }
    })

    observeEvent(input$add_text_element_action, {
      current_card_val <- card_reactive()
      current_card_val[[length(current_card_val) + 1L]] <- ""
      card_reactive(current_card_val)
    }, ignoreInit = TRUE)

    observeEvent(input$edit, {
      shiny::showModal(
        shiny::modalDialog(
          title = paste("Editing Card:", id),
          size = "l", easyClose = TRUE,
          shiny::tagList(
            editor_ui,
            uiOutput(session$ns("add_text_element_button_ui"))
          ),
          footer = shiny::tagList(
            actionButton(session$ns("edit_save"), label = "Save"),
            modalButton("Close")
          )
        )
      )
    })

    observeEvent(input$edit_save, {
      if (!identical(new_card(), card)) {
        reporter$replace_card(id = id, card = new_card)
        card_reactive(new_card())
      }
      shiny::removeModal()
    })

    # remove self from reporter
    observeEvent(input$remove, {
      reporter$remove_cards(ids = id)
    })
  })
}

#' @importFrom tools toHTML
#' @keywords internal
#' @export
toHTML.ReportCard <- function(x, ...) {
  lapply(x$get_content(), toHTML)
}

#' @keywords internal
#' @export
toHTML.ReportDocument <- function(x, ...) {
  lapply(x, toHTML)
}

#' @keywords internal
#' @export
toHTML.default <- function(x, ...) {
  shiny::HTML(commonmark::markdown_html(x, extensions = TRUE))
}

#' @keywords internal
#' @export
toHTML.ContentBlock <- function(x, ...) {
  UseMethod("toHTML", x$get_content()) # Further dispatch for subclasses
}

#' @keywords internal
#' @export
toHTML.TextBlock <- function(x, ...) {
  b_content <- x$get_content()
  switch(x$get_style(),
    header1 = shiny::tags$h1(b_content),
    header2 = shiny::tags$h2(b_content),
    header3 = shiny::tags$h3(b_content),
    header4 = shiny::tags$h4(b_content),
    verbatim = shiny::tags$pre(b_content),
    shiny::tags$pre(b_content)
  )
}

#' @keywords internal
#' @export
toHTML.RcodeBlock <- function(x, ...) {
  panel_item("R Code", shiny::tags$pre(x$get_content()))
}

#' @keywords internal
#' @export
toHTML.PictureBlock <- function(x, ...) {
  shiny::tags$img(src = knitr::image_uri(x$get_content()))
}

#' @keywords internal
#' @export
toHTML.TableBlock <- function(x, ...) {
  b_table <- readRDS(b$get_content())
  shiny::tags$pre(flextable::htmltools_value(b_table))
}

#' @keywords internal
#' @export
toHTML.NewpageBlock <- function(x, ...) {
  shiny::tags$br()
}

#' @keywords internal
#' @export
toHTML.HTMLBlock <- function(x, ...) {
  x$get_content()
}

#' @keywords internal
#' @export
toHTML.rtables <- function(x, ...) {
  shiny::tags$pre(flextable::htmltools_value(to_flextable(x)))
}

#' @keywords internal
#' @export
toHTML.gg <- function(x, ...) {
  tmpfile <- tempfile(fileext = ".png")
  ggsave(tmpfile, plot = x, width = 5, height = 4, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @keywords internal
#' @export
toHTML.code_chunk <- function(x, ...) {
  shiny::tags$pre(x)
}

#' @keywords internal
#' @export
toHTML.TableTree <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.ElementaryTable <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.rlisting <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.data.frame <- toHTML.rtables
