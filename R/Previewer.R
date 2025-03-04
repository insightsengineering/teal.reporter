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
          downloadButton(
            ns("download_data_prev"),
            label = "Download Report",
            icon = shiny::icon("download")
          ),
          class = if (nr_cards) "" else "disabled"
        ),
        load = actionButton(
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

    output$mcards <- shiny::renderUI({
      reporter$get_reactive_add_card()
      input$card_remove_id
      input$card_down_id
      input$card_up_id

      cards <- reporter$get_cards()

      if (length(cards)) {
        shiny::tags$div(
          class = "panel-group accordion",
          id = "reporter_previewer_panel",
          lapply(seq_along(cards), function(ic) {
            if (inherits(cards[[ic]], "ReportDocument")) {
              shiny::textAreaInput(
                inputId = ns(paste0("text_card", ic)),
                label = paste0("markdown input for card: ", attr(cards[[ic]], "name")),
                value = paste(unlist(cards[[ic]]), collapse = "\n"),
                width = "100%",
                height = "800px"
              )
            }
          })
        )
      }
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
        input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
        names(input_list) <- names(rmd_yaml_args)
        if (is.logical(input$showrcode)) global_knitr[["echo"]] <- input$showrcode
        report_render_and_compress(reporter, input_list, global_knitr, file)
        shinybusy::unblock(id = ns("download_data_prev"))
      },
      contentType = "application/zip"
    )

    observe({
      edit_buttons <- grep("^edit_card_", names(input), value = TRUE)

      if (length(edit_buttons) > 0) {
        for (btn in edit_buttons) {
          observeEvent(input[[btn]], {
            card_name <- sub("^edit_card_", "", btn)
            showModal(
              modalDialog(
                title = paste("Edit Card:", card_name),
                textAreaInput(
                  inputId = session$ns(paste0("edit_text_", card_name)),
                  label = "Modify Content:",
                  value = paste(unlist(reporter$get_cards()[[card_name]]), collapse = "\n"),
                  width = "100%",
                  height = "400px"
                ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(session$ns(paste0("save_edit_", card_name)), "Save", class = "btn-primary")
                )
              )
            )
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
        }
      }
    })

    observe({
      save_buttons <- grep("^save_edit_", names(input), value = TRUE)

      if (length(save_buttons) > 0) {
        for (btn in save_buttons) {
          observeEvent(input[[btn]], {
            card_name <- sub("^save_edit_", "", btn)
            edited_content <- input[[paste0("edit_text_", card_name)]]
            edited_report_document <- report_document(edited_content) # TODO, maybe split for the same length? as in input?
            reporter$set_card_content(card_name, report_document(edited_report_document))

            output$pcards <- shiny::renderUI({
              reporter$get_reactive_add_card()
              cards <- reporter$get_cards()

              tags$div(
                tags$div(
                  class = "panel-group accordion",
                  id = "reporter_previewer_panel",
                  setNames(
                    lapply(card_name, function(card_name) { # refresh only this one card
                      previewer_collapse_item(card_name, cards[[card_name]], ns, open = TRUE)
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
            })

            removeModal()
            shiny::showNotification(paste("Card", card_name, "has been updated!"))
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
        }
      }
    })

  })
}

#' @noRd
#' @keywords internal
block_to_html <- function(b) {
  if (!inherits(b, "ContentBlock")) {
    shiny::HTML(commonmark::markdown_html(text = b, extensions = TRUE))
  } else {
    b_content <- b$get_content()
    if (inherits(b, "TextBlock")) {
      switch(b$get_style(),
        header1 = shiny::tags$h1(b_content),
        header2 = shiny::tags$h2(b_content),
        header3 = shiny::tags$h3(b_content),
        header4 = shiny::tags$h4(b_content),
        verbatim = shiny::tags$pre(b_content),
        shiny::tags$pre(b_content)
      )
    } else if (inherits(b, "RcodeBlock")) {
      panel_item("R Code", shiny::tags$pre(b_content))
    } else if (inherits(b, "PictureBlock")) {
      shiny::tags$img(src = knitr::image_uri(b_content))
    } else if (inherits(b, "TableBlock")) {
      b_table <- readRDS(b_content)
      shiny::tags$pre(
        flextable::htmltools_value(b_table)
      )
    } else if (inherits(b, "NewpageBlock")) {
      shiny::tags$br()
    } else if (inherits(b, "HTMLBlock")) {
      b_content
    } else {
      stop("Unknown block class")
    }
  }
}


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



