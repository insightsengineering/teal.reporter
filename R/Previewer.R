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
    add_previewer_js(ns),
    add_previewer_css(),
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
          actionButton(
            ns("download_data_prev"),
            class = "teal-reporter simple_report_button",
            shiny::tags$span("Download Report", shiny::icon("download"))
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

        if (all(vapply(cards, inherits, logical(1), "ReportCard"))) {
          shiny::tags$div(
            class = "panel-group accordion",
            id = "reporter_previewer_panel",
            lapply(seq_along(cards), function(ic) {
              previewer_collapse_item(ic, cards[[ic]]$get_name(), cards[[ic]]$get_content())
            })
          )
          } else if (all(vapply(cards, inherits, logical(1), "ReportDocument"))) {
            shiny::tags$div(
              class = "panel-group accordion",
              id = "reporter_previewer_panel",
              lapply(seq_along(cards), function(ic) {
                previewer_collapse_item(ic, attr(cards[[ic]], "name"), NULL) # TODO, substitute NULL with report content
              })
            )
        }
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

    shiny::observeEvent(input$remove_card_ok, {
      reporter$remove_cards(input$card_remove_id)
      shiny::removeModal()
    })

    shiny::observeEvent(input$card_up_id, {
      if (input$card_up_id > 1) {
        reporter$swap_cards(
          as.integer(input$card_up_id),
          as.integer(input$card_up_id - 1)
        )
      }
    })

    shiny::observeEvent(input$card_down_id, {
      if (input$card_down_id < length(reporter$get_cards())) {
        reporter$swap_cards(
          as.integer(input$card_down_id),
          as.integer(input$card_down_id + 1)
        )
      }
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
  })
}

#' @noRd
#' @keywords internal
block_to_html <- function(b) {
  if (inherits(b, 'ReportDocument')) {
    # This function knows how to reshape blocks into html, based on the block class.
    # ReportDocument is just an S3 list of R objects (mostly character(), ggplot, table)
    # We can decide how to handle conversion of each element into HTML, based on:
    # a) object name - then we can have custom configuration file that can be extended by user
    # b) object class - but how we distinguish code stored as character and text stored as character.

    # Below is the WIP-implementation based on object classes, where I only support:
    # 1) character(),
    # 2) ggplot
    # 3) data.frame
    # for now.

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
add_previewer_css <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/Previewer.css", package = "teal.reporter")))
    ),
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    )
  )
}

#' @noRd
#' @keywords internal
add_previewer_js <- function(ns) {
  shiny::singleton(
    shiny::tags$head(shiny::tags$script(
      shiny::HTML(sprintf('
          $(document).ready(function(event) {
            $("body").on("click", "span.card_remove_id", function() {
              let val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
            });

            $("body").on("click", "span.card_up_id", function() {
              let val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
            });

             $("body").on("click", "span.card_down_id", function() {
              let val = $(this).data("cardid");
              Shiny.setInputValue("%s", val, {priority: "event"});
             });
          });
         ', ns("card_remove_id"), ns("card_up_id"), ns("card_down_id")))
    ))
  )
}

#' @noRd
#' @keywords internal
nav_previewer_icon <- function(name, icon_name, idx, size = 1L) {
  checkmate::assert_string(name)
  checkmate::assert_string(icon_name)
  checkmate::assert_int(size)

  shiny::tags$span(
    class = paste(name, "icon_previewer"),
    # data field needed to record clicked card on the js side
    `data-cardid` = idx,
    shiny::icon(icon_name, sprintf("fa-%sx", size))
  )
}

#' @noRd
#' @keywords internal
nav_previewer_icons <- function(idx, size = 1L) {
  shiny::tags$span(
    class = "preview_card_control",
    nav_previewer_icon(name = "card_remove_id", icon_name = "xmark", idx = idx, size = size),
    nav_previewer_icon(name = "card_up_id", icon_name = "arrow-up", idx = idx, size = size),
    nav_previewer_icon(name = "card_down_id", icon_name = "arrow-down", idx = idx, size = size)
  )
}

#' @noRd
#' @keywords internal
previewer_collapse_item <- function(idx, card_name, card_blocks) {
  shiny::tags$div(.renderHook = function(x) {
    # get bs version
    version <- get_bs_version()

    if (version == "3") {
      shiny::tags$div(
        id = paste0("panel_card_", idx),
        class = "panel panel-default",
        shiny::tags$div(
          class = "panel-heading overflow-auto",
          shiny::tags$div(
            class = "panel-title",
            shiny::tags$span(
              nav_previewer_icons(idx = idx),
              shiny::tags$a(
                class = "accordion-toggle block py-3 px-4 -my-3 -mx-4",
                `data-toggle` = "collapse",
                `data-parent` = "#reporter_previewer_panel",
                href = paste0("#collapse", idx),
                shiny::tags$h4(paste0("Card ", idx, ": ", card_name), shiny::icon("caret-down"))
              )
            )
          )
        ),
        shiny::tags$div(
          id = paste0("collapse", idx), class = "collapse out",
          shiny::tags$div(
            class = "panel-body",
            shiny::tags$div(
              id = paste0("card", idx),
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
    } else {
      shiny::tags$div(
        id = paste0("panel_card_", idx),
        class = "card",
        shiny::tags$div(
          class = "overflow-auto",
          shiny::tags$div(
            class = "card-header",
            shiny::tags$span(
              nav_previewer_icons(idx = idx),
              shiny::tags$a(
                class = "accordion-toggle block py-3 px-4 -my-3 -mx-4",
                # bs4
                `data-toggle` = "collapse",
                # bs5
                `data-bs-toggle` = "collapse",
                href = paste0("#collapse", idx),
                shiny::tags$h4(
                  paste0("Card ", idx, ": ", card_name),
                  shiny::icon("caret-down")
                )
              )
            )
          )
        ),
        shiny::tags$div(
          id = paste0("collapse", idx),
          class = "collapse out",
          # bs4
          `data-parent` = "#reporter_previewer_panel",
          # bs5
          `data-bs-parent` = "#reporter_previewer_panel",
          shiny::tags$div(
            class = "card-body",
            shiny::tags$div(
              id = paste0("card", idx),
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
  })
}
