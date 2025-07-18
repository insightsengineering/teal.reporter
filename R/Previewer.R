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
preview_report_button_ui <- function(id, label = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shinyjs::disabled(
      .outline_button(
        ns("preview_button"),
        label = uiOutput(ns("preview_button_label")),
        icon = "file-earmark-text"
      )
    )
  )
}

#' @rdname reporter_previewer
#' @export
preview_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("preview_button"))

    observeEvent(reporter$get_reactive_add_card(), {
      if (reporter$get_reactive_add_card() > 0) {
        shinyjs::enable(id = "preview_button")
      } else {
        shinyjs::disable(id = "preview_button")
      }
    })

    output$preview_button_label <- renderUI({
      tags$span(
        "Preview",
        tags$span(
          class = "position-absolute badge rounded-pill bg-primary",
          style = "top: 5px; right: 5px;",
          reporter$get_reactive_add_card()
        )
      )
    })

    preview_modal <- function() {
      shiny::tags$div(
        class = "teal-widgets reporter-modal",
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          reporter_previewer_only_ui(session$ns("preview_content")),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            )
          )
        )
      )
    }

    shiny::observeEvent(input$preview_button, {
      shiny::showModal(preview_modal())
    })
    reporter_previewer_only_srv(id = "preview_content", reporter = reporter) # todo: change the name but keep private
  })
}

#' @rdname reporter_previewer
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  # lifecycle::deprecate_soft() todo:
  # lifecycle::deprecate_soft() todo:
  bslib::page_fluid(
    add_previewer_js(ns),
    add_previewer_css(),
    shiny::tagList(
      shiny::tags$div(
        class = "col-md-3",
        shiny::tags$div(
          class = "well",
          report_load_ui(ns("load")),
          download_report_button_ui(ns("download")),
          reset_report_button_ui(ns("reset"))
        )
      ),
      shiny::tags$div(
        class = "col-md-9",
        reporter_previewer_only_ui(ns("previewer"))
      )
    )
  )
}

#' @rdname reporter_previewer
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = getOption("teal.reporter.rmd_output"),
                                   rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
                                   previewer_buttons = c("download", "load", "reset")) {
  # lifecycle::deprecate_soft() # todo:
  checkmate::assert_subset(previewer_buttons, c("download", "load", "reset"), empty.ok = FALSE)
  checkmate::assert_true("download" %in% previewer_buttons)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c("html_document", "pdf_document", "powerpoint_presentation", "word_document"),
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
    # todo: utilize previewer_buttons to show/hide buttons
    report_load_srv("load", reporter = reporter)
    download_report_button_srv(
      "download",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    )
    reset_report_button_srv("reset", reporter = reporter)
    reporter_previewer_only_srv("previewer", reporter = reporter)
  })
}

reporter_previewer_only_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "pcards"))
}

reporter_previewer_only_srv <- function(id, reporter) {
  moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("card_remove_id", "card_down_id", "card_up_id", "remove_card_ok"))
    # todo: buttons looks bad and don't work
    output$pcards <- shiny::renderUI({
      reporter$get_reactive_add_card()
      cards <- reporter$get_cards()

      if (length(cards)) {
        shiny::tags$div(
          class = "panel-group accordion",
          id = "reporter_previewer_panel",
          lapply(seq_along(cards), function(ic) {
            previewer_collapse_item(ic, cards[[ic]]$get_name(), cards[[ic]]$get_content())
          })
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
  })
}

#' @noRd
#' @keywords internal
block_to_html <- function(b) {
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
