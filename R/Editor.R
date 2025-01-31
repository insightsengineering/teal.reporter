#' Report previewer module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Module offers functionalities to visualize, manipulate,
#' and interact with report cards that have been added to a report.
#' It includes a previewer interface to see the cards and options to modify the report before downloading.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name reporter_editor
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname reporter_editor
#' @export
reporter_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    add_editor_js(ns),
    add_previewer_css(),
    shiny::tagList(
      shiny::tags$div(class = "col-md-3", shiny::tags$div(class = "well", shiny::uiOutput(ns("encoding")))),
      shiny::tags$div(class = "col-md-9", shiny::tags$div(id = "reporter_previewer", shiny::uiOutput(ns("pcards"))))
    )
  )
}

#' @rdname reporter_editor
#' @export
reporter_editor_srv <- function(id,
                                reporter,
                                global_knitr = getOption("teal.reporter.global_knitr"),
                                rmd_output = c(
                                  "html" = "html_document",
                                  "pdf" = "pdf_document",
                                  "powerpoint" = "powerpoint_presentation",
                                  "word" = "word_document"
                                ),
                                rmd_yaml_args = list(
                                  author = "NEST",
                                  title = "Report",
                                  date = as.character(Sys.Date()),
                                  output = "html_document",
                                  toc = FALSE
                                )) {
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
    ns <- session$ns

    teal.reporter::reset_report_button_srv("resetButtonPreviewer", reporter)

    output$encoding <- shiny::renderUI({
      reporter$get_reactive_add_card()
      shiny::tagList(
        shiny::tags$h3("Download the Report"),
        shiny::tags$hr(),
        reporter_download_inputs(
          rmd_yaml_args = rmd_yaml_args,
          rmd_output = rmd_output,
          showrcode = any_rcode_block(reporter),
          session = session
        ),
        htmltools::tagAppendAttributes(
          shiny::tags$a(
            id = ns("download_data_prev"),
            class = "btn btn-primary shiny-download-link",
            href = "",
            target = "_blank",
            download = NA,
            shiny::tags$span("Download Report", shiny::icon("download"))
          ),
          class = if (length(reporter$get_cards())) "" else "disabled"
        ),
        teal.reporter::reset_report_button_ui(ns("resetButtonPreviewer"), label = "Reset Report")
      )
    })

    output$pcards <- shiny::renderUI({
      reporter$get_reactive_add_card()
      input$card_remove_id
      input$card_down_id
      input$card_up_id
      input$block_remove_event
      input$block_add_event
      input$block_modify_event

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
          shiny::tags$p(class = "text-danger mt-4", shiny::tags$strong("No Cards added"))
        )
      }
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
    
    shiny::observeEvent(input$block_add_event, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Select Title",
          selectInput(ns("selectTitle"),
            label = "Select Title",
            choices = c("Generate Custom Title & Footer", unique(titles$TABLE.ID)),
            selected = "Generate Custom Title & Footer"),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::actionButton(ns("submit_text"), "Change title", class = "btn-danger")
          )
        )
      )
    })
    
    verboseTitle <- reactive({
      titles %>% 
        dplyr::filter(titles$TABLE.ID == input$selectTitle, titles$IDENTIFIER == "TITLE") %>% 
        dplyr::mutate(TEXT = paste0(TABLE.ID, ": ", TEXT)) %>% 
        dplyr::select(TEXT)
    })
    
    footer <- reactive({
      titles %>% 
        dplyr::filter(titles$TABLE.ID == input$selectTitle, titles$IDENTIFIER != "TITLE") %>% 
        dplyr::select(TEXT) %>%
        dplyr::summarise(TEXT = paste(TEXT, collapse = "\n"))
    })
    
    shiny::observeEvent(input$submit_text, {
      reporter$modify_text(as.integer(input$block_add_event[1]), 1, verboseTitle())
      
      temp <- reporter$get_text(as.integer(input$block_add_event[1]), 3)
      
      reporter$remove_block_from_card(as.integer(input$block_add_event[1]), 3)
      
      reporter$add_text(as.integer(input$block_add_event[1]), footer())
      
      reporter$add_text(as.integer(input$block_add_event[1]), as.character(temp))
      
      reporter$trigger_reactive_add_card() 
      
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$block_modify_event, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Modify Text",
          shiny::textAreaInput(ns("user_input"), 
            label = "Modify Text:", 
            value = reporter$get_text(card_id = as.numeric(input$block_modify_event[1]), block_id = input$block_modify_event[2]),
            width = "100%",
            height = "200px"),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::actionButton(ns("submit_text2"), "Change text", class = "btn-danger")
          )
        )
      )
    })
    
    shiny::observeEvent(input$submit_text2, {
      reporter$modify_text(as.integer(input$block_modify_event[1]), input$block_modify_event[2], input$user_input)

      reporter$trigger_reactive_add_card() 
      
      shiny::removeModal()
    })

    shiny::observeEvent(input$card_up_id, {
      if (input$card_up_id > 1) {
        reporter$swap_cards(as.integer(input$card_up_id), as.integer(input$card_up_id - 1))
      }
    })

    shiny::observeEvent(input$card_down_id, {
      if (input$card_down_id < length(reporter$get_cards())) {
        reporter$swap_cards(as.integer(input$card_down_id), as.integer(input$card_down_id + 1))
      }
    })

    shiny::observeEvent(input$block_remove_event, {
      reporter$remove_block_from_card(as.integer(input$block_remove_event[1]), as.integer(input$block_remove_event[2]))
    })

    output$download_data_prev <- shiny::downloadHandler(
      filename = function() {
        paste("report_", format(Sys.time(), "%y%m%d%H%M%S"), ".zip", sep = "")
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
block_to_editor_html <- function(card_id, block_id, block) {
  b_content <- block$get_content()
  if (inherits(block, "TextBlock")) {
    shiny::tags$span(
      class = paste("row", "align-middle"),
      switch(block$get_style(),
        header1 = shiny::tags$h1(class = "col-md-9", b_content),
        header2 = shiny::tags$h2(class = "col-md-9", b_content),
        header3 = shiny::tags$h3(class = "col-md-9", b_content),
        header4 = shiny::tags$h4(class = "col-md-9", b_content),
        verbatim = shiny::tags$pre(b_content),
        shiny::tags$pre(b_content)
      ),
      shiny::tags$div(class="col-md-2", title = "Delete or modify text",
        block_delete_button("block_delete", "trash", card_id, block_id, style = ""),
        block_modify_button("block_modify", "pencil", card_id, block_id, style = "")
      )
    )
  } else if (inherits(block, "RcodeBlock")) {
    better_panel_item(
      title = "R Code",
      title_buttons = block_delete_button(class_name = "block_delete", "trash", card_id, block_id, style = "width:50px;height:25px;margin-left:20px;padding:2px"),
      shiny::tags$pre(b_content)
    )
  } else if (inherits(block, "PictureBlock")) {
    shiny::tags$img(src = knitr::image_uri(b_content))
  } else if (inherits(block, "TableBlock")) {
    b_table <- readRDS(b_content)
    shiny::tags$pre(flextable::htmltools_value(b_table))
  } else if (inherits(block, "NewpageBlock")) {
    shiny::tags$br()
  } else {
    stop("Unknown block class")
  }
}

#' @noRd
#' @keywords internal
add_previewer_css <- function() {
  shiny::tagList(
    shiny::singleton(shiny::tags$head(shiny::includeCSS(system.file("css/Previewer.css", package = "teal.reporter")))),
    shiny::singleton(shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter"))))
  )
}

#' @noRd
#' @keywords internal
add_editor_js <- function(ns) {
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        shiny::HTML(
          sprintf(
            '
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
          function removeCardBlock(element) {
            let cardId = element.dataset.cardid;
            let blockId = element.dataset.blockid;
            Shiny.setInputValue("%s", [cardId, blockId], {priority: "event"});
          }
          function addText(element) {
            let cardId = element.dataset.cardid;
            let blockId = element.dataset.blockid;
            Shiny.setInputValue("%s", [cardId, blockId], {priority: "event"});
          }
          function modifyTitle(element, event) {
          
            if (event) {
              console.log("Preventing collapse");
              event.preventDefault();
              event.stopPropagation();
            }
          
            let cardId = element.dataset.cardid;
            let blockId = element.dataset.blockid;
            Shiny.setInputValue("%s", [cardId, blockId], {priority: "event"});
          }
         ',
            ns("card_remove_id"),
            ns("card_up_id"),
            ns("card_down_id"),
            ns("block_remove_event"),
            ns("block_add_event"),
            ns("block_modify_event")
          )
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
block_delete_button <- function(class_name, icon_name, card_idx, block_idx, style = "") {
  checkmate::assert_string(class_name)
  checkmate::assert_string(icon_name)
  checkmate::assert_int(card_idx)
  checkmate::assert_int(block_idx)

  shiny::tags$button(
    class = paste(class_name, "btn", "btn-danger", "btn-lg"),
    style = style,
    onclick = "removeCardBlock(this)",
    `data-blockid` = block_idx,
    `data-cardid` = card_idx,
    shiny::icon(icon_name, sprintf("fa-%sx", 1L))
  )
}

#' @noRd
#' @keywords internal
block_add_button <- function(class_name, icon_name, card_idx, block_idx, style = "", addedText) {
  checkmate::assert_string(class_name)
  checkmate::assert_string(icon_name)
  checkmate::assert_string(addedText)
  checkmate::assert_int(card_idx)
  checkmate::assert_int(block_idx)
  
  shiny::tags$button(
    class = paste(class_name, "btn", "btn-success", "btn-lg"),
    style = style,
    onclick = "addText(this)",
    `data-blockid` = block_idx,
    `data-cardid` = card_idx,
    shiny::tagList(
      shiny::icon(icon_name, class = sprintf("fa-%sx", 1L)),
      addedText
    )
  )
}

#' @noRd
#' @keywords internal
block_modify_button <- function(class_name, icon_name, card_idx, block_idx, style = "") {
  checkmate::assert_string(class_name)
  checkmate::assert_string(icon_name)
  checkmate::assert_int(card_idx)
  checkmate::assert_int(block_idx)
  
  shiny::tags$button(
    class = paste(class_name, "btn", "btn-warning", "btn-lg"),
    style = style,
    onclick = "modifyTitle(this, event)",
    `data-blockid` = block_idx,
    `data-cardid` = card_idx,
    shiny::icon(icon_name, sprintf("fa-%sx", 1L))
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
  shiny::tags$div(
    .renderHook = function(x) {
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
            id = paste0("collapse", idx),
            class = "collapse out",
            shiny::tags$div(
              class = "panel-body",
              shiny::tags$div(
                  block_add_button("block_add", "edit", card_id = idx, block_id = 1, style = "margin-left:20px;", "Select Title & Footer")
                  # block_add_button("block_add2", "plus", card_id = idx, block_id = 1, style = "", "Create New Title & Footer")
              ),
              shiny::tags$div(id = paste0("card", idx), lapply(seq_along(card_blocks), function(block_id) {
                  block_to_editor_html(card_id = idx, block_id = block_id, block = card_blocks[[block_id]])
              }))
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
                  shiny::tags$h4(paste0("Card ", idx, ": ", card_name), shiny::icon("caret-down"))
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
              shiny::tags$div(id = paste0("card", idx), lapply(seq_along(card_blocks), function(block_id) {
                block_to_editor_html(card_id = idx, block_id = block_id, block = card_blocks[[block_id]])
              }))
            )
          )
        )
      }
    }
  )
}
