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
          shiny::uiOutput(ns("pcards")),
          shiny::div(
            style = "margin-top: 10px;", 
            shiny::actionButton(ns("add_card_button"), "Add New Card", icon = shiny::icon("plus"), class = "btn-primary")
          )
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
    card_to_edit_rv <- reactiveVal(NULL)
    text_block_to_edit_rv <- reactiveVal(NULL)
    card_to_delete_rv <- reactiveVal(NULL)
    ui_refresh_trigger <- reactiveVal(0)

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
                  content <- cards[[card_name]]$get_content()
                  edit <- FALSE
                } else if (inherits(cards[[card_name]], "ReportDocument")) {
                  edit <- TRUE
                  content <- cards[[card_name]]
                }
                previewer_collapse_item(card_name, content, ns = ns, edit = edit)
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

    # Observer 1: Detect click from JS and set reactiveVal
    shiny::observeEvent(input$edit_card_clicked, {
      card_to_edit_rv(input$edit_card_clicked)
    })

    # Observer 2: Show the first modal when card_to_edit_rv is set
    shiny::observeEvent(card_to_edit_rv(), {
      current_card_name <- card_to_edit_rv()

      # Show the first modal (listing blocks)
      showModal(
        modalDialog(
          title = paste("Editing Card:", current_card_name),
          size = "l", easyClose = TRUE,
          uiOutput(ns(paste0("modal_blocks_ui_", current_card_name))),
          footer = tagList(modalButton("Close"))
        )
      )

      # Render UI for Blocks inside the First Modal
      output[[paste0("modal_blocks_ui_", current_card_name)]] <- renderUI({

        ui_refresh_trigger()

        cards <- reporter$get_cards()
        card <- cards[[current_card_name]]

        # Display Block List View

        tagList(
          div(
            style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #eee;",
            actionButton(
                inputId = ns(paste0("add_text_block_btn_", current_card_name)),
                label = "Add New Text Block",
                icon = icon("plus"),
                class = "btn btn-success btn-sm",
                onclick = sprintf(
                  # Triggers input$add_text_block_clicked
                  "Shiny.setInputValue('%s', { card: '%s' }, {priority: 'event'});",
                  ns("add_text_block_clicked"),
                  current_card_name
                )
            )
          ),
          if (length(card) == 0) {
            tags$p("This card has no blocks.")
          } else {
            lapply(seq_along(card), function(i) {
              block <- card[[i]]
              block_modal_id <- paste0(current_card_name, "_modal_block_", i)
              block_content_html <- block_to_html(block)

              tags$div(
                style = "border: 1px solid #eee; padding: 10px; margin-bottom: 10px; display: flex; align-items: center;",
                tags$div(style = "flex-grow: 1; margin-right: 10px;", block_content_html),
                if (inherits(block, "character")) {
                  actionButton(
                    inputId = ns(paste0("edit_modal_block_", block_modal_id)),
                    label = NULL, icon = icon("pen-to-square"),
                    class = "btn btn-sm btn-outline-primary",
                    # Use onclick to set the second reactiveVal (text_block_to_edit_rv)
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', { card: '%s', index: %d }, {priority: 'event'});",
                      ns("text_block_edit_clicked"), 
                      current_card_name,
                      i
                    )
                  )
                },
                {
                  onclick_js_delete <- sprintf(
                    "Shiny.setInputValue('%s', { card: '%s', index: %d }, {priority: 'event'});",
                    ns("delete_block_clicked"),
                    current_card_name,
                    i
                  )
                  actionButton(
                    inputId = ns(paste0("delete_modal_block_", block_modal_id)),
                    label = NULL, icon = icon("trash-alt"),
                    class = "btn btn-sm btn-outline-danger",
                    onclick = onclick_js_delete
                  )
              }
              )
            })
          }
        )
      })

      # Reset the trigger for the first modal
      card_to_edit_rv(NULL)
    })

    # Observer 3: Detect click for text block edit from JS
    shiny::observeEvent(input$text_block_edit_clicked, {
        edit_info <- input$text_block_edit_clicked
        req(edit_info, edit_info$card, edit_info$index)
        text_block_to_edit_rv(edit_info)
    }, ignoreInit = TRUE)

    # Observer 4: Show the second modal (text editing) when text_block_to_edit_rv is set
    shiny::observeEvent(text_block_to_edit_rv(), {
        edit_info <- text_block_to_edit_rv()
        req(edit_info)

        current_card_name <- edit_info$card
        block_index <- edit_info$index

        cards <- reporter$get_cards()
        card <- cards[[current_card_name]]
 
        block_edit <- card[[block_index]]
        req(inherits(block_edit, "character"))

        # Define IDs for the second modal's inputs
        modal_instance_id <- paste0(current_card_name, "_", block_index)
        text_area_id <- ns(paste0("text_area_edit_", modal_instance_id))
        save_button_id_ui <- ns(paste0("save_text_edit_", modal_instance_id))
        cancel_button_id_ui <- ns(paste0("cancel_text_edit_", modal_instance_id))

        # Show the second modal for text editing
        showModal(
            modalDialog(
                title = paste("Edit Text Block", block_index, "in Card:", current_card_name),
                easyClose = TRUE,
                textAreaInput(
                    inputId = text_area_id,
                    label = "Edit Text Content:",
                    value = block_edit,
                    rows = 15, width = "100%"
                ),
                footer = tagList(
                    # Cancel button - uses onclick to trigger input$cancel_text_edit_clicked
                    actionButton(
                       inputId = cancel_button_id_ui,
                       label = "Cancel",
                       class = "btn-secondary",
                       onclick = sprintf(
                         "Shiny.setInputValue('%s', true, {priority: 'event'});",
                         ns("cancel_text_edit_clicked")
                       )
                    ),
                    # Save button - uses onclick to trigger input$save_text_edit_clicked
                    actionButton(
                       inputId = save_button_id_ui,
                       label = "Save Text",
                       class = "btn-primary",
                       onclick = sprintf(
                         "Shiny.setInputValue('%s', { value: document.getElementById('%s').value }, {priority: 'event'});",
                         ns("save_text_edit_clicked"),
                         text_area_id
                       )
                    )
                )
            )
        )
    }, ignoreInit = TRUE)

    # Observer 5: Handle Cancel Button Click (triggered by JS)
    shiny::observeEvent(input$cancel_text_edit_clicked, {
        req(text_block_to_edit_rv())
        removeModal()
        text_block_to_edit_rv(NULL)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Observer 6: Handle Save Button Click (triggered by JS)
    shiny::observeEvent(input$save_text_edit_clicked, {
        event_data <- input$save_text_edit_clicked
        req(event_data, !is.null(event_data$value))
        new_text <- event_data$value

        edit_info <- text_block_to_edit_rv()

        current_card_name <- edit_info$card
        block_index <- edit_info$index

        cards <- reporter$get_cards()
        card <- cards[[current_card_name]]
        block_original <- card[[block_index]]
        req(inherits(block_original, "character"))

        card[[block_index]] <- new_text

        reporter$set_card_content(current_card_name, card)
        removeModal()
        showNotification("Text block updated successfully!", type = "message")

        text_block_to_edit_rv(NULL)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Observer 7: Detect Delete Button Click (triggered by JS)
    shiny::observeEvent(input$delete_card_clicked, {
        card_name_to_delete <- input$delete_card_clicked
        req(card_name_to_delete)

        card_to_delete_rv(card_name_to_delete)

        showModal(
          modalDialog(
            title = "Confirm Deletion",
            paste("Are you sure you want to delete card:", card_name_to_delete, "?"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("remove_card_ok"), "Delete Card", class = "btn-danger")
            )
          )
        )
    }, ignoreInit = TRUE)

    # Observer 8: Handle Block Delete Button Click (triggered by JS)
    shiny::observeEvent(input$delete_block_clicked, {
      delete_info <- input$delete_block_clicked
      req(delete_info, delete_info$card, delete_info$index)

      current_card_name <- delete_info$card
      block_index <- delete_info$index

      cards <- reporter$get_cards()
      card <- cards[[current_card_name]]
      updated_blocks <- card[-block_index]

      card <- structure(updated_blocks, class = "ReportDocument")

      reporter$set_card_content(current_card_name, card)

      showNotification(paste("Block", block_index, "deleted from card:", current_card_name), type = "message")

      # We stay in the modal, no need to reset text_block_to_edit_rv unless it was set
      ui_refresh_trigger(ui_refresh_trigger() + 1)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Observer 9: Show Add Card Modal
    shiny::observeEvent(input$add_card_button, {
      showModal(
        modalDialog(
          title = "Add New Card",
          textInput(ns("new_card_name"), "Card Name:", placeholder = "Enter a unique card name"),
          textAreaInput(ns("new_card_comment"), "Initial Comment (Optional):", rows = 4),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_add_card"), "Add Card", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    }, ignoreInit = TRUE)


    # Observer 10: Confirm Add Card
    shiny::observeEvent(input$confirm_add_card, {
        card_name <- trimws(input$new_card_name)
        comment_text <- trimws(input$new_card_comment)
        new_card <- teal.reporter::report_document(comment_text)
        reporter$append_cards(setNames(list(new_card), card_name))
        removeModal()
        showNotification(paste("Card '", card_name, "' added successfully."), type = "message")
        ui_refresh_trigger(ui_refresh_trigger() + 1)

    }, ignoreInit = TRUE)


    # Observer 11: Card Removal
    shiny::observeEvent(input$remove_card_ok, {
      card_name <- card_to_delete_rv()
      req(card_name)

      card_names <- names(reporter$get_cards())
      card_index <- match(card_name, card_names)

      reporter$remove_cards(card_index)
      showNotification(paste("Card:", card_name, "removed."), type = "message")

      removeModal()
      card_to_delete_rv(NULL)
    })

    # Observer 12: Show Add Text Block Modal
    shiny::observeEvent(input$add_text_block_clicked, {
        add_info <- input$add_text_block_clicked
        req(add_info, add_info$card)
        target_card_name <- add_info$card

        add_text_area_id <- ns(paste0("add_text_area_", target_card_name))
        add_save_button_id_ui <- ns(paste0("add_save_text_btn_", target_card_name))
        add_cancel_button_id_ui <- ns(paste0("add_cancel_text_btn_", target_card_name))

        showModal(
            modalDialog(
                title = paste("Add New Text Block to Card:", target_card_name),
                textAreaInput(
                    inputId = add_text_area_id,
                    label = "Enter Text Content:",
                    value = "", # Start empty
                    rows = 15, width = "100%"
                ),
                footer = tagList(
                    actionButton(
                        inputId = add_cancel_button_id_ui,
                        label = "Cancel",
                        class = "btn-secondary",
                        onclick = sprintf(
                            # Trigger dedicated cancel input
                            "Shiny.setInputValue('%s', true, {priority: 'event'});",
                            ns("add_text_cancel_clicked")
                        )
                    ),
                    actionButton(
                        inputId = add_save_button_id_ui,
                        label = "Save New Block",
                        class = "btn-primary",
                        onclick = sprintf(
                            # Send text value and target card name to dedicated save input
                            "Shiny.setInputValue('%s', { card: '%s', value: document.getElementById('%s').value }, {priority: 'event'});",
                            ns("add_text_save_clicked"),
                            target_card_name,
                            add_text_area_id
                        )
                    )
                ),
                easyClose = TRUE
            )
        )
    }, ignoreInit = TRUE)


    # Observer 13: Handle Cancel for Add Text Block Modal
    shiny::observeEvent(input$add_text_cancel_clicked, {
        removeModal()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Observer 14: Handle Save for Add Text Block Modal
    shiny::observeEvent(input$add_text_save_clicked, {
      save_info <- input$add_text_save_clicked
      req(save_info, save_info$card, !is.null(save_info$value))

      target_card_name <- save_info$card
      new_text_content <- save_info$value

      cards <- reporter$get_cards()
      card <- cards[[target_card_name]]

      # Create and append the new block
      if (nzchar(trimws(new_text_content))) { # ONLY IF TEXT IS NON EMPTY
        card <- c(card, new_text_content)
        reporter$set_card_content(target_card_name, card)
        removeModal() # Close the "Add Text" modal
        showNotification("New text block added successfully.", type = "message")
        ui_refresh_trigger(ui_refresh_trigger() + 1)
      } else {
        # Text was empty or whitespace only
        removeModal()
        showNotification("No text entered, block not added.", type = "warning")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


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
  switch(b$get_style(),
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
previewer_collapse_item <- function(card_name, card_blocks, ns = NULL, edit = FALSE, open = FALSE) {
  tags$div(
    `data-rank-id` = card_name,
    bslib::accordion(
      open = open,
      # CARDS IN THE ACCORDION PANEL SHOULD BE SORTABLE
      bslib::accordion_panel(
        value = card_name,
        title = tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
          tags$span(card_name),
          if (edit) {
            actionButton(
              inputId = ns(paste0("edit_card_", card_name)),
              label = NULL,
              icon = shiny::icon("edit"),
              class = "btn btn-warning btn-sm",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                ns("edit_card_clicked"),
                card_name
              )
            )
          },
          if (!is.null(ns)) {
            actionButton(
              inputId = ns(paste0("delete_card_", card_name)),
              label = NULL,
              icon = shiny::icon("trash-alt"),
              class = "btn btn-danger btn-sm",
              onclick = sprintf(
                # Trigger a new input when clicked, passing the card name
                "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                ns("delete_card_clicked"),
                card_name
              )
            )
        }
        ),
        tags$div(
          id = ns(paste0("sortable_", card_name)), # THIS MIGHT BE NEEDED FOR SORTING BUT DOESNT WORK YET
          class = "card-blocks-container",
          lapply(seq_along(card_blocks), function(i) {
            block <- card_blocks[[i]]
            block_id <- paste0(card_name, "_block_", i) # THIS MIGHT BE NEEDED FOR SORTING BUT DOESNT WORK YET
            block_to_html(block)
          })
        )
      )
    )
  )
}
