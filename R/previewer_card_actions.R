ui_previewer_card_actions <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("toggle_code_ui")),
    shiny::actionLink(
      inputId = ns("edit_action"),
      class = "btn btn-primary btn-sm float-end p-3",
      label = NULL,
      title = "Edit card",
      icon = shiny::icon("edit")
    ),
    shiny::actionLink(
      inputId = ns("remove_action"),
      class = "btn btn-danger btn-sm float-end p-3",
      label = NULL,
      icon = shiny::icon("trash-alt"),
    )
  )
}

srv_previewer_card_actions <- function(id, card_r, card_id, reporter) {
  shiny::moduleServer(id, function(input, output, session) {
    new_card_rv <- shiny::reactiveVal()

    # Conditionally render toggle button based on include_rcode setting
    output$toggle_code_ui <- shiny::renderUI({
      card <- shiny::req(card_r())
      include_rcode <- metadata(card, "include_rcode")
      if (is.null(include_rcode)) {
        include_rcode <- TRUE
      }

      if (include_rcode) {
        shiny::actionLink(
          inputId = session$ns("toggle_code_action"),
          class = "btn btn-outline-secondary btn-sm float-end p-3 card-code-toggle",
          label = NULL,
          title = "Toggle R Code chunks",
          icon = shiny::icon("code")
        )
      }
    })

    shiny::observeEvent(
      ignoreInit = TRUE,
      input$edit_action,
      {
        template_card <- card_r()
        new_card_rv(template_card)
        title <- metadata(template_card, "title")

        if (is.null(title) || isFALSE(nzchar(title))) {
          title <- shiny::tags$span(class = "text-muted", "(Empty title)")
        }

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$span(
              class = "edit_title_container",
              "Editing Card:",
              shiny::tags$span(id = session$ns("static_title"), title),
              shiny::actionButton(
                session$ns("edit_title"),
                label = shiny::tags$span(shiny::icon("pen-to-square"), "edit title"),
                class = "fs-6",
                title = "Edit title"
              ),
              shinyjs::hidden(
                shiny::textInput(
                  session$ns("new_title"),
                  label = NULL, value = metadata(template_card, "title")
                )
              )
            ),
            size = "l",
            easyClose = TRUE,
            shiny::tagList(
              ui_card_editor(session$ns("editor"), value = template_card, reporter$get_cached_html(card_id)),
              shiny::uiOutput(session$ns("add_text_element_button_ui"))
            ),
            footer = shiny::tagList(
              shiny::actionButton(session$ns("edit_save"), label = "Save"),
              shiny::modalButton("Close")
            )
          )
        )
      }
    )

    block_input_names_rvs <- srv_card_editor("editor", new_card_rv)

    shiny::observeEvent(input$edit_title, {
      shinyjs::hide("edit_title")
      shinyjs::hide("static_title")
      shinyjs::show("new_title")
      shinyjs::js$jumpToFocus(session$ns("new_title"))
    })

    # Handle
    shiny::observeEvent(input$edit_save, {
      new_card <- shiny::req(new_card_rv())
      input_r <- Filter(Negate(is.null), shiny::reactiveValuesToList(block_input_names_rvs))
      for (name in names(input_r)) {
        new_card[[name]] <- shiny::isolate(input_r[[name]]())
      }
      if (isFALSE(is.null(input$new_title))) {
        metadata(new_card, "title") <- input$new_title
      }
      if (isFALSE(identical(new_card, card_r()))) {
        tryCatch(
          {
            reporter$replace_card(card = new_card, card_id = card_id)
            new_card_rv(NULL)
            reporter$open_previewer(Sys.time())
            shiny::showNotification("Card was successfully updated.", type = "message")
          },
          error = function(err) {
            shiny::showNotification(
              sprintf(
                "A card with the name '%s' already exists. Please use a different name.",
                metadata(new_card, "title")
              ),
              type = "error",
              duration = 5
            )
            shinyjs::enable("edit_save")
          }
        )
      } else {
        new_card_rv(NULL)
        reporter$open_previewer(Sys.time())
      }
    })

    # Handle toggle code button
    shiny::observeEvent(input$toggle_code_action, {
      shinyjs::runjs(sprintf("
        (function() {
          const cardId = '%s';
          const cardElement = document.querySelector('[data-rank-id=\"' + cardId + '\"]');
          if (!cardElement) {
            console.log('Card element not found for cardId:', cardId);
            return;
          }

          // Find only R Code chunks within this card
          // Look for elements that contain 'R Code' text and have a code icon
          const rCodeChunks = [];
          const allCards = cardElement.querySelectorAll('.card');

          allCards.forEach(card => {
            const header = card.querySelector('.card-header');
            if (header) {
              const toggleButton = header.querySelector('[data-bs-toggle=\"collapse\"]');
              const titleText = header.textContent || '';
              const hasCodeIcon = header.querySelector('.fa-code, .fas.fa-code, [class*=\"fa-code\"]');

              // Check if this is an R Code chunk by looking for 'R Code' text and code icon
              if (toggleButton && (titleText.includes('R Code') || hasCodeIcon)) {
                const targetId = toggleButton.getAttribute('href') || toggleButton.getAttribute('data-bs-target');
                if (targetId) {
                  const target = document.querySelector(targetId);
                  if (target) {
                    rCodeChunks.push({
                      button: toggleButton,
                      target: target
                    });
                  }
                }
              }
            }
          });

          if (rCodeChunks.length === 0) {
            console.log('No R Code chunks found in card');
            return;
          }

          // Check if all R Code chunks are collapsed
          let allCollapsed = true;
          rCodeChunks.forEach(chunk => {
            if (chunk.target.classList.contains('show')) {
              allCollapsed = false;
            }
          });

          console.log('Toggling', rCodeChunks.length, 'R Code chunks, allCollapsed:', allCollapsed);

          // Toggle all R Code chunks based on current state
          rCodeChunks.forEach(chunk => {
            const isCurrentlyCollapsed = !chunk.target.classList.contains('show');

            // If all collapsed, expand all; if any expanded, collapse all
            if (allCollapsed && isCurrentlyCollapsed) {
              // Need to expand this R Code chunk
              chunk.button.click();
            } else if (!allCollapsed && !isCurrentlyCollapsed) {
              // Need to collapse this R Code chunk
              chunk.button.click();
            }
          });
        })();
      ", card_id))
    })

    # Handle remove button
    shiny::observeEvent(input$remove_action, reporter$remove_cards(ids = card_id))
  })
}
