report_edit_srv <- function(id, input_list_reactive, trigger_reactive, save_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_list <- reactiveVal(NULL)
    
    observeEvent(trigger_reactive(), {
      req(trigger_reactive())
      initial_list <- input_list_reactive()
      current_list(initial_list)

      showModal(modalDialog(
        uiOutput(ns("dynamic_list_ui")),
        footer = tagList(
          actionButton(ns("add_text"), "Add Text Element", icon = icon("plus")),
          modalButton("Cancel"),
          actionButton(ns("save"), "Save Changes", class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    })

    output$dynamic_list_ui <- renderUI({
      req(current_list())
      the_list <- current_list()

      if (length(the_list) == 0) {
        return(tags$p("The list is currently empty. Add text elements or cancel."))
      }

      list_items_ui <- lapply(seq_along(the_list), function(i) {
        el <- the_list[[i]]
        el_id_base <- paste0("item_content_", i)

        item_content <- if (is.character(el)) {
          textAreaInput(ns(paste0("text_", el_id_base)), 
                        label = NULL,
                        value = paste(el, collapse = "\n"),
                        rows = 2,
                        width = "100%")
        } else {
          tags$div(
            id = ns(el_id_base), 
            style = "background-color: #eee; padding: 5px; border-radius: 3px; font-family: monospace;",
            paste("[Non-editable object:", class(el)[1], "]")
          )
        }

        tags$div(
          class = "list-edit-item",
          style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; background-color: #f9f9f9; display: flex; align-items: center;",
          tags$div(style = "flex-grow: 1; margin-right: 10px;", item_content),
          actionButton(ns(paste0("delete_item_", i)), "", icon = icon("trash"), class = "btn-danger btn-sm")
        )
      })

      names(list_items_ui) <- paste0("item_index_", seq_along(list_items_ui))

      rank_list(
        labels = list_items_ui,
        input_id = ns("rank_list_order")
      )
    })

    observe({
      req(current_list())
      list_data <- current_list()
      indices <- seq_along(list_data)
      lapply(indices, function(i) {
         delete_input_id <- paste0("delete_item_", i)
         observeEvent(input[[delete_input_id]], {
          
             current_l <- current_list()
             if (i >= 1 && i <= length(current_l)) {
              cat('current_l length:', length(current_l), '\n')
                 new_list <- current_l[-i]
              cat('new_list length:', length(new_list), '\n')
                 current_list(new_list) # it keeps coming here untill all i:n elements are deleted which is wrong
             } else {
                 warning(paste("Attempted to delete invalid index:", i))
             }
         }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })

      lapply(indices, function(i) {
          text_input_id <- paste0("text_item_content_", i)
          element_type_is_text <- FALSE
          isolate({
              temp_list <- current_list()
              if (i <= length(temp_list)) {
                  element_type_is_text <- is.character(temp_list[[i]])
              }
          })

          if (element_type_is_text) {
               observeEvent(input[[text_input_id]], {
                   current_l <- current_list()
                   if (i <= length(current_l) && is.character(current_l[[i]])) {
                       if (!identical(current_l[[i]], input[[text_input_id]])) {
                            current_l[[i]] <- input[[text_input_id]]
                            current_list(current_l)
                       }
                   }
               }, ignoreNULL = TRUE, ignoreInit = TRUE)
          }
      })

    })

    observeEvent(input$rank_list_order, {
        req(current_list(), input$rank_list_order)
        original_list <- current_list()
        ordered_names <- input$rank_list_order
        original_indices <- suppressWarnings(as.integer(sub("item_index_", "", ordered_names)))

        if (!anyNA(original_indices) &&
            length(original_indices) == length(original_list) &&
            all(original_indices %in% seq_along(original_list))
           ){
            reordered_list <- original_list[original_indices]
            # Update only if order actually changed
            # Check indices directly rather than names vs sequence
            current_indices_order <- match(paste0("item_index_", seq_along(original_list)), ordered_names)
            if (!identical(current_indices_order, seq_along(original_list))) {
                 current_list(reordered_list)
            }
        } else {
             warning(
              paste(
                "Rank_list order mapping failed. Input names:", 
                paste(ordered_names, collapse=", "), 
                "Parsed indices:", 
                paste(original_indices, collapse=", ")
              )
            )
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$add_text, {
        current_l <- current_list()
        new_list <- c(current_l, list("New Text"))
        current_list(new_list)
    })

    observeEvent(input$save, {
        final_list <- isolate(current_list())
        indices <- seq_along(final_list)
        for (i in indices) {
            el <- final_list[[i]]
            input_id <- paste0("text_item_content_", i)
            if (is.character(el) && !is.null(input[[input_id]])) {
                 final_list[[i]] <- isolate(input[[input_id]])
            }
        }
        save_trigger(final_list)
        removeModal()
    })

  })
}
