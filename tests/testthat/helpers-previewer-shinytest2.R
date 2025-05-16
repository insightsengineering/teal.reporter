create_test_reporter <- function(n_cards = 2) {
  cards <- lapply(seq_len(n_cards), function(i) {
      teal.reporter::report_document(
        sprintf("Card %d", i)
      )
  })
  names(cards) <- seq_along(1:n_cards)

  reporter <- Reporter$new()
  reporter$append_cards(cards)
  reporter
}

get_card_order <- function(app) {
  tryCatch(
    {
      app$get_js("
      Array.from(document.querySelectorAll('.accordion-header'))
        .map(el => el.getAttribute('data-value') || el.textContent.trim())
    ")
    },
    error = function(e) {
      warning("Failed to get card order: ", e$message)
      NULL
    }
  )
}

simulate_drag_and_drop <- function(app, from_idx, to_idx) {
  tryCatch(
    {
      app$run_js(sprintf("
      (function() {
        const cards = document.querySelectorAll('.accordion-header');
        if (!cards || cards.length < 2) {
          throw new Error('Not enough cards found: ' + cards.length);
        }

        const fromCard = cards[%d];
        const toCard = cards[%d];
        if (!fromCard || !toCard) {
          throw new Error('Could not find source or target card');
        }

        // Create a dummy dataTransfer object
        const dataTransfer = new DataTransfer();

        // Create events with coordinates
        const rect = fromCard.getBoundingClientRect();
        const toRect = toCard.getBoundingClientRect();

        const startEvent = new DragEvent('dragstart', {
          bubbles: true,
          cancelable: true,
          dataTransfer: dataTransfer,
          clientX: rect.left,
          clientY: rect.top
        });

        const overEvent = new DragEvent('dragover', {
          bubbles: true,
          cancelable: true,
          dataTransfer: dataTransfer,
          clientX: toRect.left,
          clientY: toRect.top
        });

        const dropEvent = new DragEvent('drop', {
          bubbles: true,
          cancelable: true,
          dataTransfer: dataTransfer,
          clientX: toRect.left,
          clientY: toRect.top
        });

        const endEvent = new DragEvent('dragend', {
          bubbles: true,
          cancelable: true,
          dataTransfer: dataTransfer,
          clientX: toRect.left,
          clientY: toRect.top
        });

        // Dispatch events
        fromCard.dispatchEvent(startEvent);
        toCard.dispatchEvent(overEvent);
        toCard.dispatchEvent(dropEvent);
        fromCard.dispatchEvent(endEvent);

        return true;
      })();
    ", from_idx - 1, to_idx - 1))

      app$wait_for_idle()
      Sys.sleep(0.5) # Give a bit more time for animations
    },
    error = function(e) {
      warning("Failed to simulate drag and drop: ", e$message)
      FALSE
    }
  )
}

start_reporter_preview_app <- function(name) {
  skip_if_too_deep(5)
  skip_if_not(requireNamespace("chromote", quietly = TRUE), "chromote is not available")

  testapp <- shiny::shinyApp(
    ui = shiny::fluidPage(
      reporter_previewer_ui("preview")
    ),
    server = function(input, output, session) {
      reporter <- create_test_reporter(2)
      reporter_previewer_srv(
        "preview",
        reporter = reporter,
        rmd_output = c("html" = "html_document"),
        rmd_yaml_args = list(
          author = "TEST",
          title = "Test Report",
          date = as.character(Sys.Date()),
          output = "html_document",
          toc = FALSE
        )
      )
    }
  )

  app <- NULL
  tryCatch(
    {
      app <- shinytest2::AppDriver$new(
        testapp,
        name = name,
        options = list(
          chromePath = NULL,
          windowSize = c(1000, 800),
          browserOptions = list(
            position = NULL,
            debug = FALSE
          )
        ),
        seed = 123,
        timeout = default_idle_timeout
      )
    },
    error = function(e) {
      skip(paste("Could not initialize AppDriver:", e$message))
    }
  )

  app$wait_for_idle()
  app
}
