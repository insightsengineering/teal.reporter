testthat::describe("add_card_button_srv", {
  it("add a Card to the Reporter", {
    card_fun <- function(card = teal_card(),
                         comment = NULL) {
      card <- c(card, "## Header 2 text", "A paragraph of default text")
      if (!is.null(comment)) {
        card <- c(card, "### Comment", comment)
      }
      card
    }
    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        # Test the card_fun directly to get expected length
        test_card <- card_fun(comment = "Comment Body")
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(comment = "Comment Body")
        session$setInputs(`add_card_ok` = 0)

        # The reporter adds a title, so we expect +1 for the title
        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )
  })

  it("supports custom teal_card classes", {
    # Create a custom teal_card with additional functionality
    custom_card <- function(...) {
      card <- teal_card(...)
      class(card) <- c("custom_teal_card", class(card))
      card
    }

    card_fun <- function(card = custom_card()) {
      card <- c(card, "Test")
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        # Test the card_fun directly to get expected length
        test_card <- card_fun()
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        # The reporter adds a title, so we expect +1 for the title
        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )
  })

  it("supports passing no default object to the card", {
    card_fun <- function() {
      card <- teal_card()
      card <- c(card, "Test")
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        # Test the card_fun directly to get expected length
        test_card <- card_fun()
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        # The reporter adds a title, so we expect +1 for the title
        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )
  })

  it("try the card_fun", {
    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = function(card) stop("ARTIFICIAL ERROR")),
      expr = {
        session$setInputs(`add_report_card_button` = 0)
        testthat::expect_warning(session$setInputs(`add_card_ok` = 0))
      }
    )

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = function(card, comment) stop("ARTIFICIAL ERROR")),
      expr = {
        session$setInputs(`add_report_card_button` = 0)
        testthat::expect_warning(session$setInputs(`add_card_ok` = 0))
      }
    )

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = function(card) stop("ARTIFICIAL ERROR")),
      expr = {
        session$setInputs(`add_report_card_button` = 0)
        testthat::expect_warning(session$setInputs(`add_card_ok` = 0))
      }
    )
  })

  it("uses card_title as a default value for card title input", {
    card_fun <- function(card = teal_card()) {
      c(card, "## Header 2 text", "A paragraph of default text")
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun, card_title = "My Module"),
      expr = {
        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)
        testthat::expect_equal(metadata(reporter$get_cards()[[1]], "title"), card_title)
      }
    )
  })

  it("change in input$label updates title of the card", {
    card_fun <- function(card = teal_card()) {
      c(card, "## Header 2 text", "A paragraph of default text")
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun, card_title = "My Module"),
      expr = {
        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(label = "Test")
        session$setInputs(`add_card_ok` = 0)
        testthat::expect_equal(metadata(reporter$get_cards()[[1]], "title"), "Test")
      }
    )
  })

  it("supports passing card_fun with any of the 2 available arguments", {
    card_fun <- function() {
      card <- teal_card()
      card <- c(card, "Test")
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        test_card <- card_fun()
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )

    card_fun <- function(card = teal_card()) {
      card <- c(card, "Test")
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        test_card <- card_fun()
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )

    card_fun <- function(comment) {
      card <- teal_card()
      card <- c(card, "Test")
      if (!is.null(comment) && comment != "") {
        card <- c(card, "### Comment", comment)
      }
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        test_card <- card_fun("")
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )

    card_fun <- function(comment, card = teal_card()) {
      card <- c(card, "Test")
      if (!is.null(comment) && comment != "") {
        card <- c(card, "### Comment", comment)
      }
      card
    }

    shiny::testServer(
      add_card_button_srv,
      args = list(reporter = Reporter$new(), card_fun = card_fun),
      expr = {
        test_card <- card_fun("")
        expected_len <- length(test_card)

        session$setInputs(`add_report_card_button` = 0)
        session$setInputs(`add_card_ok` = 0)

        testthat::expect_length(reporter$get_blocks(), expected_len + 1)
      }
    )
  })
})
