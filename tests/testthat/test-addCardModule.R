testthat::test_that("add_card_button_srv - add a Card to the Reporter", {
  card_fun <- function(card = ReportCard$new(),
                       comment = NULL) {
    card$append_text("Header 2 text", "header2")
    card$append_text("A paragraph of default text", "header2")
    card
  }
  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(comment = "Comment Body")
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )
})

testthat::test_that("add_card_button_ui - returns a tagList", {
  testthat::expect_true(
    inherits(add_card_button_ui("sth"), c("shiny.tag.list", "list"))
  )
})

testthat::test_that("add_card_button_srv supports custom ReportCard classes", {
  custom_card <- R6::R6Class(
    classname = "CustomCard",
    inherit = ReportCard
  )
  card_fun <- function(card = custom_card$new()) {
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )
})

testthat::test_that("add_card_button_srv supports passing no default object to the card", {
  card_fun <- function(card) {
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun(ReportCard$new())$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )
})

testthat::test_that("add_card_button_srv try the card_fun", {
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

testthat::test_that("add_card_button_srv supports passing card_fun with any of the 2 available arguments", {
  card_fun <- function() {
    card <- ReportCard$new()
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )

  card_fun <- function(card) {
    card <- ReportCard$new()
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun(ReportCard$new())$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )

  card_fun <- function(comment) {
    card <- ReportCard$new()
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun("")$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )

  card_fun <- function(comment, card) {
    card <- ReportCard$new()
    card$append_text("Test")
    card
  }

  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = Reporter$new(), card_fun = card_fun),
    expr = {
      card_len <- length(card_fun("", ReportCard$new())$get_content())
      session$setInputs(`add_report_card_button` = 0)
      session$setInputs(`add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )
})
