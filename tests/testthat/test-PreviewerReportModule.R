testthat::skip_if_not_installed("ggplot2")
card1 <- ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("reporter_previewer_srv - render and downlaod a document", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      session$setInputs(`output` = "html_document")
      session$setInputs(`title` = "TITLE")
      session$setInputs(`author` = "AUTHOR")
      session$setInputs(`toc` = FALSE)
      session$setInputs(`download_data_prev` = 0)

      f <- output$download_data_prev
      testthat::expect_true(file.exists(f))
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("report_test_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      zip::unzip(f, exdir = output_dir)
      files <- list.files(output_dir, recursive = TRUE)
      testthat::expect_true(any(grepl("[.]Rmd", files)))
      testthat::expect_true(any(grepl("[.]html", files)))
      unlink(output_dir, recursive = TRUE)
    }
  )
})


testthat::test_that("reporter_previewer_srv - subset of rmd_yaml_args", {
  rmd_yaml_args_correct <- list(
    correct1 = list(
      author = "NEST", title = "Report",
      date = as.character(Sys.Date()), output = "html_document",
      toc = FALSE
    ),
    correct2 = list(
      author = "NEST", title = "Report",
      date = as.character(Sys.Date()), output = "html_document"
    ),
    correct3 = list(output = "html_document")
  )

  rmd_yaml_args_wrong <- list(
    wrong1 = list(author = "NEST", title = "Report"),
    wrong2 = list(output = "WRONG_document"),
    wrong3 = list()
  )

  for (iset in seq_along(rmd_yaml_args_correct)) {
    testthat::expect_silent(
      shiny::testServer(
        reporter_previewer_srv,
        args = list(reporter = reporter, rmd_yaml_args = rmd_yaml_args_correct[[iset]]),
        expr = {
        }
      )
    )
  }

  for (iset in seq_along(rmd_yaml_args_wrong)) {
    testthat::expect_error(
      shiny::testServer(
        reporter_previewer_srv,
        args = list(reporter = reporter, rmd_yaml_args = rmd_yaml_args_wrong[[iset]]),
        expr = {
        }
      ),
      "Assertion"
    )
  }
})

reporter <- Reporter$new()
reporter$append_cards(list(card1))
testthat::test_that("reporter_previewer_srv - remove a card", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      len_prior <- length(reporter$get_cards())
      session$setInputs(`card_remove_id` = 1L)
      session$setInputs(`remove_card_ok` = TRUE)
      len_post <- length(reporter$get_cards())

      testthat::expect_identical(len_prior, len_post + 1L)
    }
  )
})

card2 <- ReportCard$new()
card2$append_text("Header 2 text 2", "header2")
card2$append_text("A paragraph of default text 2", "header2")
card2$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Width)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))

testthat::test_that("reporter_previewer_srv - up with first card and down with last card does not induce change", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      cards_pre <- reporter$get_cards()
      session$setInputs(`card_up_id` = 1L)
      cards_post <- reporter$get_cards()
      testthat::expect_identical(cards_pre, cards_post)

      cards_pre <- reporter$get_cards()
      session$setInputs(`card_down_id` = 2L)
      cards_post <- reporter$get_cards()
      testthat::expect_identical(cards_pre, cards_post)
    }
  )
})

testthat::test_that("reporter_previewer_srv - card up and down compensate", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      cards_pre <- reporter$get_cards()
      session$setInputs(`card_up_id` = 2L)
      session$setInputs(`card_down_id` = 1L)
      cards_post <- reporter$get_cards()
      testthat::expect_equal(cards_pre, cards_post)
    }
  )
})

testthat::test_that("reporter_previewer_srv - card down", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      cards_pre <- reporter$get_cards()
      session$setInputs(`card_down_id` = 1L)
      cards_post <- reporter$get_cards()
      testthat::expect_equivalent(cards_pre, cards_post[2:1])
    }
  )
})

testthat::test_that("reporter_previewer_srv - card up", {
  shiny::testServer(
    reporter_previewer_srv,
    args = list(reporter = reporter),
    expr = {
      cards_pre <- reporter$get_cards()
      session$setInputs(`card_up_id` = 2L)
      cards_post <- reporter$get_cards()
      testthat::expect_equivalent(cards_pre, cards_post[2:1])
    }
  )
})

testthat::test_that("reporter_previewer_ui - returns a tagList", {
  testthat::expect_true(
    inherits(reporter_previewer_ui("sth"), c("shiny.tag"))
  )
})
