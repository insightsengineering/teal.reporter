card1 <- ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("simple_reporter_srv - render and downlaod  a document", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter, card_fun = NULL),
    expr = {
      session$setInputs(`download_button` = 0)
      session$setInputs(`downloadButton-docType` = "html_document")
      session$setInputs(`downloadButton-docTitle` = "TITLE")
      session$setInputs(`downloadButton-docAuthor` = "AUTHOR")
      session$setInputs(`downloadButton-download_data` = 0)

      f <- output$`downloadButton-download_data`
      testthat::expect_true(file.exists(f))
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("report_test_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      zip::unzip(f, exdir = output_dir)
      files <- list.files(output_dir, recursive = TRUE)
      testthat::expect_true(any(grepl("[.]Rmd", files)))
      testthat::expect_true(any(grepl("[.]html", files)))
    }
  )
})

card1 <- ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("simple_reporter_srv - reset a reporter", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter),
    expr = {
      testthat::expect_identical(reporter$get_cards(), list(card1))
      session$setInputs(`downloadButton-reset_reporter` = 0)
      session$setInputs(`downloadButton-reset_reporter_ok` = 0)
      testthat::expect_identical(reporter$get_blocks(), list())
    }
  )
})


card_fun <- function(card = ReportCard$new()) {
  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
      ggplot2::geom_histogram()
  )
  card
}

reporter <- Reporter$new()

testthat::test_that("simple_reporter_srv - add a Card to Reporter", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter, card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`addReportCard-addReportCardButton` = 0)
      session$setInputs(`addReportCard-comment` = "Comment Body")
      session$setInputs(`addReportCard-addCardOk` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len + 2L
      )

      testthat::expect_identical(
        tail(reporter$get_blocks(), 1)[[1]]$get_content(),
        "Comment Body"
      )

      testthat::expect_identical(
        tail(reporter$get_blocks(), 2)[[1]]$get_content(),
        "Comment"
      )
    }
  )
})

testthat::test_that("simple_reporter_ui - returns a tagList", {
  testthat::expect_true(
    inherits(simple_reporter_ui("sth"), c("shiny.tag.list", "list"))
  )
})
