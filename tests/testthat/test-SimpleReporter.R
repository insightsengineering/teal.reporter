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
reporter$append_cards(list(card_fun()))

testthat::test_that("simple_reporter_srv - render and downlaod  a document", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter, notification = FALSE, card_fun = card_fun),
    expr = {
      session$setInputs(`download_button_simple` = 0)
      session$setInputs(`download_button_simple-output` = "html_document")
      session$setInputs(`download_button_simple-title` = "TITLE")
      session$setInputs(`download_button_simple-author` = "AUTHOR")
      session$setInputs(`download_button_simple-download_data` = 0)


      f <- output$`download_button_simple-download_data`
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

card_fun <- function(card = ReportCard$new(), comment = NULL) {
  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
      ggplot2::geom_histogram()
  )
  card
}

card1 <- card_fun()

reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("simple_reporter_srv - reset a reporter", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter, notification = FALSE, card_fun = card_fun),
    expr = {
      testthat::expect_identical(reporter$get_cards(), list(card1))
      session$setInputs(`download_button_simple-reset_reporter` = 0)
      session$setInputs(`download_button_simple-reset_reporter_ok` = 0)
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
    args = list(reporter = reporter, notification = FALSE, card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`add_report_card_simple-add_report_card_button` = 0)
      session$setInputs(`add_report_card_simple-comment` = "Comment Body")
      session$setInputs(`add_report_card_simple-add_card_ok` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len + 2L
      )
    }
  )
})

testthat::test_that("simple_reporter_ui - returns a tagList", {
  testthat::expect_true(
    inherits(simple_reporter_ui("sth"), c("shiny.tag.list", "list"))
  )
})
