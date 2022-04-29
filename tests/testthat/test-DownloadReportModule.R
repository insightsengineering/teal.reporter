card1 <- ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("download_report_button_srv - render and downlaod a document", {
  shiny::testServer(
    download_report_button_srv,
    args = list(reporter = reporter, notification = FALSE),
    expr = {
      session$setInputs(`download_button` = 0)
      session$setInputs(`output` = "html_document")
      session$setInputs(`title` = "TITLE")
      session$setInputs(`author` = "AUTHOR")
      session$setInputs(`download_data` = 0)

      f <- output$download_data
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

testthat::test_that("download_report_button_srv - reset a report", {
  shiny::testServer(
    download_report_button_srv,
    args = list(reporter = reporter, notification = FALSE),
    expr = {
      testthat::expect_identical(reporter$get_cards(), list(card1))
      session$setInputs(`reset_reporter` = 0)
      session$setInputs(`reset_reporter_ok` = 0)
      testthat::expect_identical(reporter$get_blocks(), list())
    }
  )
})

testthat::test_that("download_report_button_ui - returns a tagList", {
  testthat::expect_true(
    inherits(download_report_button_ui("sth"), c("shiny.tag.list", "list"))
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
input <- list(author = "NEST", title = "Report", output = "html_document")
temp_dir <- tempdir()

testthat::test_that("report_render_and_compress - valid arguments", {
  testthat::expect_error(report_render_and_compress(reporter, input, temp_dir), NA)
})

testthat::test_that("report_render_and_compress - invalid arguments", {
  testthat::expect_error(report_render_and_compress(reporter, list(), temp_zip))
  testthat::expect_error(report_render_and_compress(reporter, input, 2))
  testthat::expect_error(report_render_and_compress(reporter, list, ""))
})

testthat::test_that("report_render_and_compress - render an html document", {
  input <- list(author = "NEST", title = "Report", output = "html_document")
  temp_dir <- tempdir()
  res_path <- report_render_and_compress(reporter, input, temp_dir)
  expect_identical(res_path, temp_dir)
  files <- list.files(temp_dir, recursive = TRUE)
  testthat::expect_true(any(grepl("[.]Rmd", files)))
  testthat::expect_true(any(grepl("[.]html", files)))
})
