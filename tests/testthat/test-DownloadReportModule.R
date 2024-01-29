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
    args = list(reporter = reporter),
    expr = {
      session$setInputs(`download_button` = 0)
      session$setInputs(`output` = "html_document")
      session$setInputs(`title` = "TITLE")
      session$setInputs(`author` = "AUTHOR")
      session$setInputs(`toc` = TRUE)
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
      unlink(output_dir, recursive = TRUE)
    }
  )
})

testthat::test_that("download_report_button_srv - subset of rmd_yaml_args", {
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
        download_report_button_srv,
        args = list(reporter = reporter, rmd_yaml_args = rmd_yaml_args_correct[[iset]]),
        expr = {
        }
      )
    )
  }

  for (iset in seq_along(rmd_yaml_args_wrong)) {
    testthat::expect_error(
      shiny::testServer(
        download_report_button_srv,
        args = list(reporter = reporter, rmd_yaml_args = rmd_yaml_args_wrong[[iset]]),
        expr = {
        }
      ),
      "Assertion"
    )
  }
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
knitr_args <- list()
temp_dir <- tempdir()

testthat::test_that("report_render_and_compress - valid arguments", {
  testthat::expect_no_error(report_render_and_compress(reporter, input, knitr_args, temp_dir))
})

testthat::test_that("report_render_and_compress - invalid arguments", {
  testthat::expect_error(report_render_and_compress(reporter, list(), list(), temp_zip))
  testthat::expect_error(report_render_and_compress(reporter, input, list(), 2))
  testthat::expect_error(report_render_and_compress(reporter, list, list(), ""))
})

testthat::test_that("report_render_and_compress - render an html document", {
  input <- list(author = "NEST", title = "Report", output = "html_document", toc = FALSE)
  temp_dir <- tempdir()
  knitr_args <- list()
  res_path <- report_render_and_compress(reporter, input, knitr_args, temp_dir)
  testthat::expect_identical(res_path, temp_dir)
  files <- list.files(temp_dir, recursive = TRUE)
  testthat::expect_true(any(grepl("[.]Rmd", files)))
  testthat::expect_true(any(grepl("[.]html", files)))
})

testthat::test_that("any_rcode_block", {
  testthat::expect_false(any_rcode_block(reporter))
  card_t <- ReportCard$new()
  card_t$append_text("Header 2 text", "header2")
  card_t$append_rcode("2+2")
  reporter$append_cards(list(card_t))
  testthat::expect_true(any_rcode_block(reporter))
})
