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

testthat::test_that("download_report_button_srv - download a document", {
  shiny::testServer(
    download_report_button_srv,
    args = list(
      reporter = reporter,
      global_knitr = getOption("teal.reporter.global_knitr"),
      rmd_output = getOption("teal.reporter.rmd_output"),
      rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args")
    ),
    expr = {
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

testthat::test_that("download_report_button_srv - with external show_rcode parameter", {
  # Create a reactive value to simulate external show_rcode state
  show_rcode_reactive <- shiny::reactiveVal(FALSE)
  
  shiny::testServer(
    download_report_button_srv,
    args = list(
      reporter = reporter,
      global_knitr = getOption("teal.reporter.global_knitr"),
      rmd_output = getOption("teal.reporter.rmd_output"),
      rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
      show_rcode = show_rcode_reactive
    ),
    expr = {
      # The server should initialize without errors when show_rcode is provided
      testthat::expect_true(TRUE)  # Basic test that server starts
    }
  )
})
