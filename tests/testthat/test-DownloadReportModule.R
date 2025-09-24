testthat::describe("DownloadReportModule", {
  it("download_report_button_srv - render and downlaod a document", {
    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))

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
        output_dir <-
          file.path(tmp_dir, sprintf("report_test_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
        dir.create(path = output_dir)
        zip::unzip(f, exdir = output_dir)
        files <- list.files(output_dir, recursive = TRUE)
        testthat::expect_true(any(grepl("[.]Rmd", files)))
        testthat::expect_true(any(grepl("[.]html", files)))
        testthat::expect_true(any(grepl("Report[.]json", files)))
        unlink(output_dir, recursive = TRUE)
      }
    )
  })

  it("download_report_button_srv - subset of rmd_yaml_args", {
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

    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))

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
          expr = {}
        ),
        "Assertion"
      )
    }
  })

  it("download_report_button_ui - returns a tagList", {
    checkmate::expect_multi_class(download_report_button_ui("sth"), c("shiny.tag.list", "shiny.tag"))
  })

  it("report_render_and_compress - valid arguments", {
    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))
    input <- list(author = "NEST", title = "Report", output = "html_document")
    testthat::expect_no_error(report_render_and_compress(reporter, input, list(), withr::local_tempdir()))
  })

  it("report_render_and_compress - invalid arguments", {
    input <- list(author = "NEST", title = "Report", output = "html_document")
    temp_zip_file <- withr::local_tempfile(pattern = "report_", fileext = ".zip")
    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))
    testthat::expect_error(report_render_and_compress(reporter, list(), list(), temp_zip))
    testthat::expect_error(report_render_and_compress(reporter, input, list(), 2))
    testthat::expect_error(report_render_and_compress(reporter, list, list(), ""))
  })

  it("report_render_and_compress - render an html document", {
    input <- list(author = "NEST", title = "Report", output = "html_document", toc = FALSE)
    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))
    temp_dir <- withr::local_tempdir()
    res_path <- report_render_and_compress(reporter, input, list(), temp_dir)
    testthat::expect_identical(res_path, temp_dir)
    withr::with_dir(res_path, zip::unzip(list.files(pattern = "[.]zip$")[[1]])) # Unzip compressed files
    files <- list.files(temp_dir, recursive = TRUE)
    testthat::expect_true(any(grepl("[.]Rmd", files)))
    testthat::expect_true(any(grepl("[.]html", files)))
    testthat::expect_true(any(grepl("Report[.]json", files)))
  })

  it("any_rcode_block", {
    reporter <- Reporter$new()
    reporter$append_cards(list(test_card1.ReportCard()))
    testthat::expect_false(any_rcode_block(reporter))
    card_t <- ReportCard$new()
    card_t$append_text("Header 2 text", "header2")
    card_t$append_rcode("2+2")
    reporter$append_cards(list(card_t))
    testthat::expect_true(any_rcode_block(reporter))
  })
})
