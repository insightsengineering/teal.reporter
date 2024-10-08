testthat::test_that("report_load_srv - loading reporter restores saved content", {
  testthat::skip_if_not_installed("ggplot2")

  reporter <- Reporter$new()
  reporter$set_id("xyz")
  card <- teal.reporter::ReportCard$new()

  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
      ggplot2::geom_histogram()
  )
  reporter$append_cards(list(card))

  temp_dir <- file.path(tempdir(), "tempdir")
  suppressWarnings(dir.create(temp_dir))
  unlink(list.files(temp_dir, recursive = TRUE, full.names = TRUE))

  reporter_path <- reporter$to_jsondir(temp_dir)

  temp_zip_file <- tempfile(pattern = "report_", fileext = ".zip")
  zip::zipr(temp_zip_file, reporter_path)

  shiny::testServer(
    report_load_srv,
    args = list(reporter = reporter),
    expr = {
      reporter$reset()
      session$setInputs(`reporter_load` = 0)
      session$setInputs(
        archiver_zip = list(
          datapath = temp_zip_file,
          name = basename(temp_zip_file)
        )
      )
      session$setInputs(`reporter_load_main` = 0)
      testthat::expect_length(reporter$get_cards(), 1)
      testthat::expect_length(reporter$get_blocks(), 3)
      testthat::expect_s3_class(reporter$get_blocks()[[1]], "TextBlock")
      testthat::expect_identical(reporter$get_blocks()[[1]]$get_content(), "Header 2 text")
      testthat::expect_s3_class(reporter$get_blocks()[[2]], "TextBlock")
      testthat::expect_identical(reporter$get_blocks()[[2]]$get_content(), "A paragraph of default text")
      testthat::expect_s3_class(reporter$get_blocks()[[3]], "PictureBlock")
    }
  )
})

testthat::test_that("report_load_srv - fail to load a reporter because of different id", {
  reporter <- Reporter$new()
  reporter$set_id("xyz")

  temp_dir <- file.path(tempdir(), "tempdir")
  suppressWarnings(dir.create(temp_dir))
  unlink(list.files(temp_dir, recursive = TRUE, full.names = TRUE))

  reporter_path <- reporter$to_jsondir(temp_dir)

  temp_zip_file <- tempfile(pattern = "report_", fileext = ".zip")
  zip::zipr(temp_zip_file, reporter_path)

  reporter <- Reporter$new()$set_id("different")

  oo <- capture_output(shiny::testServer(
    report_load_srv,
    args = list(reporter = reporter),
    expr = {
      reporter$reset()
      session$setInputs(`reporter_load` = 0)
      session$setInputs(
        archiver_zip = list(
          datapath = temp_zip_file,
          name = basename(temp_zip_file)
        )
      )
      session$setInputs(`reporter_load_main` = 0)
    }
  ))
  testthat::expect_true(grepl("Loaded Report id has to match the current instance one", oo))
})


testthat::test_that("report_load_ui - returns a tagList", {
  testthat::expect_s3_class(report_load_ui("sth"), c("shiny.tag.list", "list"))
})
