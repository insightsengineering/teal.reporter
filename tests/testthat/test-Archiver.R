card1 <- ReportCard$new()

card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

card2 <- ReportCard$new()

card2$append_text("Header 2 text", "header2")
card2$append_text("A paragraph of default text", "header2")
lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
table_res2 <- rtables::build_table(lyt, airquality)
card2$append_table(table_res2)
card2$append_table(iris)

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))

testthat::test_that("intialize Archiver", {
  expect_error(Archiver$new(), NA)
})

testthat::test_that("new returns an object of type Archiver", {
  testthat::expect_true(inherits(Archiver$new(), "Archiver"))
})

testthat::test_that("Archiver errors with the abstract methods", {
  archiver <- Archiver$new()
  expect_error(archiver$read(), "Pure virtual method")
  expect_error(archiver$write(), "Pure virtual method")
})

testthat::test_that("intialize FileArchiver", {
  expect_error(FileArchiver$new(), NA)
})

testthat::test_that("FileArchiver creates a temp directory when initialized", {
  archiver <- FileArchiver$new()
  expect_true(dir.exists(archiver$get_output_dir()))
})

testthat::test_that("FileArchiver creates a temp directory when initialized, with a proper name", {
  archiver <- FileArchiver$new()
  expect_true(grepl("archive_[0-9]{18,18}$", archiver$get_output_dir()))
})

testthat::test_that("FileArchiver dectructor removes the temp dir", {
  archiver <- FileArchiver$new()
  temp_dir <- archiver$get_output_dir()
  expect_true(dir.exists(temp_dir))
  rm(archiver)
  # we need a garbage collector
  gc()
  expect_false(dir.exists(temp_dir))
})

testthat::test_that("intialize JSONArchiver", {
  expect_error(JSONArchiver$new(), NA)
})

testthat::test_that("JSONArchiver creates a temp directory when initialized", {
  archiver <- JSONArchiver$new()
  expect_true(dir.exists(archiver$get_output_dir()))
})

testthat::test_that("JSONArchiver dectructor removes the temp dir", {
  archiver <- JSONArchiver$new()
  temp_dir <- archiver$get_output_dir()
  expect_true(dir.exists(temp_dir))
  rm(archiver)
  # we need a garbage collector
  gc()
  expect_false(dir.exists(temp_dir))
})

archiver <- JSONArchiver$new()

testthat::test_that("JSONArchiver write a reporter", {
  expect_error(archiver$write(reporter), NA)
})

testthat::test_that("JSONArchiver write a reporter with a json file and static files", {
  files <- list.files(archiver$get_output_dir())
  expect_true(length(files) == 4)
  expect_true("Report.json" %in% files)
})

testthat::test_that("JSONArchiver read back the Reporter instance", {
  expect_s3_class(archiver$read(), "Reporter")
  expect_length(archiver$read()$get_cards(), 2L)
  expect_length(archiver$read()$get_blocks(), 8L)
})

testthat::test_that("JSONArchiver read back and all table/picture statics exists", {
  file_blocks <- Filter(
    function(x) inherits(x, "PictureBlock") || inherits(x, "TableBlock"),
    archiver$read()$get_blocks()
  )
  expect_true(all(vapply(file_blocks, function(f) file.exists(f$get_content()), logical(1))))
})

testthat::test_that("JSONArchiver with an empty dir", {
  temp_dir <- file.path(tempdir(), "test")
  dir.create(temp_dir)

  expect_warning(archiver$read(temp_dir), "The directory provided to the Archiver is empty.")

  unlink(temp_dir, recursive = TRUE)
})
