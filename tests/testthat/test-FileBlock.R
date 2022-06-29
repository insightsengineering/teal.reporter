testthat::test_that("FileBlock object can be created", {
  testthat::expect_error(FileBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type FileBlock", {
  testthat::expect_true(inherits(FileBlock$new(), "FileBlock"))
})

testthat::test_that("destructor removes the file", {
  temp_file <- tempfile(fileext = ".png")
  file.create(temp_file)
  testthat::expect_true(file.exists(temp_file))
  block <- FileBlock$new()$set_content(temp_file)
  rm(block)
  gc()
  testthat::expect_false(file.exists(temp_file))
})

testthat::test_that("to_list returns a named list with a one field", {
  block <- FileBlock$new()
  temp_dir <- tempdir()
  testthat::expect_equal(block$to_list(temp_dir), list(basename = character(0)))
})

testthat::test_that("to_list with base_path arg", {
  block <- TableBlock$new()
  testthat::expect_identical(
    block$to_list(dirname(block$get_content())),
    list(basename = character(0))
  )
})

testthat::test_that("to_list copies a file to a target directory", {
  temp_dir_name <- file.path(tempdir(), "test")
  dir.create(temp_dir_name)
  temp_file <- tempfile(fileext = ".png")
  file.create(temp_file)
  block <- FileBlock$new()$set_content(temp_file)
  block$to_list(temp_dir_name)

  testthat::expect_true(file.exists(file.path(temp_dir_name, basename(block$get_content()))))

  unlink(temp_dir_name, recursive = TRUE)
  unlink(temp_file)
})

testthat::test_that("from_list copies a file from a target directory", {
  temp_dir_name <- file.path(tempdir(), "test")
  dir.create(temp_dir_name)

  temp_file <- tempfile(fileext = ".rds")
  file.create(temp_file)

  block <- FileBlock$new()$set_content(temp_file)
  file.copy(block$get_content(), file.path(temp_dir_name, basename(block$get_content())))

  new_block <- block$from_list(list(basename = basename(block$get_content())), temp_dir_name)
  testthat::expect_true(file.exists(new_block$get_content()))

  unlink(temp_dir_name, recursive = TRUE)
  unlink(temp_file)
})
