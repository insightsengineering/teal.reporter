testthat::test_that("ContentBlock object can be created", {
  testthat::expect_no_error(ContentBlock$new())
})

testthat::test_that("new returns an object of type ContentBlock", {
  testthat::expect_true(inherits(ContentBlock$new(), "ContentBlock"))
})

testthat::test_that("set_content accepts a character object", {
  block <- ContentBlock$new()
  testthat::expect_no_error(block$set_content("test"))
})

testthat::test_that("set_content accepts a list object", {
  block <- ContentBlock$new()
  testthat::expect_no_error(block$set_content(list("a")))
})

testthat::test_that("set_content returns the ContentBlock object", {
  block <- ContentBlock$new()
  testthat::expect_identical(block$set_content("test"), block)
})

testthat::test_that("get_content returns NULL on a newly initialized ContentBlock", {
  testthat::expect_equal(ContentBlock$new()$get_content(), NULL)
})

testthat::test_that("The deep copy constructor copies the content file to a new file", {
  original_file <- tempfile()
  writeLines("Test content", con = original_file)
  content_block <- ContentBlock$new()$set_content(original_file)
  content_block_copy <- content_block$clone(deep = TRUE)

  testthat::expect_true(original_file != content_block_copy$get_content())
})

testthat::test_that("The deep copy constructor preserves the file extension of the copied file", {
  original_file <- tempfile(fileext = ".test_extension")
  writeLines("Test content", con = original_file)
  content_block <- ContentBlock$new()$set_content(original_file)
  content_block_copy <- content_block$clone(deep = TRUE)
  split <- strsplit(basename(original_file), split = "\\.")
  copied_file_extension <- split[[1]][length(split[[1]])]

  testthat::expect_equal(copied_file_extension, "test_extension")
})

testthat::test_that("The deep copy constructor does not find an extension of a file name like .gitignore", {
  original_file <- file.path(tempdir(), ".test")
  writeLines("Test content", con = original_file)
  content_block <- ContentBlock$new()$set_content(original_file)
  content_block_copy <- content_block$clone(deep = TRUE)

  testthat::expect_false(grepl(".", basename(content_block_copy$get_content()), fixed = TRUE))
})

testthat::test_that("The deep copy constructor finds an extension of a file name like .gitignore.txt", {
  original_file <- file.path(tempdir(), ".test.test_extension")
  writeLines("Test content", con = original_file)
  content_block <- ContentBlock$new()$set_content(original_file)
  content_block_copy <- content_block$clone(deep = TRUE)
  split <- strsplit(basename(original_file), split = "\\.")
  copied_file_extension <- split[[1]][length(split[[1]])]

  testthat::expect_equal(copied_file_extension, "test_extension")
})

testthat::test_that("The shallow copy constructor does not copy the content file to a new file", {
  original_file <- tempfile()
  writeLines("Test content", con = original_file)
  content_block <- ContentBlock$new()$set_content(original_file)
  content_block_copy <- content_block$clone(deep = FALSE)

  testthat::expect_true(original_file == content_block_copy$get_content())
})
