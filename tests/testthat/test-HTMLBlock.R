testthat::test_that("HTMLBlock object can be created", {
  testthat::expect_no_error(HTMLBlock$new())
})

testthat::test_that("new returns an object of type HTMLBlock", {
  testthat::expect_true(inherits(HTMLBlock$new(), "HTMLBlock"))
})


testthat::test_that("new accepts a shiny.tag", {
  testthat::expect_no_error(HTMLBlock$new(shiny::tags$div()))
})

testthat::test_that("new accepts a shiny.tag.list", {
  testthat::expect_no_error(HTMLBlock$new(shiny::tagList()))
})

testthat::test_that("new doesn't accept character", {
  testthat::expect_error(HTMLBlock$new("test"), "'shiny.tag'/'shiny.tag.list'")
})

testthat::test_that("get_content returns a html content asis", {
  content <- shiny::tags$div()
  obj <- HTMLBlock$new(content)
  testthat::expect_identical(obj$get_content(), content)
})

testthat::test_that("to_list returns a list containing a content (asis)", {
  content <- shiny::tags$div()
  obj <- HTMLBlock$new(content)
  out <- obj$to_list()
  testthat::expect_identical(out, list(content = content))
})

testthat::test_that("from_list creates a HTMLBlock", {
  list <- list(content = shiny::tags$div())
  obj <- HTMLBlock$new()
  obj$from_list(list)
  testthat::expect_identical(obj$get_content(), list$content)
})
