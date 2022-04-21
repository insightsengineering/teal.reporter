testthat::test_that("error if use not a reactivevalues", {
  vals <- list()
  testthat::expect_error(isolate(extract_addcard_id(vals)))
})

testthat::test_that("empty id if there is no match", {
  vals <- shiny::reactiveValues(a = 1, b = 2)
  testthat::expect_identical(
    isolate(extract_addcard_id(vals)),
    "not_exists_id"
  )
})

testthat::test_that("correct match", {
  vals <- shiny::reactiveValues(a = 1, b = 2, `addReportCardButton` = 0)
  testthat::expect_identical(
    isolate(extract_addcard_id(vals)),
    "addReportCardButton"
  )
})

testthat::test_that("correct match 2", {
  vals <- shiny::reactiveValues(a = 1, b = 2, `teal-addReportCard-addReportCardButton` = 0)
  testthat::expect_identical(
    isolate(extract_addcard_id(vals)),
    "teal-addReportCard-addReportCardButton"
  )
})

testthat::test_that("return empty id if there is double match", {
  vals <- shiny::reactiveValues(
    a = 1,
    b = 2,
    `addReportCard-addReportCard` = 0,
    `addReportCard2-addReportCard` = 0
  )
  testthat::expect_identical(
    isolate(extract_addcard_id(vals)),
    "not_exists_id"
  )
})
