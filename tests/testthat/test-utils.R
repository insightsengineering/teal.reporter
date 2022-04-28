testthat::test_that("extract_addcard_input throws error when input is not a reactivevalues", {
  vals <- list()
  testthat::expect_error(isolate(extract_addcard_input(vals)))
})

testthat::test_that("extract_addcard_input returns empty id if there is no match", {
  vals <- shiny::reactiveValues(a = 1, b = 2)
  testthat::expect_identical(
    isolate(extract_addcard_input(vals)),
    NULL
  )
})

testthat::test_that("extract_addcard_input returns right id when there is a correct match", {
  vals <- shiny::reactiveValues(a = 1, b = 2, `addReportCardButton` = 0)
  testthat::expect_identical(
    isolate(extract_addcard_input(vals)),
    0
  )
})

testthat::test_that("correct match 2", {
  vals <- shiny::reactiveValues(a = 1, b = 2, `teal-addReportCard-addReportCardButton` = 0)
  testthat::expect_identical(
    isolate(extract_addcard_input(vals)),
    0
  )
})

testthat::test_that("extract_addcard_input returns empty id if there is double match", {
  vals <- shiny::reactiveValues(
    a = 1,
    b = 2,
    `addReportCard-addReportCard` = 0,
    `addReportCard2-addReportCard` = 0
  )
  testthat::expect_identical(
    isolate(extract_addcard_input(vals)),
    NULL
  )
})
