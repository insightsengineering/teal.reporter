testthat::test_that("get_bs_version", {
  testthat::expect_identical(get_bs_version(), "3")
})

testthat::test_that("panel_item", {
  testthat::expect_s3_class(panel_item("LABEL", shiny::tags$div()), "shiny.tag")
})
