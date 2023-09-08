testthat::test_that("get_bs_version", {
  testthat::expect_identical(get_bs_version(), "3")
})

testthat::test_that("panel_item", {
  testthat::expect_s3_class(panel_item("LABEL", shiny::tags$div()), "shiny.tag")
})

testthat::test_that("Test to_flextable", {
  data_frame <- data.frame(A = 1:3, B = 4:6)
  flextable_output <- to_flextable(data_frame)
  testthat::expect_s3_class(flextable_output, "flextable")
})
