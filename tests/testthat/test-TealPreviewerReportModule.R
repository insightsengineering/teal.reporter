testthat::test_that("teal_reporter_previewer returns a teal_module instance",
  testthat::expect_true(inherits(teal_reporter_previewer("LABEL"), "teal_module"))
)
