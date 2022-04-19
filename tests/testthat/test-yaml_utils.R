testthat::test_that("yaml_quoted adds the `quoted` attribute equal to `TRUE`", {
  object <- "test"
  yaml_quoted_object <- yaml_quoted(object)
  testthat::expect_equal(attr(yaml_quoted_object, "quoted"), TRUE)
})

testthat::test_that("yaml_quoted does not modify the value of the object", {
  object <- "test"
  yaml_quoted_object <- yaml_quoted(object)
  testthat::expect_true(object == yaml_quoted_object)
})
