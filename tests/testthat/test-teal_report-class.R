testthat::describe("teal_card built from teal_data", {
  code <- c(
      "aa <- 1",
      "bb <- 2",
      "mtcars <- mtcars",
      "iris <- iris",
      "aa",
      "bb",
      "plot(1:10)"
    )
  
  it("is identical when running via eval_code with teal_report object", {
    td <- Reduce(
      f = function(result, this) eval_code(result, this),
      init = teal.data::teal_data(),
      x = code
    )

    tr <- Reduce(
      f = function(result, this) eval_code(result, this),
      init = teal_report(),
      x = code
    )

    testthat::expect_equal(unname(teal_card(td)), unname(teal_card(tr)))
  })
  
  it("is identical when calling as.teal_report", {
    td <- Reduce(
      f = function(result, this) eval_code(result, this),
      init = teal.data::teal_data(),
      x = code
    )

    tr <- Reduce(
      f = function(result, this) eval_code(result, this),
      init = teal_report(),
      x = code
    )

    testthat::expect_equal(unname(teal_card(as.teal_report(td))), unname(teal_card(tr)))
  })
})

testthat::test_that("teal_data converts to teal_report when assigning teal_card", {
  td <- teal.data::teal_data()
  teal_card(td) <- teal_card("# A title")

  testthat::expect_s4_class(td, "teal_report")
})
