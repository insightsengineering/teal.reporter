testthat::describe("teal_report initializes", {
  it("silently with all default arguments", {
    testthat::expect_no_error(teal_report())
  })
  it("silently with teal_card", {
    testthat::expect_no_error(teal_report(teal_card = teal_card()))
  })
  it("silently with teal_card being NULL", {
    testthat::expect_no_error(teal_report(teal_card = NULL))
  })
  it("with error when teal_card is not teal_card nor NULL", {
    testthat::expect_error(teal_report(teal_card = list()))
  })
})

testthat::describe("as.teal_report coerces", {
  it("from teal_report", {
    testthat::expect_s4_class(as.teal_report(teal_report()), "teal_report")
  })

  it("from teal_data", {
    testthat::expect_s4_class(as.teal_report(teal.data::teal_data()), "teal_report")
  })
  it("from qenv", {
    testthat::expect_s4_class(as.teal_report(teal.code::qenv()), "teal_report")
  })
})

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
    td <- eval_code(teal.data::teal_data(), code)
    tr <- eval_code(teal_report(), code)

    testthat::expect_equal(unname(teal_card(td)), unname(teal_card(tr)))
  })

  it("is identical when calling as.teal_report", {
    td <- eval_code(teal.data::teal_data(), code)
    tr <- eval_code(teal_report(), code)

    testthat::expect_equal(unname(teal_card(as.teal_report(td))), unname(teal_card(tr)))
  })

  it("is identical when calling as.teal_report with multiple calls", {
    td <- Reduce(f = eval_code, init = teal.data::teal_data(), x = code)
    tr <- Reduce(f = eval_code, init = teal_report(), x = code)

    testthat::expect_equal(unname(teal_card(as.teal_report(td))), unname(teal_card(tr)))
  })

  it("drops conditions produced in a code-chunk evaluation", {
    td <- eval_code(teal.data::teal_data(), "warning(1)")
    testthat::expect_equal(unname(teal_card(td)), unname(teal_card(code_chunk("warning(1)"))))
  })
})

testthat::test_that("teal_data converts to teal_report when assigning teal_card", {
  td <- teal.data::teal_data()
  teal_card(td) <- teal_card("# A title")

  testthat::expect_s4_class(td, "teal_report")
})
