testthat::describe(".onLoad: Initialised options on package load", {
  it("that are unset are loaded with defaults", {
    testthat::skip_if(getOption("testthat_interactive"))
    withr::with_options(
      list(teal.reporter.devices.dev.width = NULL),
      {
        expect_no_error(.onLoad())
        expect_equal(getOption("teal.reporter.devices.dev.width"), 800)
      }
    )
  })

  it("are retained and not overwritten", {
    testthat::skip_if(getOption("testthat_interactive"))
    withr::with_options(
      list(teal.reporter.devices.dev.width = 500),
      {
        expect_no_error(.onLoad())
        expect_equal(getOption("teal.reporter.devices.dev.width"), 500)
      }
    )
  })
})

testthat::test_that(".onAttach: packageStartupMessage", {
  testthat::skip_if(getOption("testthat_interactive"))

  testthat::with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base",
    code = expect_message(
      .onAttach(),
      regexp = "For better code formatting, consider installing the formatR package.",
      fixed = TRUE
    )
  )
})