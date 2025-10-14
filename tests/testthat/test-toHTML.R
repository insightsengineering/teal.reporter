testthat::describe("toHTML generates image tags", {
  it("from ggplot2 object", {
    testthat::skip_if_not_installed("ggplot2")
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point()
    result <- toHTML(plot)
    testthat::expect_snapshot(result)
  })

  it("from grob", {
    testthat::skip_if_not_installed("grid")
    grob <- grid::rectGrob()
    result <- toHTML(grob)
    testthat::expect_snapshot(result)
  })

  it("from recordedplot", {
    q <- within(teal.code::qenv(), ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point())
    recorded_plot <- teal.code::get_outputs(q)[[1]]
    result <- toHTML(recorded_plot)
    testthat::expect_snapshot(result)
  })
})

testthat::describe("toHTML generates", {
  testthat::test_that("toHTML generates a code chunk with pre element", {
    skip("accordion sets random attributes to the html output")
    result <- toHTML(code_chunk("1"))
    testthat::expect_snapshot(result)
  })

  testthat::test_that("toHTML generates an output chunk that is similar to its contents", {
    result <- toHTML(structure("1", class = "chunk_output"))
    testthat::expect_snapshot(result)
  })

  testthat::test_that("toHTML generates output from teal_card in teal_report", {
    skip("accordion sets random attributes to the html output")
    q <- within(teal_report(), 1 + 1)
    result <- toHTML(q)
    testthat::expect_snapshot(result)
  })

  testthat::test_that("toHTML can be locally overwritten", {
    toHTML.teal_card <- function(...) "Function was overwritten" # nolint: object_name.
    card <- teal_card("## Header")
    testthat::expect_equal(toHTML(card), "Function was overwritten")
  })
})


testthat::describe("toHTML generates a shiny tag", {
  it("for gtsummary", {
    result <- toHTML(gtsummary::as_gtsummary(mtcars))
    testthat::expect_snapshot(result)
  })

  it("for rlistings", {
    result <- toHTML(rlistings::as_listing(mtcars))
    testthat::expect_snapshot(result)
  })

  it("for teal_card", {
    skip("accordion sets random attributes to the html output")
    card <- teal_card("## Header")
    result <- toHTML(card)
    testthat::expect_snapshot(result)
  })

  it("for ReportCard", {
    skip("accordion sets random attributes to the html output")
    card <- ReportCard$new()
    card$append_text("a text")
    result <- toHTML(card)
    testthat::expect_snapshot(result)
  })
})
