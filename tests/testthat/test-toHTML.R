testthat::describe("toHTML generates image tags", {
  it("from ggplot2 object", {
    testthat::skip_if_not_installed("ggplot2")
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
    result <- toHTML(plot)
    testthat::expect_s3_class(result, "shiny.tag")
    testthat::expect_equal((result$name), "img")
    testthat::expect_match(result$attribs$src, "^data:image/png;base64,")
  })

  it("from grob", {
    testthat::skip_if_not_installed("grid")
    grob <- grid::rectGrob()
    result <- toHTML(grob)
    testthat::expect_s3_class(result, "shiny.tag")
    testthat::expect_equal(result$name, "img")
    testthat::expect_match(result$attribs$src, "^data:image/png;base64,")
  })

  it("from recordedplot", {
    q <- within(teal.code::qenv(), ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point())
    recorded_plot <- teal.code::get_outputs(q)[[1]]
    result <- toHTML(recorded_plot)
    testthat::expect_s3_class(result, "shiny.tag")
    testthat::expect_equal(result$name, "img")
    testthat::expect_match(result$attribs$src, "^data:image/png;base64,")
  })
})

testthat::test_that("toHTML generates a code chunk with pre element", {
  inner_value <- "1"
  result <- toHTML(code_chunk(inner_value))
  result2 <- toHTML(inner_value)
  testthat::expect_equal(result$name, "pre")
})

testthat::test_that("toHTML generates an output chunk that is similar to its contents", {
  inner_value <- "1"
  result <- toHTML(structure(inner_value, class = "chunk_output"))
  testthat::expect_equal(result, toHTML(inner_value))
})

testthat::test_that("toHTML generates output from teal_card in teal_report", {
  q <- within(teal_report(), 1 + 1)
  result <- toHTML(q)
  testthat::expect_equal(result, toHTML(teal_card(q)))
})

testthat::describe("toHTML generates a shiny tag", {
  it("for gtsummary", {
    result <- toHTML(gtsummary::as_gtsummary(mtcars))
    checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  })

  it("for rlistings", {
    result <- toHTML(rlistings::as_listing(mtcars))
    checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  })

  it("for teal_card", {
    result <- toHTML(teal_card("## Header"))
    checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  })

  it("for teal_report", {
    q <- within(teal_report(), 1 + 1)
    result <- toHTML(q)
    checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  })

  it("for ReportCard", {
    card <- ReportCard$new()
    card$append_text("a text")
    result <- toHTML(card)
    checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
    testthat::expect_match(as.character(result), "a text")
  })
})

testthat::test_that("toHTML can be locally overwritten", {
  toHTML.teal_card <- function(...) "Function was overwritten"
  testthat::expect_equal(toHTML(teal_card("## Header")), "Function was overwritten")
})
