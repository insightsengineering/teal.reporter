testthat::describe("toHTML generates image tags", {
  it("from ggplot2 object", {
    testthat::skip_if_not_installed("ggplot2")
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point()
    result <- toHTML(plot)
    testthat::expect_equal(result$name, "img")
  })

  it("from grob", {
    testthat::skip_if_not_installed("grid")
    grob <- grid::rectGrob()
    result <- toHTML(grob)
    testthat::expect_equal(result$name, "img")
  })

  it("from recordedplot", {
    q <- within(teal.code::qenv(), ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point())
    recorded_plot <- teal.code::get_outputs(q)[[1]]
    result <- toHTML(recorded_plot)
    testthat::expect_equal(result$name, "img")
  })
})

testthat::describe("toHTML generates", {
  testthat::test_that("toHTML generates a code chunk as code tag with class 'language-$lang'", {
    result <- toHTML(code_chunk("this-unique-code", lang = "C--"))
    html <- as.character(result$children[[1]])
    testthat::expect_match(html, "<code class=\"language-C--\">this-unique-code</code>")
  })

  testthat::test_that("toHTML generates tags based on markdown syntax", {
    testthat::expect_identical(as.character(toHTML("1")), "<p>1</p>\n")
    testthat::expect_identical(as.character(toHTML("# test")), "<h1>test</h1>\n")
    testthat::expect_identical(as.character(toHTML("## test")), "<h2>test</h2>\n")
    testthat::expect_identical(as.character(toHTML("### test")), "<h3>test</h3>\n")
    testthat::expect_identical(
      as.character(toHTML("```C++\ntest\n```")),
      "<pre><code class=\"language-C++\">test\n</code></pre>\n"
    )
    testthat::expect_identical(as.character(toHTML("**test**")), "<p><strong>test</strong></p>\n")
    testthat::expect_identical(as.character(toHTML(">test")), "<blockquote>\n<p>test</p>\n</blockquote>\n")
    testthat::expect_identical(as.character(toHTML("1. test\n2. test")), "<ol>\n<li>test</li>\n<li>test</li>\n</ol>\n")
  })

  testthat::test_that("toHTML generates an output based on its content", {
    result <- toHTML(structure("1", class = "chunk_output"))
    expected <- toHTML("1")
    testthat::expect_identical(result, expected)
  })

  testthat::test_that("toHTML generates output from teal_card in teal_report", {
    q <- within(teal_report(), 1)
    teal_card(q) <- c("# Header", teal_card(q))
    result <- toHTML(q)
    html <- as.character(result)
    testthat::expect_match(html, "<h1>Header</h1>.+<code class=\"language-R\">1</code>")
  })

  testthat::test_that("toHTML can be locally overwritten", {
    toHTML.teal_card <- function(...) "Function was overwritten" # nolint: object_name.
    card <- teal_card("## Header")
    testthat::expect_equal(toHTML(card), "Function was overwritten")
  })
})


testthat::describe("toHTML", {
  it("for gtsummary converts into styled html table", {
    skip("too complicated as table contains random elements - possible to test with webshot")
    result <- toHTML(gtsummary::as_gtsummary(mtcars))
  })

  it("for rlistings converts into styled html table", {
    skip("too complicated as table contains random elements - possible to test with webshot")
    result <- toHTML(rlistings::as_listing(mtcars))
  })

  it("for teal_card concatenates each element into single html text", {
    card <- teal_card("# Title", "## Header", "paragraph")
    names(card) <- c("a", "b", "c")
    result <- toHTML(card)
    expected <- tagList(list(
      a = toHTML("# Title"),
      b = toHTML("## Header"),
      c = toHTML("paragraph")
    ))
    testthat::expect_identical(result, expected)
  })

  it("for ReportCard converts its content toHTML", {
    card <- ReportCard$new()
    card$append_text("# test")
    card$append_rcode("a <- b")
    result <- toHTML(card)
    expected <- toHTML(teal_card("# test", code_chunk("a <- b")))
    testthat::expect_identical(
      gsub("[0-9]{3,4}", "", result), # gsub because bslib::accordion adds random id to the attrs
      gsub("[0-9]{3,4}", "", expected)
    )
  })
})
