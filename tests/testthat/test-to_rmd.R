testthat::describe("to_rmd", {
  r_block_start_regexp <- "^```[{][rR].*[}]"
  md_block_end_regexp <- "```[ \n]*$"
  testthat::skip_if_not_installed("withr")
  withr::local_dir(withr::local_tempdir())

  it("character arguments return themselves", {
    x <- "This is a test string."
    result <- to_rmd(x)
    testthat::expect_identical(result, x)
  })

  it("ggplot objects are converted to code chunks", {
    testthat::skip_if_not_installed("ggplot2")
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    result <- to_rmd(p)
    testthat::expect_match(result, r_block_start_regexp)
    testthat::expect_match(result, "readRDS[(].*[)]")
    testthat::expect_match(result, md_block_end_regexp)
  })

  it("data frames are converted to code chunks", {
    df <- data.frame(x = 1:5, y = letters[1:5])
    result <- to_rmd(df)
    testthat::expect_match(result, r_block_start_regexp)
    testthat::expect_match(result, "readRDS[(].*[)]")
    testthat::expect_match(result, md_block_end_regexp)
  })

  it("code_chunk objects are converted correctly", {
    code <- code_chunk("summary(cars)")
    result <- to_rmd(code)
    testthat::expect_match(result, r_block_start_regexp)
    testthat::expect_match(result, "summary[(]cars[)]")
    testthat::expect_match(result, md_block_end_regexp)
  })

  it("simpleCondition objects are converted to text blocks", {
    cond <- simpleCondition("This is a warning message.")
    result <- to_rmd(cond)
    testthat::expect_match(result, "This is a warning message.")
  })

  it("flextable objects are converted to code chunks", {
    testthat::skip_if_not_installed("flextable")
    ft <- flextable::flextable(head(iris))
    result <- to_rmd(ft)
    testthat::expect_match(result, r_block_start_regexp)
    testthat::expect_match(result, "readRDS[(].*[)]")
    testthat::expect_match(result, md_block_end_regexp)
  })

  it("teal_card adds a header with chunk opts", {
    card <- teal.reporter::teal_card("chunk 1", "chunk 2")
    result <- to_rmd(card)
    testthat::expect_match(
      result,
      sprintf(
        "%s.*knitr::opts_chunk[$]set.*%s",
        r_block_start_regexp,
        "```[ \n]*"
      )
    )
  })

  it("teal_card render each element", {
    card <- teal.reporter::teal_card("chunk 1", "chunk 2")
    result <- to_rmd(card)
    checkmate::expect_string(result)
    testthat::expect_match(result, "chunk 1\n\nchunk 2")
  })
})
