testthat::describe("to_rmd default support: ", {
  it("character object return themselves", {
    x <- "This is a test string."
    result <- to_rmd(x)
    testthat::expect_identical(result, x)
  })

  it("unknown object return themselves", {
    x <- structure(list("This is a test object"), class = "unknown_class")
    result <- to_rmd(x)
    testthat::expect_identical(result, x)
  })

  it("code_chunk object are converted correctly", {
    code <- code_chunk("summary(cars)")
    testthat::expect_match(
      to_rmd(code),
      "^```[{][rR].*[}].*summary[(]cars[)].*```[ \n]*$"
    )
  })

  it("simpleCondition object are converted to text blocks", {
    cond <- simpleCondition("This is a warning message.")
    result <- to_rmd(cond)
    testthat::expect_match(result, "This is a warning message.")
  })

  it("teal_card adds a header with chunk opts", {
    card <- teal.reporter::teal_card("chunk 1", "chunk 2")
    result <- to_rmd(card)
    testthat::expect_match(
      result,
      "^```[{][rR].*[}].*.*knitr::opts_chunk[$]set.*```[ \n]*"
    )
  })

  it("teal_card render each element", {
    card <- teal.reporter::teal_card("chunk 1", "chunk 2")
    result <- to_rmd(card)
    checkmate::expect_string(result)
    testthat::expect_match(result, "chunk 1\n\nchunk 2")
  })

  it("teal_card adds code_block declaration", {
    card <- teal_card("1")
    metadata(card, "output") <- "powerpoint_presentation"
    testthat::expect_match(
      to_rmd(card),
      "code_block[ ]*<-[ ]*function[(]code_text[)]"
    )
  })
})

testthat::describe("to_rmd generating blocks with rds auxiliary files", {
  testthat::skip_if_not_installed("withr")
  withr::local_dir(withr::local_tempdir())

  expect_rds_generation <- function(result) {
    testthat::expect_match(
      result,
      "^```[{][rR].*[}].*readRDS[(].*[)].*```[ \n]*$"
    )
    pattern <- "\\.?/[^\\n]+\\.(rds|RDS)"
    matches <- gregexpr(pattern, result)
    paths <- regmatches(result, matches)
    testthat::expect_length(paths, 1)
    checkmate::expect_file_exists(paths[[1]])
  }

  it("ggplot objects are converted to code chunks with readRDS", {
    testthat::skip_if_not_installed("ggplot2")
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    expect_rds_generation(to_rmd(p))
  })

  it("data frames are converted to code chunks with readRDS", {
    expect_rds_generation(to_rmd(data.frame(x = 1:5, y = letters[1:5])))
  })

  it("flextable objects are converted to code chunks with readRDS", {
    testthat::skip_if_not_installed("flextable")
    expect_rds_generation(to_rmd(flextable::flextable(head(iris))))
  })
})

testthat::describe("to_rmd declaration", {
  it("of existing character method", {
    to_rmd.character <- function(block, ...) { # nolint: object_name_linter.
      paste0("Character method: ", block)
    }
    testthat::expect_equal(to_rmd("test"), "Character method: test")
  })

  it("of new class", {
    to_rmd.testthat_internal <- function(block, ...) { # nolint: object_name_linter.
      paste0("internal method: ", block)
    }
    testthat::expect_equal(
      to_rmd(structure("test", class = "testthat_internal")),
      "internal method: test"
    )
  })
})

testthat::describe("to_rmd uses custom dimensions of plots", {
  testthat::skip_if_not_installed("withr")
  withr::local_dir(withr::local_tempdir())

  it("and throws warning when they are too small", {
    testthat::skip_if_not_installed("ggplot2")
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    attr(p, "dev.width") <- 100
    attr(p, "dev.height") <- 100
    testthat::expect_warning(to_rmd(p), "Figure dimensions too small")
  })

  it("in inches", {
    testthat::skip_if_not_installed("ggplot2")
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    dpi <- formals(.determine_default_dimensions)$dpi
    attr(p, "dev.width") <- dpi * 3 # 96 dpi
    attr(p, "dev.height") <- dpi * 4 # 96 dpi

    result <- to_rmd(p)
    testthat::expect_match(result, "fig[.]width [=] 3.[0]+")
    testthat::expect_match(result, "fig[.]height [=] 4.[0]+")
  })
})
