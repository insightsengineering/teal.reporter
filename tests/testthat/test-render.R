testthat::describe("render() accepts", {
  it("empty teal_report object", {
    testthat::expect_no_error(render(teal_report(), quiet = TRUE))
  })
  it("empty teal_card object", {
    testthat::expect_no_error(render(teal_card(), quiet = TRUE))
  })
  it("character object", {
    testthat::expect_no_error(render("# Document", quiet = TRUE))
  })
  it("teal_report containing code chunks and outputs", {
    r <- within(teal_report(), {
      a <- 1:10
      plot(a)
    })
    testthat::expect_no_error(render(r, quiet = TRUE))
  })
})

testthat::describe("render() by default", {
  it("outputs and keeps report.Rmd file in the working directory", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    setwd(temp_dir)
    render(teal_report(), quiet = TRUE)
    testthat::expect_true(file.exists(file.path(temp_dir, "report.Rmd")))
  })

  it("renders report.html file in the working directory", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    setwd(temp_dir)
    render(teal_report(), quiet = TRUE)
    testthat::expect_true(file.exists(file.path(temp_dir, "report.html")))
  })

  it("outputs report.Rmd file containing knitr::opts_chunk$set with tidy options set", {
    render(teal_report(), quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "```{r, include=FALSE}",
        "knitr::opts_chunk$set(list(tidy.opts = list(width.cutoff = 60), tidy = TRUE))",
        "```"
      )
    )
  })
})

testthat::describe("render() outputs report.Rmd with", {
  withr::local_options(teal.reporter.global_knitr = list())
  it("output_dir set to other location then working directory", {
    tr <- teal_report()
    temp_dir <- tempfile()
    render(tr, output_dir = temp_dir, quiet = TRUE)
    testthat::expect_true(file.exists(file.path(temp_dir, "report.Rmd")))
  })

  it("markdown content added to teal_card", {
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading", "Lorem ipsum")
    render(tr, quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(lines, c("# test heading", "Lorem ipsum"))
  })

  it("yaml header containing entries set through metadata", {
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading")
    metadata(teal_card(tr)) <- list(title = "test title", author = "me is tot")
    render(tr, quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "---",
        "title: test title",
        "author: me is tot",
        "---",
        "# test heading"
      )
    )
  })

  it("code_chunk with knitr::opts_chunk$set call using value from teal.reporter.global_knitr", {
    withr::local_options(teal.reporter.global_knitr = list(eval = TRUE, echo = FALSE))
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading")
    render(tr, quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "```{r, include=FALSE}",
        "knitr::opts_chunk$set(list(eval = TRUE, echo = FALSE))",
        "```",
        "",
        "# test heading"
      )
    )
  })

  it("code_chunk with knitr::opts_chunk$set call using value from global_knitr argument", {
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading")
    render(tr, global_knitr = list(echo = TRUE, eval = TRUE), quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "```{r, include=FALSE}",
        "knitr::opts_chunk$set(list(echo = TRUE, eval = TRUE))",
        "```",
        "",
        "# test heading"
      )
    )
  })

  it("arbitrary code chunk with additional parameters", {
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading", code_chunk("a <- 1L", eval = FALSE, echo = FALSE))
    render(tr, quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "# test heading",
        "```{r, eval=FALSE, echo=FALSE}",
        "a <- 1L",
        "```"
      )
    )
  })

  it("arbitrary code cunk but chunk_output is missing", {
    tr <- teal_report()
    tr <- teal.code::eval_code(tr, "plot(1:10)")
    render(tr, quiet = TRUE)
    lines <- base::readLines("report.Rmd", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "```{r}",
        "plot(1:10)",
        "```"
      )
    )
  })
})


testthat::describe("render() renders output based on metadata$output field:", {
  withr::local_options(teal.reporter.global_knitr = list())
  it("- md_document containing markdown content, code chunks and their outputs", {
    tr <- teal_report()
    teal_card(tr) <- c(teal_card(tr), "# test heading", "Lorem ipsum")
    tr <- within(tr, plot(1:10))
    metadata(teal_card(tr)) <- list(output = "md_document")
    render(tr)
    lines <- base::readLines("report.md", warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "# test heading",
        "",
        "Lorem ipsum",
        "",
        "    plot(1:10)",
        "",
        sprintf("![](report_files/figure-markdown_strict/unnamed-chunk-3-1.png)")
      )
    )
  })

  it("- md_document containing absolute path to a plot even if output_dir is set to absolute path", {
    temp_dir <- tempfile()
    tr <- teal_report()
    tr <- within(tr, plot(1:10))
    metadata(teal_card(tr)) <- list(output = "md_document")
    render(tr, output_dir = temp_dir)
    lines <- base::readLines(file.path(temp_dir, "report.md"), warn = FALSE)
    testthat::expect_identical(
      lines,
      c(
        "    plot(1:10)",
        "",
        sprintf("![](report_files/figure-markdown_strict/unnamed-chunk-3-1.png)")
      )
    )
  })
})
