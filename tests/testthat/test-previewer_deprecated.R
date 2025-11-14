# Set of tests for deprecated code blocks used for backwards compatibility
# TODO: remove on next release

testthat::describe("TextBlock", {
  it("get_content gets properly formatted header2", {
    tb <- TextBlock$new("This is a text block.", "header2")

    testthat::expect_identical(tb$get_content(), "## This is a text block.")
  })

  it("get_content gets properly formatted header3", {
    tb <- TextBlock$new("This is a text block.", "header3")

    testthat::expect_identical(tb$get_content(), "### This is a text block.")
  })

  it("get_content gets properly formatted verbatim", {
    tb <- TextBlock$new("This is a text block.", "verbatim")

    testthat::expect_identical(tb$get_content(), "```\nThis is a text block.\n```\n")
  })

  it("get_content gets properly formatted paragraph", {
    tb <- TextBlock$new("This is a text block.")

    testthat::expect_identical(tb$get_content(), "This is a text block.")
  })
})

testthat::describe("RcodeBlock", {
  it("get_content gets properly formatted R code chunk", {
    rc <- RcodeBlock$new("summary(cars)")

    testthat::expect_match(
      rc$get_content(),
      "^```[{][rR].*[}].*summary[(]cars[)].*```[ \n]*$"
    )
  })

  it("get_content gets properly formatted R code chunk for powerpoint presentations", {
    rc <- RcodeBlock$new(
      paste(
        sep = "\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16",
        "17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30",
        "summary(cars)"
      )
    )

    testthat::expect_match(
      rc$get_content("powerpoint_presentation"),
      "```{r, echo=FALSE}\ncode_block(\n\"summary(cars)\")\n```\n",
      fixed = TRUE
    )
  })
})
