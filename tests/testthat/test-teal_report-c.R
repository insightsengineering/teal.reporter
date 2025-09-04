testthat::describe("c.teal_report combines", {
  it("two empty teal_report objects", {
    testthat::expect_equal(c(teal_report(), teal_report()), teal_report())
  })

  it("empty and non-empty teal_report by appending elements of teal_card", {
    treport1 <- teal_report()
    treport2 <- teal_report(teal_card = teal_card("Text 2"))
    testthat::expect_equal(
      teal_card(c(treport1, treport2)),
      teal_card("Text 2"),
      ignore_attr = TRUE
    )
  })

  it("two teal_report by combining elements of teal_card", {
    treport1 <- teal_report(teal_card = teal_card("Text 1"))
    treport2 <- teal_report(teal_card = teal_card("Text 2"))
    testthat::expect_equal(
      teal_card(c(treport1, treport2)),
      teal_card("Text 1", "Text 2"),
      ignore_attr = TRUE
    )
  })

  it("multiple teal_report by combining elements of teal_card", {
    treport1 <- teal_report(teal_card = teal_card("Text 1"))
    treport2 <- teal_report(teal_card = teal_card("Text 2"))
    treport3 <- teal_report()
    treport4 <- teal_report(teal_card = teal_card("Text 2"))

    testthat::expect_equal(
      teal_card(c(treport1, treport2, treport3, treport4)),
      teal_card("Text 1", "Text 2", "Text 2"),
      ignore_attr = TRUE
    )
  })
})
