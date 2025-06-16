testthat::test_that("c.teal_report concatenates multiple teal_report objects and concatenates teal_card objects", {
  treport1 <- teal_report(teal_card = teal_card("Text 1"))
  treport2 <- teal_report(teal_card = teal_card("Text 2"))
  treport3 <- teal_report(teal_card = teal_card("Text 3"))

  combined_treport <- c(treport1, treport2, treport3)

  testthat::expect_length(teal_card(combined_treport), 3)
  testthat::expect_equal(teal_card(combined_treport), c(teal_card(treport1), teal_card(treport2), teal_card(treport3))) 
})
