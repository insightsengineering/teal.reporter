test_that("[.teal_report keeps the teal card", {
  q <- within(teal.reporter::teal_report(), {
    iris <- iris
    mtcars <- mtcars
  })
  testthat::expect_equal(teal_card(q["iris"]), teal_card(q))
})
