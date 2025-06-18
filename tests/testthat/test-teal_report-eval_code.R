testthat::describe("keep_output stores the objects in teal_card", {
  it("using eval_code and explicit reference", {
    q <- eval_code(teal_report(), "a <- 1L;b <-2L;c<- 3L", keep_output = "b")
    testthat::expect_equal(teal_card(q)[[length(teal_card(q))]], 2L)
  })

  it("using within and explicit reference", {
    q <- within(teal_report(),
      {
        a <- 1L
        b <- 2L
        c <- 3L
      },
      keep_output = "a"
    )
    testthat::expect_equal(teal_card(q)[[length(teal_card(q))]], 1L)
  })

  it("with multiple explicit object references", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = c("c", "a"))
    testthat::expect_equal(teal_card(q)[[length(teal_card(q)) - 1]], 3L)
    testthat::expect_equal(teal_card(q)[[length(teal_card(q))]], 1L)
  })

  it("without explicit reference returing none", {
    q <- eval_code(teal_report(), "a <- 1L;z <- 2L;c <- 3L", keep_output = character(0L))
    testthat::expect_equal(teal_card(q)[-1], teal_card())
  })
})
