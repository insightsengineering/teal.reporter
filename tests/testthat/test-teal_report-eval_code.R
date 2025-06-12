testthat::describe("keep_output stores the objects in teal_card", {
  it("using eval_code and explicit reference", {
    q <- eval_code(teal_report(), "a <- 1L;b <-2L;c<- 3L", keep_output = "b")
    testthat::expect_identical(teal_card(q)[[length(teal_card(q))]], 2L)
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
    testthat::expect_identical(teal_card(q)[[length(teal_card(q))]], 1L)
  })

  it("with multiple explicit object references", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = c("c", "a"))
    testthat::expect_identical(teal_card(q)[[length(teal_card(q)) - 1]], 3L)
    testthat::expect_identical(teal_card(q)[[length(teal_card(q))]], 1L)
  })

  it("without explicit reference (keep all)", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = TRUE)
    testthat::expect_identical(teal_card(q)[-1], teal_card(1L, 2L, 3L))
  })

  it("without explicit reference (keep all)", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L")
    q2 <- eval_code(q, "d <- 4L; e <- 5L", keep_output = TRUE)
    testthat::expect_identical(teal_card(q2)[-c(1, 2)], teal_card(4L, 5L))
  })

  it("without explicit reference (keep all and in alphabetical order)", {
    q <- eval_code(teal_report(), "a <- 1L;z <- 2L;c <- 3L", keep_output = TRUE)
    testthat::expect_identical(teal_card(q)[-1], teal_card(1L, 3L, 2L))
  })

  it("without explicit reference returing none", {
    q <- eval_code(teal_report(), "a <- 1L;z <- 2L;c <- 3L", keep_output = FALSE)
    testthat::expect_identical(teal_card(q)[-1], teal_card())
  })
})
