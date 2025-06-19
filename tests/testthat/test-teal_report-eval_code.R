testthat::describe("eval_code appends to teal_card", {
  it("code as code_chunk", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L")
    testthat::expect_identical(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("b <- 2L"),
        code_chunk("c <- 3L")
      )
    )
  })

  it("code as code_chunk and its output as chunk_output", {
    q <- eval_code(teal_report(), "a <- 1L;a")
    testthat::expect_identical(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("a"),
        structure(1L, class = c("chunk_output", "integer"))
      )
    )
  })
})

testthat::describe("within appends to teal_card", {
  it("code as code_chunk", {
    q <- within(teal_report(), {
      a <- 1L
      b <- 2L
      c <- 3L
    })
    testthat::expect_identical(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("b <- 2L"),
        code_chunk("c <- 3L")
      )
    )
  })

  it("code as code_chunk and its output as chunk_output", {
    q <- within(teal_report(), {
      a <- 1L
      a
    })
    testthat::expect_identical(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("a"),
        structure(1L, class = c("chunk_output", "integer"))
      )
    )
  })
})
