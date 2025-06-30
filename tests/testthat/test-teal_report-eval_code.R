testthat::describe("eval_code appends code_chunks to the teal_card", {
  it("code as code_chunk", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L")
    testthat::expect_equal(
      teal_card(q),
      teal_card(
        code_chunk("a <- 1L"),
        code_chunk("b <- 2L"),
        code_chunk("c <- 3L")
      ),
      ignore_attr = TRUE
    )
  })

  it("code as code_chunk and its output as chunk_output", {
    q <- eval_code(teal_report(), "a <- 1L;a")
    testthat::expect_equal(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("a"),
        structure(1L, class = c("chunk_output", "integer"))
      ),
      ignore_attr = TRUE
    )
  })

  it("code as code_chunk and condition is excluded from output", {
    q <- eval_code(teal_report(), "warning('test')")
    testthat::expect_equal(
      teal_card(q),
      c(teal_card(), code_chunk("warning('test')")),
      ignore_attr = TRUE
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
    testthat::expect_equal(
      teal_card(q),
      c(
        teal_card(),
        code_chunk("a <- 1L"),
        code_chunk("b <- 2L"),
        code_chunk("c <- 3L")
      ),
      ignore_attr = TRUE
    )
  })
})
