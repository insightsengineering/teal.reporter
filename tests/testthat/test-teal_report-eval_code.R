testthat::describe("keep_output stores the objects in teal_card", {
  it("using eval_code and explicit reference", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = "b")
    testthat::expect_equal(
      teal_card(q),
      teal_card(
        code_chunk("a <- 1L;b <- 2L;c <- 3L"),
        structure(2L, class = c("chunk_output", "integer"))
      ),
      ignore_attr = TRUE
    )
  })

  it("using within and explicit reference", {
    q <- within(teal_report(),
      {
        a <- 1L
        b <- 2L
        c <- 3L
      },
      keep_output = "b"
    )
    testthat::expect_equal(
      teal_card(q),
      teal_card(
        code_chunk("a <- 1L\nb <- 2L\nc <- 3L"),
        structure(2L, class = c("chunk_output", "integer"))
      ),
      ignore_attr = TRUE
    )
  })

  it("with multiple explicit object references", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = c("a", "b"))
    testthat::expect_equal(
      teal_card(q),
      teal_card(
        code_chunk("a <- 1L;b <- 2L;c <- 3L"),
        structure(1L, class = c("chunk_output", "integer")),
        structure(2L, class = c("chunk_output", "integer"))
      ),
      ignore_attr = TRUE
    )
  })

  it("without explicit reference returing none", {
    q <- eval_code(teal_report(), "a <- 1L;b <- 2L;c <- 3L", keep_output = character(0L))
    testthat::expect_equal(
      teal_card(q),
      teal_card(code_chunk("a <- 1L;b <- 2L;c <- 3L")),
      ignore_attr = TRUE
    )
  })
})
