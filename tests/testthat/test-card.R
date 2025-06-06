testthat::test_that("card creates an empty document", {
  doc <- teal_card()
  testthat::expect_s3_class(doc, "teal_card")
  testthat::expect_length(doc, 0)
})

testthat::test_that("card creates a document with initial elements", {
  doc <- teal_card("a", list(1, 2), code_chunk("print('hi')"))
  testthat::expect_s3_class(doc, "teal_card")
  testthat::expect_length(doc, 3)
  testthat::expect_equal(doc[[1]], "a")
  testthat::expect_equal(doc[[2]], list(1, 2))
})

testthat::describe("c.card combines with", {
  doc_base <- teal_card("a", "b")

  it("character and retains class", {
    doc_result <- c(doc_base, "c")
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 3)
    testthat::expect_equal(doc_result[[3]], "c")
  })

  it("list and retains class", {
    doc_result <- c(doc_base, list(1, 2))
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[3]], 1)
    testthat::expect_equal(doc_result[[4]], 2)
  })

  it("NULL and retains class", {
    doc_result <- c(doc_base, NULL)
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 2)
  })

  it("card with multiple elements and retains class", {
    doc_result <- c(doc_base, teal_card("c", "d"))
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[3]], "c")
    testthat::expect_equal(doc_result[[4]], "d")
  })

  it("ggplot and retains class", {
    plot <- ggplot2::ggplot(iris) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
    doc_result <- c(doc_base, plot)
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 3)
    testthat::expect_s3_class(doc_result[[3]], "ggplot")
  })

  it("ggplot with title and retains class", {
    plot <- ggplot2::ggplot(iris) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
    doc_result <- c(doc_base, teal_card("# Plot", plot))
    testthat::expect_s3_class(doc_result, "teal_card")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[3]], "# Plot")
    testthat::expect_s3_class(doc_result[[4]], "ggplot")
  })
})

testthat::test_that("[.card subsets and retains class", {
  doc <- teal_card("a", "b", "c", "d")
  sub_doc <- doc[c(1, 3)]
  testthat::expect_s3_class(sub_doc, "teal_card")
  testthat::expect_length(sub_doc, 2)
  testthat::expect_equal(sub_doc[[1]], "a")
  testthat::expect_equal(sub_doc[[2]], "c")

  empty_sub_doc <- doc[0]
  testthat::expect_s3_class(empty_sub_doc, "teal_card")
  testthat::expect_length(empty_sub_doc, 0)
})

testthat::test_that("edit_card modifies elements", {
  doc <- teal_card("a", "b", "c")
  edited_doc <- edit_teal_card(doc, modify = c(3, 1))
  testthat::expect_s3_class(edited_doc, "teal_card")
  testthat::expect_length(edited_doc, 2)
  testthat::expect_equal(edited_doc[[1]], "c")
  testthat::expect_equal(edited_doc[[2]], "a")
})

testthat::test_that("edit_card appends elements", {
  doc <- teal_card("a", "b")
  edited_doc <- edit_teal_card(doc, append = "c")
  testthat::expect_s3_class(edited_doc, "teal_card")
  testthat::expect_length(edited_doc, 3)
  testthat::expect_equal(edited_doc[[3]], "c")

  edited_doc_after <- edit_teal_card(doc, append = "c", after = 1)
  testthat::expect_s3_class(edited_doc_after, "teal_card")
  testthat::expect_length(edited_doc_after, 3)
  testthat::expect_equal(edited_doc_after[[1]], "a")
  testthat::expect_equal(edited_doc_after[[2]], "c")
  testthat::expect_equal(edited_doc_after[[3]], "b")
})

testthat::test_that("edit_card modifies and appends", {
  doc <- teal_card("a", "b", "c", "d")
  edited_doc <- edit_teal_card(doc, modify = c(4, 1), append = "e", after = 1)
  testthat::expect_s3_class(edited_doc, "teal_card")
  testthat::expect_length(edited_doc, 3)
  testthat::expect_equal(edited_doc[[1]], "d")
  testthat::expect_equal(edited_doc[[2]], "e")
  testthat::expect_equal(edited_doc[[3]], "a")
})

testthat::test_that("edit_card preserves attributes", {
  doc <- teal_card("a")
  attr(doc, "test") <- "test"
  edited_doc <- edit_teal_card(doc, append = "b")
  testthat::expect_equal(attr(edited_doc, "test"), "test")
})
