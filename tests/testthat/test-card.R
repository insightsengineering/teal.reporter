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
