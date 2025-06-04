testthat::test_that("teal_document creates an empty document", {
  doc <- teal_document()
  testthat::expect_s3_class(doc, "teal_document")
  testthat::expect_length(doc, 0)
})

testthat::test_that("teal_document creates a document with initial elements", {
  doc <- teal_document("a", list(1, 2), code_chunk("print('hi')"))
  testthat::expect_s3_class(doc, "teal_document")
  testthat::expect_length(doc, 3)
  testthat::expect_equal(doc[[1]], "a")
  testthat::expect_s3_class(doc[[3]], "code_chunk")
})

testthat::describe("c.teal_document combines with", {
  doc_base <- teal_document("a", "b")

  it("character element and retains class", {
    doc_result <- c(doc_base, "c")
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 3)
    testthat::expect_equal(doc_result[[3]], "c")
  })

  it("multiple character elements and retains class", {
    doc_result <- c(doc_base, "c", list("d"))
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[3]], "c")
  })

  it("multiple character elements and retains class", {
    doc_result <- c(doc_base, "c", list("d", "e"))
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[4]], list("d", "e"))
  })

  it("teal_document with multiple elements and retains class", {
    doc_result <- c(doc_base, teal_document("c", "d"))
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 4)
    testthat::expect_equal(doc_result[[3]], "c") # Assuming it unnests the document
  })

  it("with single ggplot2 element and retains class", {
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    doc_result <- c(doc_base, plot)
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 3)
    testthat::expect_identical(doc_result[[3]], plot)
  })

  it("ggplot2 section and retains class", {
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    doc_result <- c(doc_base, teal_document("# Plot", plot))
    testthat::expect_s3_class(doc_result, "teal_document")
    testthat::expect_length(doc_result, 4)
    testthat::expect_identical(doc_result[[4]], plot)
  })
})

testthat::test_that("[.teal_document subsets and retains class", {
  doc <- teal_document("a", "b", "c", "d")
  sub_doc <- doc[c(1, 3)]
  testthat::expect_s3_class(sub_doc, "teal_document")
  testthat::expect_length(sub_doc, 2)
  testthat::expect_equal(sub_doc[[1]], "a")
  testthat::expect_equal(sub_doc[[2]], "c")

  empty_sub_doc <- doc[0]
  testthat::expect_s3_class(empty_sub_doc, "teal_document")
  testthat::expect_length(empty_sub_doc, 0)
})

testthat::test_that("edit_teal_document modifies elements", {
  doc <- doc("a", "b", "c")
  edited_doc <- edit_teal_document(doc, modify = c(3, 1))
  testthat::expect_s3_class(edited_doc, "doc")
  testthat::expect_length(edited_doc, 2)
  testthat::expect_equal(edited_doc[[1]], "c")
  testthat::expect_equal(edited_doc[[2]], "a")
})

testthat::test_that("edit_teal_document appends elements", {
  doc <- doc("a", "b")
  edited_doc <- edit_teal_document(doc, append = "c")
  testthat::expect_s3_class(edited_doc, "doc")
  testthat::expect_length(edited_doc, 3)
  testthat::expect_equal(edited_doc[[3]], "c")

  edited_doc_after <- edit_teal_document(doc, append = "c", after = 1)
  testthat::expect_s3_class(edited_doc_after, "doc")
  testthat::expect_length(edited_doc_after, 3)
  testthat::expect_equal(edited_doc_after[[1]], "a")
  testthat::expect_equal(edited_doc_after[[2]], "c")
  testthat::expect_equal(edited_doc_after[[3]], "b")
})

testthat::test_that("edit_teal_document modifies and appends", {
  doc <- doc("a", "b", "c", "d")
  edited_doc <- edit_teal_document(doc, modify = c(4, 1), append = "e", after = 1)
  # After modify: doc becomes ("d", "a")
  # After append: doc becomes ("d", "e", "a")
  testthat::expect_s3_class(edited_doc, "doc")
  testthat::expect_length(edited_doc, 3)
  testthat::expect_equal(edited_doc[[1]], "d")
  testthat::expect_equal(edited_doc[[2]], "e")
  testthat::expect_equal(edited_doc[[3]], "a")
})

testthat::test_that("edit_teal_document preserves attributes", {
  doc <- doc("a")
  attr(doc, "custom_attr") <- "test_value"
  edited_doc <- edit_teal_document(doc, append = "b")
  testthat::expect_equal(attributes(edited_doc)$custom_attr, "test_value")
  testthat::expect_s3_class(edited_doc, "doc")
})


testthat::test_that("code_chunk creates a code_chunk object with params", {
  chunk <- code_chunk("print('hello')", echo = FALSE, eval = TRUE)
  testthat::expect_s3_class(chunk, "code_chunk")
  testthat::expect_equal(as.character(chunk), "print('hello')")
  testthat::expect_equal(attributes(chunk)$params, list(echo = FALSE, eval = TRUE))
})

testthat::test_that("keep_in_report sets the 'keep' attribute", {
  obj1 <- "some text"
  kept_obj1 <- keep_in_report(obj1, TRUE)
  testthat::expect_true(attributes(kept_obj1)$keep)

  obj2 <- list(a = 1)
  not_kept_obj2 <- keep_in_report(obj2, FALSE)
  testthat::expect_false(attributes(not_kept_obj2)$keep)

  # Test default is TRUE
  obj3 <- "another text"
  kept_obj3_default <- keep_in_report(obj3)
  testthat::expect_true(attributes(kept_obj3_default)$keep)
}) 