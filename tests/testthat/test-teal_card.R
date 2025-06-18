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

  it("new `teal_card` and keeps original title", {
    metadata(doc_base, "title") <- "A Title"
    doc_result <- c(doc_base, teal_card("new content"))
    testthat::expect_equal(metadata(doc_result, "title"), "A Title")
  })

  it("new `teal_card` and adds new metadata", {
    metadata(doc_base, "title") <- "A Title"
    second_doc <- teal_card("new content")
    metadata(second_doc, "prop") <- "A property of metadata"
    doc_result <- c(doc_base, second_doc)
    testthat::expect_equal(metadata(doc_result, "title"), "A Title")
    testthat::expect_equal(metadata(doc_result, "prop"), "A property of metadata")
  })

  it("new `teal_card` and merges metadata", {
    metadata(doc_base, "title") <- "A Title"
    metadata(doc_base, "prop_base") <- "An unchanged property"
    second_doc <- teal_card("new content")
    metadata(second_doc, "prop") <- "A property of metadata"
    metadata(second_doc, "title") <- "A modified title"
    doc_result <- c(doc_base, second_doc)
    testthat::expect_equal(metadata(doc_result, "title"), "A modified title")
    testthat::expect_equal(metadata(doc_result, "prop"), "A property of metadata")
    testthat::expect_equal(metadata(doc_result, "prop_base"), "An unchanged property")
  })
})

testthat::it("[.card subsets and", {
  it("retains class", {
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
})

testthat::describe("as.teal_card" , {
  it("converts a simple list with each element being converted to a report content", {
    simple_list <- list("a", "b", "c")
    doc <- as.teal_card(simple_list)
    testthat::expect_s3_class(doc, "teal_card")
    testthat::expect_length(doc, 3)
    testthat::expect_equal(doc[[1]], "a")
    testthat::expect_equal(doc[[2]], "b")
    testthat::expect_equal(doc[[3]], "c")
  })

  it("converts a custom list with many elements with only 1 element being created as report content", {
    custom_list <- list("a", "b", "c", "d")
    class(custom_list) <- c(custom_list, "extra class")
    doc <- as.teal_card(custom_list)
    testthat::expect_s3_class(doc, "teal_card")
    testthat::expect_length(doc, 1)
    testthat::expect_equal(doc[[1]], custom_list)
  })

  it("converts a ggplot2 to a teal_card with only 1 report content", {
    testthat::skip_if_not_installed("ggplot2")
    plot <- ggplot2::ggplot(iris) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
    doc <- as.teal_card(plot)
    testthat::expect_s3_class(doc, "teal_card")
    testthat::expect_length(doc, 1)
    testthat::expect_s3_class(doc[[1]], "ggplot")
  })
})

testthat::describe("metadata", {
  it("can be assigned individually to `teal_card` object", {
    doc <- teal_card("a", "b")
    metadata(doc, "title") <- "A Title"
    testthat::expect_equal(metadata(doc, "title"), "A Title")
  })

  it("can be assigned as named list to `teal_card` object", {
    doc <- teal_card("a", "b")
    metadata(doc) <- list(title = "A Title")
    testthat::expect_equal(metadata(doc, "title"), "A Title")
  })

  it("can be assigned individually to `ReportCard` object", {
    doc <- ReportCard$new()
    metadata(doc, "title") <- "A Title"
    testthat::expect_equal(metadata(doc, "title"), "A Title")
    testthat::expect_equal(doc$get_name(), "A Title")
  })

  it("can be assigned as named list to `ReportCard` object if only has title", {
    doc <- ReportCard$new()
    metadata(doc) <- list(title = "A Title")
    testthat::expect_equal(metadata(doc, "title"), "A Title")
    testthat::expect_equal(doc$get_name(), "A Title")
  })

  it("assignment throws warning when named list has other elements than title", {
    doc <- ReportCard$new()
    testthat::expect_warning(
      fixed = TRUE,
      metadata(doc) <- list(title = "A Title", prop = "A property"),
      "ReportCard class only supports `title` in metadata"
    )
    testthat::expect_equal(metadata(doc, "title"), "A Title")
    testthat::expect_equal(doc$get_name(), "A Title")
  })

  it("assignment throws warning when named list has element, but not title", {
    doc <- ReportCard$new()
    testthat::expect_warning(
      fixed = TRUE,
      metadata(doc) <- list(prop = "A property"),
      "ReportCard class only supports `title` in metadata"
    )
    testthat::expect_equal(metadata(doc), list(title = character(0L)))
  })

  it("only supports assigning `title` in `ReportCard` object", {
    doc <- ReportCard$new()
    testthat::expect_warning(
      fixed = TRUE,
      metadata(doc, "prop") <- "A Property",
      "ReportCard class only supports `title` in metadata"
    )
  })
})
