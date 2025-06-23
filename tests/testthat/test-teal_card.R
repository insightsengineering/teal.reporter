testthat::describe("teal_card contructor creates", {
  testthat::it("empty teal_card", {
    doc <- teal_card()
    testthat::expect_identical(doc, structure(list(), class = "teal_card"))
  })

  testthat::it("teal_card appends arguments and sets them random unique names", {
    doc <- teal_card("a", "b", "c", "d", "e", "f", "g", "h")
    testthat::expect_true(all(!duplicated(names(doc))))
  })

  testthat::it("teal_card doesn't ignore NULL", {
    doc <- unname(teal_card(NULL))
    testthat::expect_identical(doc, structure(list(NULL), class = "teal_card"))
  })

  testthat::it("teal_card keeps conditions", {
    doc <- unname(teal_card(simpleCondition("test")))
    testthat::expect_identical(doc, structure(list(simpleCondition("test")), class = "teal_card"))
  })

  testthat::it("teal_card appends each element asis (no list unwrapping)", {
    doc <- unname(teal_card("a", list(1, list(2)), code_chunk("print('hi')")))
    testthat::expect_identical(
      doc,
      structure(
        list("a", list(1, list(2)), code_chunk("print('hi')")),
        class = "teal_card"
      )
    )
  })
})

testthat::describe("c.teal_card combines", {
  it("two empty teal_card(s)", {
    testthat::expect_identical(c(teal_card(), teal_card()), teal_card())
  })

  it("empty teal_card with non-empty", {
    doc2 <- teal_card(TRUE)
    testthat::expect_identical(c(teal_card(), doc2), doc2)
  })

  it("with empty teal_card - remains the same", {
    doc <- teal_card("a", "b")
    testthat::expect_identical(c(doc, teal_card()), doc)
  })

  it("with character - adds as a new element", {
    doc_result <- c(teal_card("a", "b"), "c")
    testthat::expect_equal(doc_result, teal_card("a", "b", "c"), ignore_attr = TRUE)
  })

  it("with list - adds each list element separately (unwraps list)", {
    doc_result <- c(teal_card("a", "b"), list(1, 2))
    testthat::expect_equal(doc_result, teal_card("a", "b", 1, 2), ignore_attr = TRUE)
  })

  it("with teal_card containing a list - append this list asis (doesn't unwrap list)", {
    doc_result <- c(teal_card("a", "b"), teal_card(list(1, 2)))
    testthat::expect_equal(doc_result, teal_card("a", "b", list(1, 2)), ignore_attr = TRUE)
  })

  it("with NULL - remains the same (ignores NULL)", {
    doc_result <- c(teal_card("a", "b"), NULL)
    testthat::expect_equal(doc_result, teal_card("a", "b"), ignore_attr = TRUE)
  })

  it("with character(0) - adds as a new element", {
    doc_result <- c(teal_card("a", "b"), character(0))
    testthat::expect_equal(doc_result, teal_card("a", "b", character(0)), ignore_attr = TRUE)
  })

  it("with ggplot - adds as a new element", {
    plot <- ggplot2::ggplot(iris)
    doc_result <- c(teal_card("a", "b"), plot)
    testthat::expect_equal(doc_result, teal_card("a", "b", plot), ignore_attr = TRUE)
  })

  it("with new teal_card - adds new elements asis", {
    doc_result <- c(teal_card("a", "b"), teal_card("c", "d"))
    testthat::expect_equal(doc_result, teal_card("a", "b", "c", "d"), ignore_attr = TRUE)
  })

  it("with new teal_card containing ggplot - adds new elements asis", {
    plot <- ggplot2::ggplot(iris) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
    doc_result <- c(teal_card("a", "b"), teal_card("# Plot", plot))
    testthat::expect_equal(doc_result, teal_card("a", "b", "# Plot", plot), ignore_attr = TRUE)
  })

  it("with teal_card containing new and old items - adds only new", {
    doc1 <- teal_card("a", "b")
    doc2 <- c(doc1, "c", "d")
    testthat::expect_equal(c(doc1, doc2), teal_card("a", "b", "c", "d"), ignore_attr = TRUE)
  })

  it("with teal_card containing new and old items - adds even if their order is different", {
    doc1 <- teal_card("a", "b")
    doc2 <- c(doc1, "c", "d")
    doc2 <- doc2[c(3, 1, 4, 2)]
    testthat::expect_equal(c(doc1, doc2), teal_card("c", "a", "d", "b"), ignore_attr = TRUE)
  })

  it("with teal_card with new and missing old items - restores original items, adds new at the end and warn", {
    doc1 <- teal_card("a", "b")
    doc2 <- c(doc1, "c", "d")[c(4, 3, 2)]
    testthat::expect_warning(
      testthat::expect_equal(c(doc1, doc2), teal_card("a", "b", "d", "c"), ignore_attr = TRUE)
    )
  })

  it("with a `teal_card` - keeps original metadata", {
    doc <- teal_card("a", "b")
    metadata(doc) <- list(title = "A Title", a = "test")
    doc_result <- c(doc, teal_card("new content"))
    testthat::expect_identical(metadata(doc_result), list(title = "A Title", a = "test"))
  })

  it("new `teal_card` - combines metadata and overwrites original", {
    doc1 <- teal_card("a", "b")
    metadata(doc1) <- list(title = "A Title", a = "test")
    doc2 <- teal_card("new content")
    metadata(doc2) <- list(title = "A New Title", b = "test2")
    doc_result <- c(doc1, doc2)
    testthat::expect_identical(metadata(doc_result), list(title = "A New Title", a = "test", b = "test2"))
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

testthat::describe("as.teal_card", {
  it("converts a simple list with each element being converted to a report content", {
    simple_list <- list("a", "b", "c")
    doc <- as.teal_card(simple_list)
    testthat::expect_equal(doc, teal_card("a", "b", "c"), ignore_attr = TRUE)
  })

  it("converts a custom list class with many elements into single-element-teal_card", {
    custom_list <- list("a", "b", "c", "d")
    class(custom_list) <- "extra class"
    doc <- as.teal_card(custom_list)
    testthat::expect_equal(doc, teal_card(custom_list), ignore_attr = TRUE)
  })

  it("converts a ggplot2 to a teal_card with only 1 report content", {
    testthat::skip_if_not_installed("ggplot2")
    plot <- ggplot2::ggplot(iris) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
    doc <- as.teal_card(plot)
    testthat::expect_equal(doc, teal_card(plot), ignore_attr = TRUE)
  })
})

testthat::describe("metadata", {
  it("can be assigned individually to `teal_card` object using 'which' argument", {
    doc <- teal_card("a", "b")
    metadata(doc, "title") <- "A Title"
    testthat::expect_equal(metadata(doc, "title"), "A Title")
  })

  it("can be assigned individually to `teal_card` using `$`", {
    doc <- teal_card("a", "b")
    metadata(doc)$title <- "A Title"
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
