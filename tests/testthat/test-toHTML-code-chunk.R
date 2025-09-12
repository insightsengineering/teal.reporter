testthat::test_that(".toHTML.code_chunk returns collapsible panel when include_rcode is TRUE", {
  code_chunk_obj <- code_chunk("x <- 1:10\nprint(x)")
  result <- .toHTML.code_chunk(code_chunk_obj, include_rcode = TRUE)
  
  # Should return a panel structure with collapsible functionality
  testthat::expect_s3_class(result, "shiny.tag")
  
  # Should contain the code content
  result_html <- as.character(result)
  testthat::expect_true(grepl("x <- 1:10", result_html, fixed = TRUE))
  testthat::expect_true(grepl("print\\(x\\)", result_html))
  
  # Should have collapsible structure (check for Bootstrap classes)
  testthat::expect_true(grepl("collapse", result_html))
})

testthat::test_that(".toHTML.code_chunk returns empty div when include_rcode is FALSE", {
  code_chunk_obj <- code_chunk("x <- 1:10\nprint(x)")
  result <- .toHTML.code_chunk(code_chunk_obj, include_rcode = FALSE)
  
  # Should return an empty div
  testthat::expect_s3_class(result, "shiny.tag")
  testthat::expect_equal(result$name, "div")
  testthat::expect_null(result$children)
})

testthat::test_that(".toHTML.code_chunk defaults to include_rcode = TRUE", {
  code_chunk_obj <- code_chunk("x <- 1:10")
  result <- .toHTML.code_chunk(code_chunk_obj)
  
  # Should behave as if include_rcode = TRUE (collapsible panel)
  result_html <- as.character(result)
  testthat::expect_true(grepl("x <- 1:10", result_html, fixed = TRUE))
  testthat::expect_true(grepl("collapse", result_html))
})

testthat::test_that(".toHTML.code_chunk preserves language attribute", {
  code_chunk_obj <- code_chunk("console.log('test')", lang = "javascript")
  result <- .toHTML.code_chunk(code_chunk_obj, include_rcode = TRUE)
  
  result_html <- as.character(result)
  testthat::expect_true(grepl("language-javascript", result_html))
})

testthat::test_that(".toHTML.code_chunk creates collapsible panel with correct title", {
  code_chunk_obj <- code_chunk("test code")
  result <- .toHTML.code_chunk(code_chunk_obj, include_rcode = TRUE)
  
  result_html <- as.character(result)
  # Should contain "R Code" in the title
  testthat::expect_true(grepl("R Code", result_html, fixed = TRUE))
  # Should contain code icon
  testthat::expect_true(grepl("fa-code", result_html) || grepl("icon", result_html))
})