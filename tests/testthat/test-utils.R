testthat::test_that("get_bs_version", {
  testthat::expect_identical(get_bs_version(), "3")
})

testthat::test_that("panel_item", {
  testthat::expect_s3_class(panel_item("LABEL", shiny::tags$div()), "shiny.tag")
})

testthat::test_that("to_flextable: supported class", {
  data_frame <- data.frame(A = 1:3, B = 4:6)
  flextable_output <- to_flextable(data_frame)
  testthat::expect_s3_class(flextable_output, "flextable")
})

testthat::test_that("to_flextable: unsupported class", {
  unsupported_data <- list(a = 1, b = 2)
  expect_error(to_flextable(unsupported_data), "Unsupported class")
})

test_that("custom_theme to flextable", {
  sample_ft <- flextable::qflextable(head(mtcars))
  themed_ft <- custom_theme(sample_ft)
  expect_is(themed_ft, "flextable")
})

test_that("get_merge_index_single", {
  sample_span <- c(1, 2, 1, 3)
  merge_index <- get_merge_index_single(sample_span)
  expect_is(merge_index, "list")
})

test_that("get_merge_index", {
  sample_spans <- matrix(c(1, 2, 1, 3, 2, 1, 1, 1), ncol = 2)
  merge_index <- get_merge_index(sample_spans)
  expect_is(merge_index, "list")
})

test_that("merge_at_indice", {
  sample_ft <- flextable::qflextable(head(mtcars))
  merge_indices <- list(
    list(i = 1, j = 1:2),
    list(i = 2, j = 3:4)
  )
  merged_ft <- merge_at_indice(sample_ft, lst = merge_indices, part = "body")
  expect_is(merged_ft, "flextable")
})

test_that("padding_lst applies padding to a flextable based on indentation levels", {
  sample_ft <- flextable::qflextable(head(mtcars))
  sample_indents <- c(1, 2, 1, 3, 2)
  padded_ft <- padding_lst(sample_ft, sample_indents)
  expect_is(padded_ft, "flextable")
})

block_text <- "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6"

test_that("split_text_block - splits text block into equal parts", {
  n <- 2
  result <- split_text_block(block_text, n)
  expected_result <- as.array(c("Line 1\nLine 2", "Line 3\nLine 4", "Line 5\nLine 6"))
  expect_equal(result, expected_result)
})

test_that("split_text_block - n greater than the number of line breaks", {
  n <- 7
  result <- split_text_block(block_text, n)
  expect_equal(result, block_text)
})
