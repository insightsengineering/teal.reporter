testthat::test_that("panel_item", {
  testthat::expect_s3_class(panel_item("LABEL", shiny::tags$div()), "shiny.tag")
})

testthat::test_that("to_flextable: supported class `data.frame`", {
  data_frame <- data.frame(A = 1:3, B = 4:6)
  flextable_output <- to_flextable(data_frame)
  testthat::expect_s3_class(flextable_output, "flextable")
})

testthat::test_that("to_flextable: supported class `rtables`", {
  tbl <- rtables::basic_table() %>%
    rtables::analyze("AGE", afun = mean) %>%
    rtables::build_table(formatters::DM)
  flextable_output <- to_flextable(tbl)
  testthat::expect_s3_class(flextable_output, "flextable")
})

testthat::test_that("to_flextable: supported class `listing_df`", {
  lsting <- rlistings::as_listing(formatters::ex_adae[1:50, ])
  flextable_output <- to_flextable(lsting)
  testthat::expect_s3_class(flextable_output, "flextable")
})

testthat::test_that("to_flextable: unsupported class", {
  unsupported_data <- list(a = 1, b = 2)
  testthat::expect_error(to_flextable(unsupported_data), "Unsupported class")
})

testthat::test_that("split_text_block - splits text block into blocks no longer than n lines", {
  l <- 5
  block_text <- paste(paste(rep("Line", l), seq_len(l)), collapse = "\n")
  n <- 2
  result <- split_text_block(block_text, n)
  result_lines <- lapply(result, function(x) strsplit(x, "\n")[[1]])
  lapply(result_lines, function(x) testthat::expect_lte(length(x), n))

  n <- 5
  result <- split_text_block(block_text, n)
  testthat::expect_equal(result, list(block_text))
})

testthat::test_that("is_table_too_wide: detects when flextable exceeds portrait width", {
  # Create a simple data frame
  data_frame <- data.frame(A = 1:3, B = 4:6)
  ft <- to_flextable(data_frame)
  
  # Test with a very small portrait width - should be too wide
  testthat::expect_true(is_table_too_wide(ft, portrait_width = 0.1))
  
  # Test with a very large portrait width - should not be too wide
  testthat::expect_false(is_table_too_wide(ft, portrait_width = 20))
  
  # Test with default width
  result <- is_table_too_wide(ft)
  testthat::expect_type(result, "logical")
})

testthat::test_that("is_table_too_wide: returns FALSE for non-flextable objects", {
  testthat::expect_false(is_table_too_wide("not a table"))
  testthat::expect_false(is_table_too_wide(data.frame(a = 1)))
  testthat::expect_false(is_table_too_wide(list(a = 1)))
})
