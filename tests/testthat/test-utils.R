testthat::test_that("panel_item", {
  testthat::expect_s3_class(panel_item("LABEL", shiny::tags$div()), "shiny.tag")
})

testthat::test_that("panel_item generates Bootstrap 5 compatible HTML", {
  result <- panel_item("Test Title", shiny::tags$p("Test content"))
  html_output <- as.character(result)
  
  # Check for Bootstrap 5 specific attributes
  testthat::expect_true(grepl('data-bs-toggle="collapse"', html_output))
  testthat::expect_true(grepl('data-bs-target="#', html_output))
  testthat::expect_true(grepl('aria-controls=', html_output))
  testthat::expect_true(grepl('aria-expanded=', html_output))
  
  # Check for proper button element instead of div
  testthat::expect_true(grepl('<button', html_output))
  testthat::expect_true(grepl('type="button"', html_output))
  
  # Check for collapse class
  testthat::expect_true(grepl('class="collapse"', html_output))
})

testthat::test_that("panel_item collapsed state works correctly", {
  # Test collapsed = TRUE (default)
  result_collapsed <- panel_item("Test", shiny::tags$p("content"))
  html_collapsed <- as.character(result_collapsed)
  testthat::expect_true(grepl('aria-expanded="false"', html_collapsed))
  testthat::expect_true(grepl('class="collapse"', html_collapsed))
  testthat::expect_false(grepl('class="collapse show"', html_collapsed))
  
  # Test collapsed = FALSE
  result_expanded <- panel_item("Test", shiny::tags$p("content"), collapsed = FALSE)
  html_expanded <- as.character(result_expanded)
  testthat::expect_true(grepl('aria-expanded="true"', html_expanded))
  testthat::expect_true(grepl('class="collapse show"', html_expanded))
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
