testthat::test_that("Renderer object can be created", {
  testthat::expect_no_error(Renderer$new())
})

testthat::test_that("new returns an object of type Renderer", {
  testthat::expect_true(inherits(Renderer$new(), "Renderer"))
})

testthat::skip_if_not_installed("ggplot2")

text_block1 <- TextBlock$new()$set_content("text")$set_style("header2")
text_block2 <- TextBlock$new()$set_content("text")
png_path <- system.file("img", "Rlogo.png", package = "png")
picture_block <- PictureBlock$new()$set_content(ggplot2::ggplot(iris))
html_block <- HTMLBlock$new(shiny::tags$div("test"))
# https://github.com/davidgohel/flextable/issues/600
withr::with_options(
  opts_partial_match_old,
  table_block <- TableBlock$new()$set_content(iris)
)
newpage_block <- NewpageBlock$new()
blocks <- list(text_block1, text_block2, picture_block, table_block, newpage_block, html_block)

testthat::test_that("renderRmd asserts the argument is a list of TextBlocks/PictureBlock/NewpageBlock/TableBlock", {
  renderer <- Renderer$new()
  testthat::expect_error(
    renderer$renderRmd(append(blocks, "STH")),
    regexp = "May only contain the following types: \\{TextBlock,PictureBlock,NewpageBlock,TableBlock,RcodeBlock,HTMLBlock\\}" # nolint line_length
  )
})

testthat::test_that("render returns the same path as get_last_output_file", {
  renderer <- Renderer$new()
  testthat::expect_true(basename(renderer$render(blocks)) %in% list.files(renderer$get_output_dir()))
})

testthat::test_that("render returns the same path as get_last_output_file", {
  renderer <- Renderer$new()
  testthat::expect_true(basename(renderer$renderRmd(blocks)) %in% list.files(renderer$get_output_dir()))
})

testthat::test_that("renderRmd includes landscape support for wide tables in PDF", {
  renderer <- Renderer$new()
  
  # Create a table block (using existing table_block from above)
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old, {
      # Create a wide data frame that should trigger landscape mode
      wide_data <- data.frame(
        matrix(rep("Long content that makes table wide", 40), nrow = 4, ncol = 10)
      )
      colnames(wide_data) <- paste0("VeryLongColumnName", 1:10)
      
      wide_table_block <- TableBlock$new()$set_content(wide_data)
      test_blocks <- list(text_block1, wide_table_block)
      
      # Test with PDF output
      pdf_yaml <- "---\ntitle: 'Test'\noutput: pdf_document\n---\n"
      
      testthat::expect_no_error({
        rmd_path <- renderer$renderRmd(test_blocks, pdf_yaml)
      })
      
      # Check that the generated file exists
      testthat::expect_true(file.exists(rmd_path))
      
      # Read the content and check for landscape features
      if (file.exists(rmd_path)) {
        content <- readLines(rmd_path)
        content_text <- paste(content, collapse = "\n")
        
        # If the table was detected as wide, check for landscape LaTeX
        if (wide_table_block$get_landscape_mode()) {
          testthat::expect_true(
            grepl("pdflscape", content_text),
            "PDF with wide tables should include pdflscape package"
          )
        }
      }
    }
  )
})

testthat::test_that("renderRmd does not add landscape support for non-PDF outputs", {
  renderer <- Renderer$new()
  
  # Test with HTML output
  html_yaml <- "---\ntitle: 'Test'\noutput: html_document\n---\n"
  
  testthat::expect_no_error({
    rmd_path <- renderer$renderRmd(blocks, html_yaml)
  })
  
  # HTML output should not include LaTeX landscape commands
  if (file.exists(rmd_path)) {
    content <- readLines(rmd_path)
    content_text <- paste(content, collapse = "\n")
    
    testthat::expect_false(
      grepl("\\\\begin\\{landscape\\}", content_text),
      "HTML output should not include LaTeX landscape commands"
    )
  }
})
