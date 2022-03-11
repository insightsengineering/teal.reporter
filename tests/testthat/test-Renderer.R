testthat::test_that("TextBlock object can be created", {
  testthat::expect_error(Renderer$new(), regexp = NA)
})

testthat::test_that("new returns an object of type Renderer", {
  testthat::expect_true(inherits(Renderer$new(), "Renderer"))
})

text_block1 <- TextBlock$new()$set_content("text")$set_style("header2")
text_block2 <- TextBlock$new()$set_content("text")
png_path <- system.file("img", "Rlogo.png", package = "png")
picture_block <- PictureBlock$new()$set_content(ggplot2::ggplot(iris))
newpage_block <- NewpageBlock$new()
blocks <- list(text_block1, text_block2, picture_block, newpage_block)

testthat::test_that("renderRmd asserts the argument is a list of TextBlocks/PictureBlock/NewpageBlock/TableBlock", {
  renderer <- Renderer$new()
  testthat::expect_error(
    renderer$renderRmd(append(blocks, "STH")),
    regexp = "May only contain the following types: \\{TextBlock,PictureBlock,NewpageBlock,TableBlock\\}"
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
