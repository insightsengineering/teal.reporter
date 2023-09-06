
# Archiver.R ----

## Archiver ----

archiver <- teal.reporter:::Archiver$new()

## FileArchiver ----

archiver <- teal.reporter:::FileArchiver$new()
archiver$get_output_dir()

## JSONArchiver ----

card1 <- teal.reporter::ReportCard$new()

card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
 ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
)

reporter <- teal.reporter::Reporter$new()
reporter$append_cards(list(card1))

archiver <- teal.reporter:::JSONArchiver$new()
archiver$write(reporter)
archiver$get_output_dir()

archiver$read()$get_cards()[[1]]$get_content()
blocks <- teal.reporter::Reporter$new()$from_reporter(archiver$read())$get_blocks()
doc <- teal.reporter:::Renderer$new()$render(blocks)

# ContentBlock.R ----

block <- teal.reporter:::ContentBlock$new()
block$set_content("Base64 encoded picture")
block$get_content()

# FileBlock.R ----

block <- teal.reporter:::FileBlock$new()
file_path <- tempfile(fileext = ".png")
saveRDS(iris, file_path)
block$from_list(list(basename = basename(file_path)), dirname(file_path))

block <- teal.reporter:::FileBlock$new()
block$to_list(tempdir())

# NewpageBlock.R ----

block <- teal.reporter:::NewpageBlock$new()

# PictureBlock.R ----

block <- teal.reporter:::PictureBlock$new()
block$set_content(ggplot2::ggplot(iris))

block <- teal.reporter:::PictureBlock$new()
block$set_content(lattice::bwplot(1))

block <- teal.reporter:::PictureBlock$new()
block$set_content(ggplot2::ggplotGrob(ggplot2::ggplot(iris)))

block <- teal.reporter:::PictureBlock$new()
block$set_title("Title")
block$get_title()

block <- teal.reporter:::PictureBlock$new()
block$set_dim(c(800, 600))
block$get_dim()

# RcodeBlock.R ----

block <- teal.reporter:::RcodeBlock$new()
block$set_params(list(echo = TRUE))
block$get_params()
block$get_available_params()
block$from_list(list(text = "sth", params = list()))
block$to_list()

# Renderer.R ----

renderer <- teal.reporter:::Renderer$new()
renderer$get_output_dir()

## renderRmd ----
card1 <- teal.reporter::ReportCard$new()

card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text")
card1$append_plot(
 ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
)

card2 <- teal.reporter::ReportCard$new()

card2$append_text("Header 2 text", "header2")
card2$append_text("A paragraph of default text", "header2")
lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
table_res2 <- rtables::build_table(lyt, airquality)
card2$append_table(table_res2)
card2$append_table(iris)
card2$append_rcode("2+2", echo = FALSE)

reporter <- teal.reporter::Reporter$new()
reporter$append_cards(list(card1, card2))

yaml_l <- list(
  author = teal.reporter:::yaml_quoted("NEST"),
  title = teal.reporter:::yaml_quoted("Report"),
  date = teal.reporter:::yaml_quoted("07/04/2019"),
  output = list(html_document = list(toc = FALSE))
)

yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
result_path <- teal.reporter:::Renderer$new()$renderRmd(reporter$get_blocks(), yaml_header)

## render ----

card1 <- teal.reporter::ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text")
card1$append_plot(
 ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
)

card2 <- teal.reporter::ReportCard$new()
card2$append_text("Header 2 text", "header2")
card2$append_text("A paragraph of default text", "header2")
lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
table_res2 <- rtables::build_table(lyt, airquality)
card2$append_table(table_res2)
card2$append_table(iris)
card2$append_rcode("2+2", echo = FALSE)

reporter <- teal.reporter::Reporter$new()
reporter$append_cards(list(card1, card2))

yaml_l <- list(
  author = teal.reporter:::yaml_quoted("NEST"),
  title = teal.reporter:::yaml_quoted("Report"),
  date = teal.reporter:::yaml_quoted("07/04/2019"),
  output = list(html_document = list(toc = FALSE))
)

yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
result_path <- teal.reporter:::Renderer$new()$render(reporter$get_blocks(), yaml_header)

# TableBlock.R ----

block <- teal.reporter:::TableBlock$new()
block$set_content(iris)

# TextBlock.R ----

block <- teal.reporter:::TextBlock$new()
block$set_style("header2")
block$get_style()
block$get_available_styles()
block$from_list(list(text = "sth", style = "default"))
block$to_list()

# yaml_utils.R ----

## yaml_quoted ----

yaml <- list(
  author = teal.reporter:::yaml_quoted("NEST"),
  title = teal.reporter:::yaml_quoted("Report"),
  date = teal.reporter:::yaml_quoted("07/04/2019"),
  output = list(pdf_document = list(keep_tex = TRUE))
)
yaml::as.yaml(yaml)

## md_header ----

yaml <- list(
  author = teal.reporter:::yaml_quoted("NEST"),
  title = teal.reporter:::yaml_quoted("Report"),
  date = teal.reporter:::yaml_quoted("07/04/2019"),
  output = list(pdf_document = list(keep_tex = TRUE))
)
teal.reporter:::md_header(yaml::as.yaml(yaml))

## conv_str_logi ----

teal.reporter:::conv_str_logi("TRUE")
teal.reporter:::conv_str_logi("True")

teal.reporter:::conv_str_logi("off")
teal.reporter:::conv_str_logi("n")

teal.reporter:::conv_str_logi("sth")
