test_card1.ReportCard <- function() { # nolint: object_name.
  testthat::skip_if_not_installed("ggplot2")
  card <- ReportCard$new()

  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length, y = Sepal.Length)) +
      ggplot2::geom_point()
  )
  card
}

test_card2.ReportCard <- local({ # nolint: object_name.
  fun <- function() {
    card <- ReportCard$new()

    card$append_text("Header 2 text", "header2")
    card$append_text("A paragraph of default text", "header2")
    lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    table_res2 <- rtables::build_table(lyt, within(airquality, Day <- factor(Day))) # nolint: object_name.
    card$append_table(table_res2)
    card$append_table(iris)
  }
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- fun()
    cache$clone()
  }
})

test_card1 <- function() {
  withr::with_environment(emptyenv(), plot <- ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram(binwidth = 0.2))
  report_document("## Header 2 text", "A paragraph of default text", plot)
}

test_card2 <- local({
  fun <- function() {
    lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    table_res2 <- rtables::build_table(lyt, within(airquality, Day <- factor(Day))) # nolint: object_name.
    report_document("## Header 2 text", "A paragraph of default text", table_res2, iris)
  }
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- fun()
    cache
  }
})

test_reporter.ReportCard <- function(card1 = test_card1.ReportCard(), card2 = test_card2.ReportCard(), ...) { # nolint: object_name, line_length.
  new_cards <- append(list(card1, card2), list(...))
  reporter <- Reporter$new()
  reporter$append_cards(new_cards)
  reporter
}

test_reporter <- function(card1 = test_card1(), card2 = test_card2(), ...) {
  new_cards <- append(list(card1, card2), list(...))
  reporter <- Reporter$new()
  reporter$append_cards(new_cards)
  reporter
}
