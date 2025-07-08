# @param card is the title of the card. Used instead of "title" for compatibility
# with add_card_button_srv
test_card1 <- function(card = NULL) {
  testthat::skip_if_not_installed("ggplot2")
  plot <- ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram(binwidth = 0.2)
  new_card <- teal_card("## Header 2 text", "A paragraph of default text", plot)
  metadata(new_card, "title") <- card
  new_card
}

# @param card is the title of the card. Used instead of "title" for compatibility
# with add_card_button_srv
test_card2 <- local({
  fun <- function(card = NULL) {
    lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    table_res2 <- rtables::build_table(lyt, within(airquality, Day <- factor(Day))) # nolint: object_name.
    new_card <- teal_card("## Header 2 text", "A paragraph of default text", table_res2, iris)
    metadata(new_card, "title") <- card
    new_card
  }
  cache <- list()
  function(card = NULL) {
    title_ix <- card %||% "no_title" # mock index for no title
    if (is.null(cache[[title_ix]])) cache[[title_ix]] <<- fun(card)
    cache[[title_ix]]
  }
})

test_card1.ReportCard <- function(card = NULL) { # nolint: object_name.
  template <- test_card1(card)
  new_card <- ReportCard$new()

  metadata(new_card, "title") <- metadata(template, "title")
  new_card$append_text(sub("^# ", "", template[[1]]), "header2")
  new_card$append_text(template[[2]])
  new_card$append_plot(template[[3]])
  new_card
}

test_card2.ReportCard <- function(card = NULL) { # nolint: object_name.
  template <- test_card2(card)
  new_card <- ReportCard$new()

  metadata(new_card, "title") <- metadata(template, "title")
  new_card$append_text(sub("^# ", "", template[[1]]), "header2")
  new_card$append_text(template[[2]])
  new_card$append_table(template[[3]])
  new_card$append_table(template[[4]])
  new_card
}

test_reporter.ReportCard <- function(card1 = test_card1.ReportCard(), # nolint: object_name.
                                     card2 = test_card2.ReportCard(), ...) { # nolint: object_name.
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
