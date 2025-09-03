testthat::test_that("Reporter object can be created", {
  testthat::expect_no_error(Reporter$new())
})

testthat::test_that("new returns an object of type Reporter", {
  testthat::expect_true(inherits(Reporter$new(), "Reporter"))
})

testthat::test_that("default reporter id", {
  testthat::expect_identical(Reporter$new()$get_id(), "")
})

testthat::test_that("set_id sets the reporter id and returns reporter", {
  reporter <- test_reporter()
  testthat::expect_s3_class(reporter$set_id("xyz"), "Reporter")
  testthat::expect_identical(reporter$set_id("xyz")$get_id(), "xyz")
})

testthat::describe("Reporter with ReportCard", {
  card1 <- test_card1.ReportCard()
  card2 <- test_card2.ReportCard()
  reporter <- test_reporter.ReportCard(card1, card2)
  it("get_cards returns the same cards which was added to reporter", {
    testthat::expect_equal(
      reporter$get_cards(),
      list(card1$get_content(), card2$get_content()),
      ignore_attr = "names"
    )
  })

  it("get_blocks returns the same blocks which was added to reporter, sep = NULL", {
    testthat::expect_equal(
      reporter$get_blocks(sep = NULL),
      c(
        teal_card("# _Unnamed Card (1)_"),
        card1$get_content(),
        "# _Unnamed Card (2)_",
        card2$get_content()
      ),
      ignore_attr = "names"
    )
  })

  it("get_blocks by default adds 'newpage' between cards", {
    reporter <- test_reporter.ReportCard(card1 <- test_card1.ReportCard(), card2 <- test_card2.ReportCard())
    reporter_blocks <- reporter$get_blocks()
    reporter_blocks2 <- c(teal_card("# _Unnamed Card (1)_"), reporter$get_cards()[[1]], "\\newpage")
    reporter_blocks2 <- c(reporter_blocks2, "# _Unnamed Card (2)_", reporter$get_cards()[[2]])
    testthat::expect_equal(
      reporter$get_blocks(),
      reporter_blocks2,
      ignore_attr = "names"
    )
  })

  it("get_blocks returns the same blocks which was added to reporter, sep = NULL", {
    reporter <- test_reporter.ReportCard(card1 <- test_card1.ReportCard(), card2 <- test_card2.ReportCard())
    testthat::expect_equal(
      unname(reporter$get_blocks(sep = NULL)),
      unname(c(teal_card("# _Unnamed Card (1)_"), card1$get_content(), "# _Unnamed Card (2)_", card2$get_content()))
    )
  })
})

testthat::test_that("get_cards returns the same cards which was added to reporter", {
  reporter <- test_reporter(card1 <- test_card1(), card2 <- test_card2())
  testthat::expect_equal(unname(reporter$get_cards()), list(card1, card2))
})

testthat::test_that("get_blocks returns the same blocks which was added to reporter, sep = NULL", {
  reporter <- test_reporter(card1 <- test_card1("A title"), card2 <- test_card2("Another title"))
  testthat::expect_equal(
    reporter$get_blocks(sep = NULL),
    as.teal_card(
      append(
        c(sprintf("# %s", metadata(card1, "title")), card1),
        c(sprintf("# %s", metadata(card2, "title")), card2)
      )
    ),
    ignore_attr = "names"
  )
})

testthat::test_that("get_blocks by default adds 'newpage' between cards", {
  card1 <- test_card1("A title")
  card2 <- test_card2("Another title")
  reporter <- test_reporter(card1, card2)

  reporter_1 <- Reporter$new()$append_cards(card1)
  reporter_2 <- Reporter$new()$append_cards(card2)

  reporter_blocks <- reporter$get_blocks()
  reporter_blocks2 <- append(reporter_1$get_blocks(), "\\newpage")
  reporter_blocks2 <- append(reporter_blocks2, reporter_2$get_blocks())
  testthat::expect_equal(reporter$get_blocks(), reporter_blocks2, ignore_attr = "names")
})

testthat::test_that("get_blocks and get_cards return empty teal_card by default", {
  reporter <- Reporter$new()
  testthat::expect_identical(reporter$get_blocks(), teal_card())
  testthat::expect_identical(reporter$get_cards(), structure(list(), names = character(0L)))
})

testthat::test_that("The deep copy constructor copies the content files to new files", {
  testthat::skip_if_not_installed("ggplot2")
  card <- teal_card(ggplot2::ggplot(iris))
  reporter <- Reporter$new()$append_cards(card)
  reporter_copy <- reporter$clone(deep = TRUE)
  original_content_file <- reporter$get_blocks()
  copied_content_file <- reporter_copy$get_blocks()

  testthat::expect_failure(
    testthat::expect_equal(rlang::obj_address(original_content_file), rlang::obj_address(copied_content_file))
  )
  testthat::expect_equal(original_content_file, copied_content_file, ignore_attr = "names")
})

testthat::test_that("append_metadata accept only named list", {
  reporter <- Reporter$new()
  testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
  testthat::expect_error(reporter$append_metadata("sth"), "'list', not 'character'")
  testthat::expect_error(reporter$append_metadata(list("sth")), "Must have names")
})

testthat::test_that("append_metadata accept only unique names which could not be repeated", {
  reporter <- Reporter$new()
  testthat::expect_error(reporter$append_metadata(list(sth = "sth", sth = 2)), "but element 2 is duplicated")
  reporter <- Reporter$new()
  testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
  testthat::expect_error(reporter$append_metadata(list(sth = "sth")), "failed: Must be TRUE")
})

testthat::test_that("get_metadata", {
  reporter <- Reporter$new()
  testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
  testthat::expect_identical(reporter$get_metadata(), list(sth = "sth"))
})

testthat::test_that("from_reporter returns identical/equal object from the same reporter", {
  reporter <- test_reporter(card1 <- test_card1(), card2 <- test_card2())
  lifecycle::expect_deprecated(
    testthat::expect_identical(reporter, reporter$from_reporter(reporter))
  )
})

testthat::test_that("from_reporter does not return identical/equal object form other reporter", {
  reporter1 <- test_reporter(card1 <- test_card1(), card2 <- test_card2())
  reporter2 <- Reporter$new()
  lifecycle::expect_deprecated(
    testthat::expect_false(identical(reporter1, reporter2$from_reporter(reporter1)))
  )
})

testthat::test_that("from_reporter persists the cards structure", {
  lifecycle::expect_deprecated(
    testthat::expect_identical(unname(reporter1$get_cards()), unname(reporter2$from_reporter(reporter1)$get_cards()))
  )
})

testthat::describe("metadata", {
  it("append_metadata accept only named list", {
    reporter <- Reporter$new()
    testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
    testthat::expect_error(reporter$append_metadata("sth"), "'list', not 'character'")
    testthat::expect_error(reporter$append_metadata(list("sth")), "Must have names")
  })

  it("append_metadata accept only unique names which could not be repeated", {
    reporter <- Reporter$new()
    testthat::expect_error(reporter$append_metadata(list(sth = "sth", sth = 2)), "but element 2 is duplicated")
    reporter <- Reporter$new()
    testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
    testthat::expect_error(reporter$append_metadata(list(sth = "sth")), "failed: Must be TRUE")
  })

  it("get_metadata", {
    reporter <- Reporter$new()
    testthat::expect_no_error(reporter$append_metadata(list(sth = "sth")))
    testthat::expect_identical(reporter$get_metadata(), list(sth = "sth"))
  })
})

testthat::describe("from_reporter", {
  it("from_reporter returns identical/equal object from the same reporter", {
    reporter <- test_reporter()
    lifecycle::expect_deprecated(
      testthat::expect_identical(reporter, reporter$from_reporter(reporter))
    )
  })

  it("from_reporter does not return identical/equal object form other reporter", {
    reporter1 <- test_reporter(test_card1(), test_card2())
    reporter2 <- Reporter$new()

    lifecycle::expect_deprecated(
      testthat::expect_false(identical(reporter1, reporter2$from_reporter(reporter1)))
    )
  })

  it("from_reporter persists the cards structure, but not the name", {
    reporter1 <- test_reporter(test_card1(), test_card2())
    reporter2 <- Reporter$new()
    lifecycle::expect_deprecated(
      testthat::expect_identical(
        unname(reporter1$get_cards()),
        unname(reporter2$from_reporter(reporter1)$get_cards())
      )
    )
  })
})

testthat::describe("to_list", {
  it("require the existing directory path", {
    reporter1 <- test_reporter(test_card1(), test_card2())
    testthat::expect_error(reporter1$to_list(), 'argument "output_dir" is missing, with no default')
    testthat::expect_error(reporter1$to_list("/path/WRONG"), "Directory '/path/WRONG' does not exist.")
  })

  it("returns a list.", {
    temp_dir <- withr::local_tempdir()
    testthat::expect_equal(
      list(name = "teal Reporter", version = "1", id = "", cards = list(), metadata = list()),
      Reporter$new()$to_list(temp_dir)
    )
  })

  it("to_list and from_list could be used to save and retrieve a Reporter card", {
    temp_dir <- withr::local_tempdir()
    reporter1 <- test_reporter(test_card1(), test_card2())
    testthat::expect_identical(
      length(reporter1$get_cards()),
      length(Reporter$new()$from_list(reporter1$to_list(temp_dir), temp_dir)$get_cards())
    )
    testthat::expect_identical(
      length(reporter1$get_blocks()),
      length(Reporter$new()$from_list(reporter1$to_list(temp_dir), temp_dir)$get_blocks())
    )
  })

  it("to_list and from_list could be used to save and retrieve a Reporter blocks", {
    temp_dir <- withr::local_tempdir()
    reporter1 <- test_reporter(test_card1(), test_card2())
    testthat::expect_identical(
      length(reporter1$get_blocks()),
      length(Reporter$new()$from_list(reporter1$to_list(temp_dir), temp_dir)$get_blocks())
    )
  })
})

testthat::describe("from_reporter", {
  it("returns same object from the same reporter", {
    shiny::reactiveConsole(TRUE)
    reporter <- test_reporter(card1 <- test_card1(), card2 <- test_card2())
    lifecycle::expect_deprecated(
      testthat::expect_identical(reporter, teal.reporter::Reporter$new()$from_reporter(reporter))
    )
  })

  it("returns different object if id has already been set", {
    reporter1 <- test_reporter(test_card1(), test_card2())
    reporter2 <- teal.reporter::Reporter$new()
    reporter1$set_id("a_id")
    testthat::expect_failure(
      lifecycle::expect_deprecated(
        testthat::expect_identical(reporter1, (reporter2$from_reporter(reporter1)))
      ),
      "not identical to"
    )
  })

  it("from_reporter persists the cards structure", {
    reporter1 <- test_reporter(test_card1(), test_card2())
    reporter2 <- teal.reporter::Reporter$new()
    lifecycle::expect_deprecated(
      testthat::expect_equal(
        reporter1$get_cards(),
        reporter2$from_reporter(reporter1)$get_cards(),
        ignore_attr = "names"
      )
    )
  })
})

testthat::describe("to_jsondir", {
  it("to_jsondir require the existing directory path", {
    reporter <- test_reporter(test_card1(), test_card2())
    testthat::expect_error(reporter$to_jsondir(), 'argument "output_dir" is missing, with no default')
    testthat::expect_error(reporter$to_jsondir("/path/WRONG"), "Directory '/path/WRONG' does not exist.")
  })

  it("to_jsondir returns the same dir it was provided to it", {
    temp_dir <- withr::local_tempdir()
    reporter <- test_reporter(test_card1(), test_card2())
    testthat::expect_identical(temp_dir, reporter$to_jsondir(temp_dir))
  })

  it("from_jsondir returns identical/equal object", {
    temp_dir <- withr::local_tempdir()
    reporter <- test_reporter(test_card1(), test_card2())
    reporter$to_jsondir(temp_dir)
    testthat::expect_identical(reporter, reporter$from_jsondir(temp_dir))
  })

  it("to_jsondir and from_jsondir could be used to save and retrive a Reporter", {
    temp_dir <- withr::local_tempdir()
    reporter <- test_reporter(test_card1(), test_card2())
    reporter_arch <- reporter$from_jsondir(reporter$to_jsondir(temp_dir))
    testthat::expect_identical(reporter$get_cards(), reporter_arch$get_cards())
    testthat::expect_identical(reporter$get_metadata(), reporter_arch$get_metadata())
  })
})

testthat::describe("reorder_cards", {
  card1 <- teal_card("# Section 1")
  metadata(card1, "title") <- "Card1"
  card2 <- teal_card("# Section A")
  metadata(card2, "title") <- "Card2"
  card3 <- teal_card("# Section I")
  metadata(card3, "title") <- "Card3"
  card4 <- teal_card("# Section i")
  metadata(card4, "title") <- "Card4"

  it("returns the correct order", {
    reporter <- teal.reporter::Reporter$new() # prefix needed in "it" to avoid testthat::Reporter
    reporter$append_cards(list(card1, card2, card3))

    names_before <- names(reporter$get_cards())
    reporter$reorder_cards(rev(names_before))
    names_after <- names(reporter$get_cards())

    testthat::expect_equal(names_after, rev(names_before))
  })

  it("returns the correct order after removal", {
    reporter <- teal.reporter::Reporter$new() # prefix needed in "it" to avoid testthat::Reporter
    reporter$append_cards(list(card1, card2, card3))

    names_before <- names(reporter$get_cards())
    reporter$reorder_cards(rev(names_before))
    name_to_remove <- sample(names_before, 1) # Random pick to avoid any bias
    reporter$remove_cards(name_to_remove)

    names_after <- names(reporter$get_cards())
    testthat::expect_equal(names_after, rev(names_before[names_before != name_to_remove]))
  })

  it("returns the correct order after adding (new card at the end)", {
    reporter <- teal.reporter::Reporter$new() # prefix needed in "it" to avoid testthat::Reporter
    reporter$append_cards(list(card1, card2, card3))

    names_before <- names(reporter$get_cards())
    reporter$reorder_cards(rev(names_before))
    reporter$append_cards(card4)

    names_after <- names(reporter$get_cards())
    testthat::expect_equal(names_after, c(rev(names_before), setdiff(names_after, names_before)))
  })
})

testthat::test_that("from_reporter persists the cards structure", {
  reporter1 <- teal.reporter::Reporter$new()
  card1 <- test_card1("A title")
  card2 <- test_card2("Another title")
  reporter1$append_cards(list(card1, card2))
  lifecycle::expect_deprecated(
    expect_equal(
      unname(reporter1$get_cards()),
      unname(Reporter$new()$from_reporter(reporter1)$get_cards())
    )
  )
})

testthat::describe("Reporter with custom template function", {
  it("modifies teal_cards on append", {
    card <- teal_card("## A Header", "A paragraph")
    reporter <- Reporter$new()

    template_fun <- function(card) c(teal_card("Here comes disclaimer text"), card)

    reporter$set_template(template_fun)
    reporter$append_cards(card)

    testthat::expect_equal(reporter$get_cards()[[1]][[1]], "Here comes disclaimer text")
  })

  it("removes chunk_outputs and code_chunk", {
    tr <- within(teal_report(), 1 + 1)
    teal_card(tr) <- c(teal_card(tr), "A separator")
    tr <- within(tr, iris <- iris)
    teal_card(tr) <- c(teal_card(tr), "A footer")

    reporter <- Reporter$new()

    template_fun <- function(card) {
      Filter(
        f = function(x) !(inherits(x, "chunk_output") || inherits(x, "code_chunk")),
        x = card
      )
    }

    reporter$set_template(template_fun)
    reporter$append_cards(teal_card(tr))

    expected <- teal_card("A separator", "A footer")
    metadata(expected) <- list()

    testthat::expect_equal(
      unname(reporter$get_cards()[[1]]),
      unname(expected),
      ignore_attr = TRUE
    )
  })
})
