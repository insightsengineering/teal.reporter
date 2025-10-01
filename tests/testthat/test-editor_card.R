test_that("ui_card_editor returns a shiny tag or tag list", {
  result <- ui_card_editor("id", list("one" = "1"), NULL)
  checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  testthat::expect_match(as.character(result), "Editable markdown block")
})

test_that("ui_card_editor returns a shiny tag or tag list", {
  result <- ui_card_editor("id", list("one" = 1), list("one" = shiny::tags$span("cached")))
  checkmate::expect_multi_class(result, c("shiny.tag", "shiny.tag.list"))
  testthat::expect_match(as.character(result), "cached")
  testthat::expect_match(as.character(result), "Non-editable block")
})

test_that("srv_card_editor adds new block to queue", {
  shiny::testServer(srv_card_editor, args = list(card_r = shiny::reactiveVal(list(one = "1"))), {
    expect_null(blocks_queue_rv())
    session$setInputs(add_block = 1)
    session$flushReact()
    testthat::expect_equal("block", blocks_queue_rv())
  })
})
