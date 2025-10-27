testthat::describe("srv_card_editor", {
  it("adds new block to queue", {
    shiny::testServer(srv_card_editor, args = list(card_r = shiny::reactiveVal(list(one = "1"))), {
      expect_null(blocks_queue_rv())
      session$setInputs(add_block = 1)
      session$flushReact()
      testthat::expect_equal("block", blocks_queue_rv())
    })
  })
})
