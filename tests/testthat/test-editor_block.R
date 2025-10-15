testthat::describe("srv_editor_block: ", {
  it("for default method returns NULL", {
    shiny::testServer(
      srv_editor_block,
      args = list(id = "id", value = 1),
      expr = {
        session$setInputs(content = value)
        expect_equal(result, NULL)
      }
    )
  })

  it("for default method returns NULL", {
    shiny::testServer(
      srv_editor_block,
      args = list(id = "id", value = "1"),
      expr = {
        session$setInputs(content = value)
        expect_equal(result(), value)
      }
    )
  })
})
