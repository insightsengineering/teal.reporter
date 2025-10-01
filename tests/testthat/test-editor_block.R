testthat::describe("ui_editor_block: UI functions return shiny tag or taglist", {
  it("for default method", {
    res <- ui_editor_block("id", 1, NULL)
    checkmate::expect_multi_class(res, c("shiny.tag", "shiny.tag.list"))
    testthat::expect_match(as.character(res), "Non-editable block")
  })

  it("for default method keeps the cached html", {
    res <- ui_editor_block("id", 1, "cached")
    checkmate::expect_multi_class(res, c("shiny.tag", "shiny.tag.list"))
    testthat::expect_match(as.character(res), "Non-editable block")
    testthat::expect_match(as.character(res), "cached")
  })

  it("for character method", {
    res <- ui_editor_block("id", "1", NULL)
    checkmate::expect_multi_class(res, c("shiny.tag", "shiny.tag.list"))
    testthat::expect_match(as.character(res), "Editable markdown block")
  })
})

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
