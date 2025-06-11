testthat::describe("reporter_previewer", {
  # https://github.com/rstudio/sortable/issues/123
  # teal.reporter issue: https://github.com/insightsengineering/teal.reporter/issues/336
  it("card reordering works")

  it("card removal works", {
    app <- start_reporter_preview_app("reporter_previewer_remove")

    initial_count <- length(app$get_values()$export[["preview-cards"]])

    remove_card_id <- sprintf(
      "preview-cards-%s-actions-remove_action",
      names(app$get_values()$export[["preview-cards"]])[[1]]
    )

    app$click(remove_card_id)
    app$wait_for_idle()
    testthat::expect_equal(length(app$get_values()$export[["preview-cards"]]), initial_count - 1)
  })

  it("card editing modal is being shown", {
    app <- start_reporter_preview_app("reporter_previewer_edit")

    edit_card_id <- sprintf(
      "preview-cards-%s-actions-edit_action",
      names(app$get_values()$export[["preview-cards"]])[[1]]
    )

    app$click(edit_card_id)
    app$wait_for_idle()

    modal_visible <- app$get_js("
      !!document.querySelector('.modal.show') &&
      document.querySelector('.modal-title').textContent.includes('Editing')
    ")
    testthat::expect_true(modal_visible)
  })

  it("card download")

  it("card editing")
})
