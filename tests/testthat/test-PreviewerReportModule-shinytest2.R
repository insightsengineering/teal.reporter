testthat::describe("preview_report", {
  it("card removal works", {
    app <- start_reporter_preview_app("reporter_previewer_remove")

    initial_count <- length(app$get_values()$export[["preview-preview_content-cards"]])

    remove_card_id <- sprintf(
      "preview-preview_content-reporter_cards-%s-actions-remove_action",
      names(app$get_values()$export[["preview-preview_content-cards"]])[[1]]
    )

    app$click("preview-preview_button")
    app$click(remove_card_id)
    app$wait_for_idle()
    testthat::expect_equal(length(app$get_values()$export[["preview-preview_content-cards"]]), initial_count - 1)
  })

  it("card editing modal is being shown", {
    app <- start_reporter_preview_app("reporter_previewer_edit")

    edit_card_id <- sprintf(
      "preview-preview_content-reporter_cards-%s-actions-edit_action",
      names(app$get_values()$export[["preview-preview_content-cards"]])[[1]]
    )

    app$click(edit_card_id)
    app$wait_for_idle()

    modal_visible <- app$get_js("
      !!document.querySelector('#shiny-modal > div > div > div.modal-header > h4 > span') &&
      document.querySelector('#shiny-modal > div > div > div.modal-header > h4 > span').textContent.includes(' Editing')
    ")
    testthat::expect_true(modal_visible)
  })
})
