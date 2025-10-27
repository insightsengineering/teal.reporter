testthat::describe("preview_report", {
  it("doesn't render previewer if not clicked", {
    app <- start_reporter_preview_app("reporter_previewer_remove")
    testthat::expect_null(app$get_html(".reporter-previewer-modal"))
    app$stop()
  })

  it("shows the previewer modal when the button is clicked", {
    app <- start_reporter_preview_app("reporter_previewer_remove")
    app$click("preview-preview_button")
    testthat::expect_true(!is.null(app$get_html(".reporter-previewer-modal")))
    app$stop()
  })

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

    app$stop()
  })

  it("card editing modal is being shown", {
    app <- start_reporter_preview_app("reporter_previewer_edit")

    app$click("preview-preview_button") # previewer is a modal

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

    app$stop()
  })
})
