testthat::test_that("reporter_previewer card reordering works", {
  app <- start_reporter_preview_app("reporter_previewer_reorder")
  on.exit(try(app$stop(), silent = TRUE))

  testthat::skip("simulate_drag_and_drop does not sort yet")
  initial_order <- get_card_order(app)
  simulate_drag_and_drop(app, 1, 2)
  final_order <- get_card_order(app)

  testthat::expect_false(identical(initial_order, final_order))
  testthat::expect_equal(final_order, rev(initial_order))
})

testthat::test_that("reporter_previewer card removal works", {
  app <- start_reporter_preview_app("reporter_previewer_remove")
  on.exit(try(app$stop(), silent = TRUE))

  initial_count <- length(app$get_js("document.querySelectorAll('.accordion-header')"))

  app$run_js("
    const removeBtn = document.querySelector('.accordion-header .btn-danger');
    if (removeBtn) removeBtn.click();
  ")

  app$wait_for_idle()
  Sys.sleep(0.5)

  final_count <- length(app$get_js("document.querySelectorAll('.accordion-header')"))

  testthat::expect_equal(final_count, initial_count - 1)
})

testthat::test_that("reporter_previewer card editing works", {
  app <- start_reporter_preview_app("reporter_previewer_edit")
  on.exit(try(app$stop(), silent = TRUE))

  app$run_js("
    const editBtn = document.querySelector('.accordion-header .btn-primary');
    if (editBtn) editBtn.click();
  ")

  app$wait_for_idle()
  Sys.sleep(0.5)

  modal_visible <- app$get_js("
    !!document.querySelector('.modal.show') &&
    document.querySelector('.modal-title').textContent.includes('Editing')
  ")
  testthat::expect_true(modal_visible)
})

testthat::test_that("reporter_previewer download functionality works", {
  app <- start_reporter_preview_app("reporter_previewer_download")
  on.exit(try(app$stop(), silent = TRUE))

  initial_btn_exists <- app$get_js("
    !!document.querySelector('#preview-download-download_button')
  ")
  testthat::expect_true(initial_btn_exists)

  app$run_js("
    const initialBtn = document.querySelector('#preview-download-download_button');
    if (initialBtn) initialBtn.click();
  ")

  app$wait_for_idle()
  Sys.sleep(0.5)

  modal_visible <- app$get_js("
    !!document.querySelector('.modal.show')
  ")
  testthat::expect_true(modal_visible)

  temp_dir <- tempfile("downloads")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  app$run_js("
    const modalDownloadBtn = document.querySelector('#preview-download-download_data');
    if (modalDownloadBtn) modalDownloadBtn.click();
  ")

  app$wait_for_idle()
  Sys.sleep(2)

  # TO DO - verify that download actually happened
  # downloaded_files <- list.files(temp_dir, pattern = "\\.html$")
  # testthat::expect_length(downloaded_files, 1)
})
