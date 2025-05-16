test_that("reporter_previewer card reordering works", {
  app <- start_reporter_preview_app("reporter_previewer_reorder")
  on.exit(try(app$stop(), silent = TRUE))

  initial_order <- get_card_order(app)
  simulate_drag_and_drop(app, 1, 2) # does't drag and drop yet
  final_order <- get_card_order(app)

  expect_false(identical(initial_order, final_order))
  expect_equal(final_order, rev(initial_order))

})

test_that("reporter_previewer card removal works", {
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

  expect_equal(final_count, initial_count - 1)

})

test_that("reporter_previewer card editing works", {
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
  expect_true(modal_visible)

})

test_that("reporter_previewer download functionality works", {
  app <- start_reporter_preview_app("reporter_previewer_download")
  on.exit(try(app$stop(), silent = TRUE))

  download_btn <- app$get_js("!!document.querySelector('a.btn:contains(\"Download Report\")')")
  expect_true(download_btn) # doesn't work yet - returns NULL

  temp_dir <- tempfile("downloads")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  app$set_window_options(list(  # set_window_options doesnt exist
    prefs = list(
      "download.default_directory" = temp_dir,
      "download.prompt_for_download" = FALSE
    )
  ))

  app$run_js("
    const downloadBtn = document.querySelector('a.btn:contains(\"Download Report\")');
    if (downloadBtn) downloadBtn.click();
  ") # didn't download

  app$wait_for_idle()
  Sys.sleep(2)

  downloaded_files <- list.files(temp_dir, pattern = "\\.html$")
  expect_length(downloaded_files, 1)

})
