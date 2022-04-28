testthat::test_that("yaml_quoted adds the `quoted` attribute equal to `TRUE`", {
  object <- "test"
  yaml_quoted_object <- yaml_quoted(object)
  testthat::expect_equal(attr(yaml_quoted_object, "quoted"), TRUE)
})

testthat::test_that("yaml_quoted does not modify the value of the object", {
  object <- "test"
  yaml_quoted_object <- yaml_quoted(object)
  testthat::expect_equivalent(object, yaml_quoted_object)
})


testthat::test_that("TODO", {
  conv_str_logi("TRUE")
  conv_str_logi("True")

  conv_str_logi("off")
  conv_str_logi("n")

  conv_str_logi("sth")

  testthat::expect_error(conv_str_logi(TRUE))

  rmd_outputs()

  rmd_output_arguments("pdf_document")
  rmd_output_arguments("pdf_document", TRUE)


  # nested so using yaml::as.yaml directly
  as_yaml_auto(list(author = "", output = list(pdf_document = list(toc = TRUE))))

  # auto parsing for a flat list, like shiny input
  input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
  as_yaml_auto(input)

  as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "TRUE"))

  testthat::expect_warning(
    as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE, wrong = 2))
  )

  as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = 2), silent = TRUE)

  input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "True")
  as_yaml_auto(input)
  as_yaml_auto(input, convert_logi = TRUE, silent = TRUE)
  as_yaml_auto(input, silent = TRUE)
  as_yaml_auto(input, convert_logi = FALSE, silent = TRUE)

  # Fail
  testthat::expect_error(
    as_yaml_auto(list(author = "", output = "pdf_document", output = "html_document", toc = TRUE, keep_tex = TRUE))
  )
  # ok
  as_yaml_auto(list(author = "", output = "pdf_document", output = "html_document", toc = TRUE, keep_tex = TRUE),
               multi_output = TRUE)
  as_yaml_auto(list(author = "", output = "pdf_document", output = "html_document", toc = "True", keep_tex = TRUE),
               multi_output = TRUE)

})
