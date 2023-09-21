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


testthat::test_that("conv_str_logi - accept only a string", {
  testthat::expect_error(conv_str_logi(2))
  testthat::expect_error(conv_str_logi("string"), NA)
})

testthat::test_that("conv_str_logi - not influence the regular character", {
  testthat::expect_identical(conv_str_logi("sth"), "sth")
})

testthat::test_that("conv_str_logi - character TRUE to logical", {
  testthat::expect_true(isTRUE(conv_str_logi("TRUE")))
  testthat::expect_true(isTRUE(conv_str_logi("true")))
  testthat::expect_true(isTRUE(conv_str_logi("True")))
  testthat::expect_true(isTRUE(conv_str_logi("yes")))
  testthat::expect_true(isTRUE(conv_str_logi("y")))
  testthat::expect_true(isTRUE(conv_str_logi("Y")))
  testthat::expect_true(isTRUE(conv_str_logi("on")))
})

testthat::test_that("conv_str_logi - character FALSE to logical", {
  testthat::expect_true(isFALSE(conv_str_logi("FALSE")))
  testthat::expect_true(isFALSE(conv_str_logi("false")))
  testthat::expect_true(isFALSE(conv_str_logi("False")))
  testthat::expect_true(isFALSE(conv_str_logi("no")))
  testthat::expect_true(isFALSE(conv_str_logi("n")))
  testthat::expect_true(isFALSE(conv_str_logi("N")))
  testthat::expect_true(isFALSE(conv_str_logi("off")))
})


testthat::test_that("rmd_outputs - all returned out in the rmarkdown namespace", {
  testthat::expect_true(all(rmd_outputs() %in% ls(asNamespace("rmarkdown"))))
})

testthat::test_that("rmd_output_arguments - accepts only string from possible rmarkdown outputs", {
  testthat::expect_error(rmd_output_arguments("random_text"))
  testthat::expect_error(rmd_output_arguments("pdf_document"), NA)
  testthat::expect_error(rmd_output_arguments("pdf_document", TRUE), NA)
})

testthat::test_that("rmd_output_arguments - returned all pdf_document arguments", {
  testthat::expect_identical(
    rmd_output_arguments("pdf_document"),
    names(formals(asNamespace("rmarkdown")[["pdf_document"]]))
  )
})

testthat::test_that("rmd_output_arguments - returned all pdf_document arguments and their defaults", {
  testthat::expect_identical(
    rmd_output_arguments("pdf_document", TRUE),
    formals(asNamespace("rmarkdown")[["pdf_document"]])
  )
})

testthat::test_that("as_yaml_auto - accept a named list (optionally nested)", {
  testthat::expect_error(as_yaml_auto(list(1)))
  testthat::expect_error(as_yaml_auto("sth"))

  testthat::expect_error(as_yaml_auto(list(author = "", output = list(pdf_document = list(toc = TRUE)))), NA)
  testthat::expect_error(as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)), NA)
})

testthat::test_that("as_yaml_auto - works the same as yaml::as.yaml for a nested list when as_header is FALSE", {
  testthat::expect_identical(
    as_yaml_auto(list(author = "", output = list(pdf_document = list(toc = TRUE))), as_header = FALSE),
    structure(yaml::as.yaml(list(author = "", output = list(pdf_document = list(toc = TRUE)))),
      class = "rmd_yaml_header"
    )
  )
})

input_list <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE, date = as.Date("2022-04-29"))

testthat::test_that("as_yaml_auto - parse", {
  testthat::expect_identical(
    as_yaml_auto(input_list),
    structure(
      "---\nauthor: ''\ndate: '2022-04-29'\noutput:\n  pdf_document:\n    toc: yes\n    keep_tex: yes\n---\n",
      class = "rmd_yaml_header"
    )
  )
})

testthat::test_that("as_yaml_auto - warning for not accepted argument and skip it", {
  testthat::expect_warning(testthat::expect_identical(
    as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE, wrong = 2)),
    structure(
      "---\nauthor: ''\noutput:\n  pdf_document:\n    toc: yes\n    keep_tex: yes\n---\n",
      class = "rmd_yaml_header"
    )
  ))
})

testthat::test_that("as_yaml_auto - silent the warning for not accepted argument and skip it", {
  testthat::expect_identical(
    as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE, wrong = 2), silent = TRUE),
    structure("---\nauthor: ''\noutput:\n  pdf_document:\n    toc: yes\n    keep_tex: yes\n---\n",
      class = "rmd_yaml_header"
    )
  )
})

testthat::test_that("as_yaml_auto - convert character logical to logical", {
  testthat::expect_identical(
    as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "True"), silent = TRUE),
    structure("---\nauthor: ''\noutput:\n  pdf_document:\n    toc: yes\n    keep_tex: yes\n---\n",
      class = "rmd_yaml_header"
    )
  )
})


testthat::test_that("as_yaml_auto - do not accept multi outputs without the multi_output argument", {
  testthat::expect_error(
    as_yaml_auto(list(author = "", output = "pdf_document", output = "html_document", toc = TRUE, keep_tex = TRUE),
      silent = TRUE
    )
  )
})

testthat::test_that("as_yaml_auto - accept multi outputs with the multi_output argument", {
  testthat::expect_error(
    as_yaml_auto(list(author = "", output = "pdf_document", output = "html_document", toc = TRUE, keep_tex = TRUE),
      silent = TRUE, multi_output = TRUE
    ),
    NA
  )
})

test_that("reverse_yaml_field returns the correct result", {
  yaml_text <- "---\nauthor: ''\ndate: '2022-04-29'\noutput:\n  pdf_document:\n    toc: yes\n    keep_tex: yes\n---\n"
  field_name <- "output"
  result <- reverse_yaml_field(yaml_text, field_name)
  expect_equal(result, "pdf_document")
})
