
testthat::test_that("report_document creates a valid ReportDocument", {
  report <- report_document("Title", "Content", 123)
  testthat::expect_s3_class(report, "ReportDocument")
  testthat::expect_length(report, 3)
  testthat::expect_identical(report[[1]], "Title")
})

testthat::test_that("append adds elements at the correct position", {
  report <- report_document("Title", "Content")
  report <- append(report, list("New Section"), after = 1)

  testthat::expect_length(report, 3)
  testthat::expect_identical(report[[2]], "New Section")
})


testthat::test_that("edit_report_document correctly modifies and appends elements", {
  report <- report_document("A", "B", "C")

  # Modify order
  modified_report <- edit_report_document(report, modify = c(3, 1))
  testthat::expect_identical(modified_report, report_document("C", "A"))
  testthat::expect_s3_class(modified_report, "ReportDocument")

  # Append new element
  appended_report <- edit_report_document(report, append = "D")
  testthat::expect_length(appended_report, 4)
  testthat::expect_identical(appended_report[[4]], "D")
})


testthat::test_that("edit_report_document handles empty and null cases correctly", {
  report <- report_document()
  testthat::expect_s3_class(report, "ReportDocument")
  testthat::expect_length(report, 0)

  modified_report <- edit_report_document(report, modify = NULL, append = "X")
  testthat::expect_length(modified_report, 1)
  testthat::expect_identical(modified_report[[1]], "X")
})
