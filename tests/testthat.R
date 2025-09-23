pkg_name <- "teal.reporter"
library(pkg_name, character.only = TRUE)
reporter <- testthat::MultiReporter$new(list(
  testthat::CheckReporter$new(),
  testthat::JunitReporter$new(file = "junit-result.xml")
))
testthat::test_check(pkg_name, reporter = reporter)
