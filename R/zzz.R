.onLoad <- function(libname, pkgname) {
  # adapted from https://r-pkgs.org/code.html#sec-code-onLoad-onAttach
  op <- options()
  default_global_knitr <- list(teal.reporter.global_knitr = list(
    echo = TRUE,
    tidy.opts = list(width.cutoff = 60),
    tidy = FALSE
  ))

  toset <- !(names(default_global_knitr) %in% names(op))
  if (any(toset)) options(default_global_knitr[toset])

  if (!requireNamespace("formatR", quietly = TRUE)) {
    message("For better code formatting, consider installing the formatR package.")
  }

  invisible()
}
