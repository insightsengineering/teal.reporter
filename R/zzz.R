.onLoad <- function(libname, pkgname) {
  # adapted from https://r-pkgs.org/code.html#sec-code-onLoad-onAttach
  op <- options()
  op.teal.reporter <- list(teal.reporter.global_knitr = list(echo = TRUE,
                                                             tidy.opts = list(width.cutoff = 60),
                                                             tidy = FALSE))

  toset <- !(names(op.teal.reporter) %in% names(op))
  if (any(toset)) options(op.teal.reporter[toset])

  invisible()
}
