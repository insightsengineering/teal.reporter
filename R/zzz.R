.onLoad <- function(libname, pkgname) {
  op <- options()
  default_global_knitr <- list(teal.reporter.global_knitr = list(
    echo = TRUE,
    tidy.opts = list(width.cutoff = 60),
    tidy = requireNamespace("formatR", quietly = TRUE)
  ))

  if (!("teal.reporter.global_knitr" %in% names(op))) {
    options(default_global_knitr)
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    if (!requireNamespace("formatR", quietly = TRUE)) {
      "For better code formatting, consider installing the formatR package."
    }
  )
}
