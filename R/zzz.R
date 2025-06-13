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

  # Manual import instead of using backports and adding 1 more dependency
  if (getRversion() < "4.4") {
    `%||%` <- function(x, y) if (is.null(x)) y else x
    assign("`%||%`", `%||%`, envir = getNamespace(pkgname))
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("formatR", quietly = TRUE)) {
    packageStartupMessage(
      "For better code formatting, consider installing the formatR package."
    )
  }
}
