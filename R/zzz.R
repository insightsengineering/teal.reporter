.onLoad <- function(libname, pkgname) {
  op <- options()

  teal_reporter_default_options <- list(
    teal.reporter.global_knitr = list(
      echo = TRUE,
      tidy.opts = list(width.cutoff = 60),
      tidy = requireNamespace("formatR", quietly = TRUE)
    ),
    teal.reporter.rmd_output = c(
      "html" = "html_document", "pdf" = "pdf_document",
      "powerpoint" = "powerpoint_presentation",
      "word" = "word_document"
    ),
    teal.reporter.rmd_yaml_args = list(
      author = "NEST", title = "Report",
      date = as.character(Sys.Date()), output = "html_document",
      toc = FALSE
    )
  )

  toset <- !(names(teal_reporter_default_options) %in% names(op))
  if (any(toset)) options(teal_reporter_default_options[toset])

  # Manual import instead of using backports and adding 1 more dependency
  if (getRversion() < "4.4") {
    assign("%||%", rlang::`%||%`, envir = getNamespace(pkgname))
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
