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

  options(teal.reporter.objects = list(
    character = function(b) shiny::tags$pre(b),
    ggplot = function(b) shiny::tags$img(src = knitr::image_uri(b)),
    data.frame = function(b) shiny::tags$pre(knitr::kable(b))
  ))

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("formatR", quietly = TRUE)) {
    packageStartupMessage(
      "For better code formatting, consider installing the formatR package."
    )
  }
}
