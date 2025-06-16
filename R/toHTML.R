#' @importFrom tools toHTML
NULL

#' @method toHTML default
#' @keywords internal
#' @export
toHTML.default <- function(x, ...) {
  .toHTML(x, ...)
}

#' @keywords internal
.toHTML <- function(x, ...) { # nolint: object_name
  UseMethod(".toHTML", x)
}

#' @method .toHTML default
#' @keywords internal
.toHTML.default <- function(x, ...) {
  shiny::HTML(commonmark::markdown_html(x, extensions = TRUE))
}

#' @method .toHTML ContentBlock
#' @keywords internal
.toHTML.ContentBlock <- function(x, ...) {
  UseMethod("toHTML", x$get_content()) # Further dispatch for subclasses
}

#' @method .toHTML ReportCard
#' @keywords internal
.toHTML.ReportCard <- function(x, ...) {
  htmltools::tagList(lapply(x$get_content(), toHTML))
}

#' @method .toHTML teal_card
#' @keywords internal
.toHTML.teal_card <- function(x, ...) {
  htmltools::tagList(lapply(x, toHTML, ...))
}

#' @method .toHTML teal_report
#' @keywords internal
.toHTML.teal_report <- function(x, ...) {
  toHTML(teal_card(x), ...)
}

#' @method .toHTML TextBlock
#' @keywords internal
.toHTML.TextBlock <- function(x, ...) {
  b_content <- x$get_content()
  switch(x$get_style(),
    header1 = shiny::tags$h1(b_content),
    header2 = shiny::tags$h2(b_content),
    header3 = shiny::tags$h3(b_content),
    header4 = shiny::tags$h4(b_content),
    verbatim = shiny::tags$pre(b_content),
    shiny::tags$pre(b_content)
  )
}

#' @method .toHTML RcodeBlock
#' @keywords internal
.toHTML.RcodeBlock <- function(x, ...) {
  panel_item("R Code", shiny::tags$pre(x$get_content()))
}

#' @method .toHTML PictureBlock
#' @keywords internal
.toHTML.PictureBlock <- function(x, ...) {
  shiny::tags$img(src = knitr::image_uri(x$get_content()))
}

#' @method .toHTML TableBlock
#' @keywords internal
.toHTML.TableBlock <- function(x, ...) {
  b_table <- readRDS(x$get_content())
  shiny::tags$pre(flextable::htmltools_value(b_table))
}

#' @method .toHTML NewpageBlock
#' @keywords internal
.toHTML.NewpageBlock <- function(x, ...) {
  shiny::tags$br()
}

#' @method .toHTML HTMLBlock
#' @keywords internal
.toHTML.HTMLBlock <- function(x, ...) {
  x$get_content()
}

#' @method .toHTML rtables
#' @keywords internal
.toHTML.rtables <- function(x, ...) {
  shiny::tags$pre(flextable::htmltools_value(to_flextable(x)))
}

#' @method .toHTML gg
#' @keywords internal
.toHTML.gg <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmpfile, plot = x, width = 5, height = 4, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML trellis
#' @keywords internal
.toHTML.trellis <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(filename = tmpfile)
  print(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML grob
#' @keywords internal
.toHTML.grob <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(filename = tmpfile)
  grid::grid.newpage()
  grid::grid.draw(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}


#' @method .toHTML code_chunk
#' @keywords internal
.toHTML.code_chunk <- function(x, ...) {
  shiny::tags$pre(x)
}

#' @method .toHTML summary.lm
#' @keywords internal
.toHTML.summary.lm <- function(x, ...) {
  shiny::tags$pre(paste(capture.output(print(x)), collapse = "\n"))
}

#' @method .toHTML TableTree
#' @keywords internal
.toHTML.TableTree <- .toHTML.rtables

#' @method .toHTML ElementaryTable
#' @keywords internal
.toHTML.ElementaryTable <- .toHTML.rtables

#' @method .toHTML rlisting
#' @keywords internal
.toHTML.rlisting <- .toHTML.rtables

#' @method .toHTML data.frame
#' @keywords internal
.toHTML.data.frame <- .toHTML.rtables

#' @method .toHTML datatables
#' @keywords internal
.toHTML.datatables <- function(x, ...) {
  htmltools::as.tags(x)
}
