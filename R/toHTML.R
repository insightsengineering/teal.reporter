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

#' @method .toHTML rtables
#' @keywords internal
.toHTML.rtables <- function(x, ...) {
  shiny::tags$pre(flextable::htmltools_value(to_flextable(x)))
}

#' @method .toHTML condition
#' @keywords internal
.toHTML.condition <- function(x, ...) {
  conditionMessage(x)
}

.plot2html <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(filename = tmpfile)
  print(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML recordedplot
#' @keywords internal
.toHTML.recordedplot <- .plot2html


#' @method .toHTML trellis
#' @keywords internal
.toHTML.trellis <- .plot2html

#' @method .toHTML gg
#' @keywords internal
.toHTML.gg <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmpfile, plot = x, width = 5, height = 4, dpi = 100)
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
  shiny::tags$pre(
    shiny::tags$code(x, class = sprintf("language-%s", attr(x, "lang")))
  )
}

#' @method .toHTML chunk_output
#' @keywords internal
.toHTML.chunk_output <- function(x, ...) {
  toHTML(x[[1]], ...)
}

#' @method .toHTML code_chunk
#' @keywords internal
.toHTML.code_output <- function(x, ...) {
  toHTML(x[[1]])
}


#' @method .toHTML summary.lm
#' @keywords internal
.toHTML.summary.lm <- function(x, ...) {
  shiny::tags$pre(paste(utils::capture.output(print(x)), collapse = "\n"))
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
