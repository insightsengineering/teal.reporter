#' @importFrom tools toHTML
NULL

#' @method toHTML default
#' @keywords internal
#' @export
toHTML.default <- function(x, ...) {
  .toHTML(x, ...)
}

#' @keywords internal
.toHTML <- function(x, ...) { # nolint: object_name.
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
  shiny::tagList(lapply(x$get_content(), tools::toHTML, ...))
}

#' @method .toHTML teal_card
#' @keywords internal
.toHTML.teal_card <- function(x, ...) {
  shiny::tagList(lapply(x, tools::toHTML, ...))
}

#' @method .toHTML teal_report
#' @keywords internal
.toHTML.teal_report <- function(x, ...) {
  tools::toHTML(teal_card(x), ...)
}

#' @method .toHTML rtables
#' @keywords internal
.toHTML.rtables <- function(x, ...) {
  shiny::tags$pre(tools::toHTML(to_flextable(x)))
}

#' @method .toHTML flextable
#' @keywords internal
.toHTML.flextable <- function(x, ...) {
  flextable::htmltools_value(x)
}

#' @method .toHTML condition
#' @keywords internal
.toHTML.condition <- function(x, ...) {
  conditionMessage(x)
}

.plot2html <- function(x, ...) {
  on.exit(unlink(tmpfile))
  tmpfile <- tempfile(fileext = ".png")
  dims <- resolve_figure_dimensions(x)
  grDevices::png(filename = tmpfile, width = dims$width, height = dims$height)
  print(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile), style = "width: 100%; height: auto;")
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
  dims <- resolve_figure_dimensions(x, convert_to_inches = TRUE, dpi = 100)
  tmpfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmpfile, plot = x, width = dims$width, height = dims$height, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML grob
#' @keywords internal
.toHTML.grob <- function(x, ...) {
  on.exit(unlink(tmpfile))
  dims <- resolve_figure_dimensions(x)
  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(filename = tmpfile, width = dims$width, height = dims$height)
  grid::grid.newpage()
  grid::grid.draw(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML pseudo_code_chunk
#' @keywords internal
.toHTML.pseudo_code_chunk <- function(x, ...) {
  toHTML(x[[1]], ...)
}

#' @method .toHTML code_chunk
#' @keywords internal
.toHTML.code_chunk <- function(x, ...) {
  bslib::accordion(
    class = "code_chunk",
    bslib::accordion_panel(
      title = shiny::tags$span(shiny::icon("code"), "R Code"),
      value = "rcode",
      open = FALSE,
      shiny::tags$pre(
        shiny::tags$code(x, class = sprintf("language-%s", attr(x, "lang"))),
        .noWS = "inside"
      )
    )
  )
}

#' @method .toHTML chunk_output
#' @keywords internal
.toHTML.chunk_output <- function(x, ...) {
  new_x <- x[[1]]
  mostattributes(new_x) <- c(attributes(unclass(x)), attributes(new_x))
  tools::toHTML(new_x, ...)
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
  x
}

#' @method .toHTML gtsummary
#' @keywords internal
.toHTML.gtsummary <- function(x, ...) {
  tools::toHTML(gtsummary::as_flex_table(x))
}

#' @method .toHTML listing_df
#' @keywords internal
.toHTML.listing_df <- function(x, ...) {
  tools::toHTML(flextable::as_flextable(x))
}
