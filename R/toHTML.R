#' @importFrom tools toHTML
NULL

#' Convert report objects to HTML
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The `toHTML` S3 generic method converts various report objects into HTML representations.
#' This is the primary method for rendering report content for display in web browsers,
#' RStudio Viewer, or for inclusion in Shiny applications.
#'
#' @param x The object to convert to HTML. Supported types include:
#'   - `teal_card`: A list-like structure containing report elements
#'   - `teal_report`: A report object containing a `teal_card`
#'   - `ReportCard`: Deprecated R6 class for report cards
#'   - `code_chunk`: Code blocks created with [code_chunk()]
#'   - `chunk_output`: Output from evaluated code chunks
#'   - Plot objects: `ggplot`, `recordedplot`, `trellis`, `grob`
#'   - Table objects: `data.frame`, `rtables`, `TableTree`, `ElementaryTable`,
#'     `listing_df`, `gtsummary`, `flextable`, `datatables`
#'   - Text: `character` strings (rendered as markdown)
#'   - Other objects: Conditions, model summaries, etc.
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' ## Relationship with `teal_card`
#'
#' The `teal_card` class is a central component in the `teal.reporter` ecosystem. It is an S3 list
#' where each element represents a piece of report content (text, plots, tables, code chunks, etc.).
#' The `toHTML` method for `teal_card` objects:
#'
#' 1. Iterates through each element in the `teal_card` list
#' 2. Calls `toHTML()` recursively on each element based on its class
#' 3. Wraps all converted elements in a [bslib::card()] container
#'
#' This hierarchical conversion allows complex report structures to be rendered as styled HTML
#' with proper formatting for each content type.
#'
#' ## Content Type Conversions
#'
#' **Text and Markdown:** Character strings are converted to HTML using CommonMark markdown syntax.
#' Supports headers, lists, code blocks, emphasis, and other markdown features.
#'
#' **Code Chunks:** Created with [code_chunk()], these are rendered as collapsible Bootstrap
#' accordions with syntax highlighting. The accordion includes the programming language indicator
#' and an icon.
#'
#' **Plots:** Plot objects (`ggplot`, `recordedplot`, `trellis`, `grob`) are converted to PNG
#' images with base64-encoded data URIs, making them self-contained in the HTML output.
#'
#' **Tables:** Table objects are converted to styled HTML tables, typically via `flextable`
#' for consistent formatting.
#'
#' ## Viewer Integration
#'
#' All HTML output is wrapped with [htmltools::browsable()], which enables:
#' - Automatic display in RStudio/VS-code Viewer when displayed interactively
#' - Proper HTML dependency injection (Bootstrap CSS/JS, Font Awesome icons, etc.)
#' - Standalone HTML files with all required resources
#'
#' You can override the browsable behavior with:
#' ```r
#' print(toHTML(x), browse = FALSE)  # Print markup to console instead
#' ```
#'
#' @return An HTML representation of the input object. The exact return type depends on the
#'   input class:
#'   - For `teal_card`: A `bslib::card()` containing all elements
#'   - For `code_chunk`: A `bslib::accordion()` with the code
#'   - For plots: A `shiny::tags$img()` tag
#'   - For text: HTML markup from markdown conversion
#'   - For tables: HTML table elements
#'
#'   All returns are wrapped with `htmltools::browsable()` to enable viewer display.
#'
#' @seealso
#' - [teal_card()] for creating report cards
#' - [code_chunk()] for creating code blocks
#' - [render()] for rendering complete reports to files
#'
#' @examples
#'
#' # Convert a simple text element
#' html <- tools::toHTML("# Report Title")
#'
#' # Print html in a viewer
#' html
#'
#' # Print HTML markup to console instead of viewer
#' print(html, browse = FALSE)
#'
#' # Convert a teal_card with multiple elements
#' card <- teal_card(
#'   "# Analysis Report",
#'   "## Summary Statistics",
#'   summary(iris)
#' )
#' tools::toHTML(card)
#'
#' # Convert a code chunk
#' code <- code_chunk("x <- 1 + 1", echo = TRUE)
#' tools::toHTML(code)
#'
#' # Convert a complete teal_report
#' report <- teal_report(teal_card = teal_card("## Results", "Analysis complete"))
#' report <- within(report, iris)
#' tools::toHTML(report)
#'
#' @export
#' @method toHTML default
toHTML.default <- function(x, ...) {
  htmltools::browsable(.toHTML(x, ...))
}

#' @keywords internal
.toHTML <- function(x, ...) { # nolint: object_name.
  UseMethod(".toHTML", x)
}

#' @method .toHTML default
#' @keywords internal
.toHTML.default <- function(x, ...) {
  htmltools::HTML(commonmark::markdown_html(x, extensions = TRUE))
}

#' @method .toHTML ReportCard
#' @keywords internal
.toHTML.ReportCard <- function(x, ...) {
  shiny::tagList(lapply(x$get_content(), tools::toHTML, ...))
}

#' @method .toHTML teal_card
#' @keywords internal
.toHTML.teal_card <- function(x, ...) {
  bslib::card(lapply(x, tools::toHTML, ...))
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
  dims <- .determine_default_dimensions(x)
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
  dims <- .determine_default_dimensions(x, convert_to_inches = TRUE, dpi = 100)
  tmpfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmpfile, plot = x, width = dims$width, height = dims$height, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML grob
#' @keywords internal
.toHTML.grob <- function(x, ...) {
  on.exit(unlink(tmpfile))
  dims <- .determine_default_dimensions(x)
  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(filename = tmpfile, width = dims$width, height = dims$height)
  grid::grid.newpage()
  grid::grid.draw(x)
  grDevices::dev.off()
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @method .toHTML code_chunk
#' @keywords internal
.toHTML.code_chunk <- function(x, ...) {
  bslib::accordion(
    class = "code_chunk",
    open = FALSE,
    bslib::accordion_panel(
      title = shiny::tags$span(shiny::icon("code"), attr(x, "lang", exact = TRUE)),
      value = "rcode",
      shiny::tags$pre(
        shiny::tags$code(x, class = sprintf("language-%s", attr(x, "lang", exact = TRUE))),
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
