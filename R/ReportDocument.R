#' @title `ReportDocument`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This `S3` class is designed to store, manage, edit and adjust report cards.
#' It facilitates the creation, manipulation, and serialization of report-related data.
#'
#' @return An `S3` `list` of class `ReportDocument`.
#' @param ... elements included in `ReportDocument`
#' @param x `ReportDocument` object
#' @param values objects to be included in the modified `ReportDocument`
#' @inheritParams base::append
#'
#' @examples
#' report <- report_document()
#' class(report)
#' report <- c(report, list("## Headline"), list("## Table"), list(summary(iris)))
#' report <- report[1:2]
#' report <- append(report, c(list("## Table 2"), list(summary(mtcars))), after = 1)
#' class(report)
#'
#' report_document("Report Name", 5)
#'
#' @aliases ReportDocument
#' @name report_document
#'
#' @export
report_document <- function(...){
  objects <- list(...)
  # stopifnot("All input objects must be of length 1." = all(unlist(lapply(objects, length)) == 1))
  # Above is not needed, as ggplot has length 11.
  structure(objects, class = c('ReportDocument'))
}

#' @rdname report_document
#' @export
c.ReportDocument <- function(...){
  # Regular c() drops classes and attributes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.

  # Does not work, if ReportDocument is the second element, and not the first.
  # teal.reporter::report_document() -> x
  # class(c(list(), x)) # list
  # class(c(x, list())) # ReportDocument
  # append(x, list(), after = 1) # ReportDocument
  # append(x, list(), after = 0) # list()

  input_objects <- list(...)
  attrs <- attributes(input_objects[[1]])
  objects <- do.call(c, lapply(input_objects, unclass))
  attributes(objects) <- attrs
  objects
}

#' @rdname report_document
#' @export
`[.ReportDocument` <- function(x, i) {
  # Regular [] drops classes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.
  attrs <- attributes(x)
  xi <- unclass(x)[i]
  attributes(xi) <- attrs
  xi
}

#' @rdname report_document
#' @param x `ReportDocument`
#' @param modify `integer(n)` if present, uses `[.` syntax to extract elements.
#' Can be used to reorder or substract the object
#' @param append object to be appended to `ReportDocument` with `append` syntax.
#' Use `after` to specify the position where the object should be added.
#'
#' @examples
#' report <- report_document(1, 2, 'c')
#'
#' # Modify and append to the report
#' new_report <- edit_document_content(report, modify = c(3, 1), append = 'd')
#' new_report
#' class(new_report)
#'
#' @export
edit_document_content <- function(x, modify = NULL, append = NULL, after = length(x)) {
  checkmate::assert_class(x, "ReportDocument")
  checkmate::assert_class(modify, "numeric", null.ok = TRUE)

  attrs <- attributes(x)

  if (!is.null(modify)) {
    x <- x[modify]
  }

  if (!is.null(append)) {
    x <- append(x, append, after)
  }

  attributes(x) <- attrs
  x

}

#' Generate an R Markdown code chunk
#'
#' This function takes a character string as input and formats it as an R Markdown code chunk.
#' Additional named parameters passed via `...` will be included inside `{r}`.
#'
#' @param code A character string containing the R code to be wrapped in the chunk.
#' @param ... Additional named parameters to be included inside `{r}`.
#'
#' @return A formatted character string representing an R Markdown code chunk.
#' @examples
#' code_chunk("x <- 1:10", echo = TRUE, message = FALSE)
#' @export
#' @rdname code_output
code_chunk <- function(code, ...) {
  params <- list(...)
  params_str <- if (length(params) > 0) {
    paste(names(params), params, sep = "=", collapse = ", ")
  } else {
    ""
  }

  if (params_str != "") {
    sprintf("```{r %s}\n%s\n```", params_str, code)
  } else {
    sprintf("```{r}\n%s\n```", code)
  }
}
#' @export
#' @rdname code_output
code_output <- function(code) {
  sprintf("```\n%s\n```", code)
}

#' @export
#' @rdname code_output
link_output <- function(object, output) {
  attr(object, "output") <- output
  object
}




# to_markdown <- function(card){
#   checkmate::assert_class(card, "ReportDocument")
#   card_markdown <- lapply(card, block_to_markdown)
#   class(card_markdown) <- "ReportDocument"
#   list(card_markdown)
# }

#' @rdname block_to_markdown
#' @export block_to_markdown
block_to_markdown <- function(x) UseMethod("block_to_markdown")

#' @rdname block_to_markdown
#' @method block_to_markdown default
#' @exportS3Method teal::block_to_markdown
block_to_markdown.default <- function(x) x

#' @rdname block_to_markdown
#' @method block_to_markdown ggplot
#' @exportS3Method teal::block_to_markdown
block_to_markdown.ggplot <- function(x, width = 5, height = 4, dpi = 100) {
  # Temporary file to save the plot
  tmpfile <- tempfile(fileext = ".png")

  # Save the plot as a PNG file
  ggsave(tmpfile, plot = x, width = width, height = height, dpi = dpi)

  # Read the binary data and encode as base64
  # base64enc::base64encode(tmpfile)
  base64_string<- knitr::image_uri(tmpfile)
  sprintf("![Plot](%s)", base64_string)
}

#' #' @rdname block_to_markdown
#' #' @method block_to_markdown data.frame
#' #' @exportS3Method teal::block_to_markdown
#' block_to_markdown.data.frame <- function(x) {
#'   paste(as.character(knitr::kable(x)), collapse = "\n")
#'   # I am not sure it renders the table, but it's here to assure it has length 1.
#' }
#'
#' #' @rdname block_to_markdown
#' #' @method block_to_markdown rtable
#' #' @exportS3Method teal::block_to_markdown
#' block_to_markdown.rtable <- function(x) {
#'   rtables::as_html(x)
#' }
#'
#' #' @rdname block_to_markdown
#' #' @method block_to_markdown ElementaryTable
#' #' @exportS3Method teal::block_to_markdown
#' block_to_markdown.ElementaryTable <- function(x) {
#'   rtables::as_html(x) # to_flextable
#' }
