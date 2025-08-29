.content_to_rmd <- function(block, output_dir, ...) {
  path <- tempfile(pattern = "report_item_", fileext = ".rds", tmpdir = output_dir)
  suppressWarnings(saveRDS(block, file = path))
  sprintf("```{r echo = FALSE, eval = TRUE}\nreadRDS('%s')\n```", path)
}

#' Convert `ReporterCard`/`teal_card` content to `rmarkdown`
#'
#' This is an S3 generic that is used to generate content in `rmarkdown` format
#' from various types of blocks in a `ReporterCard` or `teal_card` object.
#'
#' # Customise `to_rmd`
#' The methods for this S3 generic can be extended by the app developer or even overwritten.
#' For this a function with the name `to_rmd.<class>` should be defined in the
#' Global Environment, where `<class>` is the class of the object to be converted.
#'
#' For example, to override the default behavior for `code_chunk` class, you can use:
#'
#' ```r
#' to_rmd.code_chunk <- function(block, output_dir, ..., output_format) {
#'   # custom implementation
#'   sprintf("### A custom code chunk\n\n```{r}\n%s\n```\n", block)
#' }
#' ```
#'
#' Alternatively, you can register the S3 method using `registerS3method("to_rmd", "<class>", fun)`
#'
#' @param block (`any`) content which can be represented in Rmarkdown syntax.
#' @param output_dir (`character(1)`) path to the directory where files should be written to. Beware
#' that absolute paths will break a reproducibility of the Rmarkdown document.
#' @return `character(1)` containing a content or Rmarkdown document.
#' @keywords internal
to_rmd <- function(block, output_dir, ...) {
  checkmate::assert_string(output_dir)
  UseMethod("to_rmd")
}

#' @method to_rmd default
#' @keywords internal
to_rmd.default <- function(block, output_dir, ...) {
  .to_rmd(block, output_dir, ...)
}

.to_rmd <- function(block, output_dir, ...) {
  UseMethod(".to_rmd")
}

#' @method .to_rmd default
#' @keywords internal
.to_rmd.default <- function(block, output_dir, ...) {
  block
}

#' @method .to_rmd teal_report
#' @keywords internal
.to_rmd.teal_report <- function(block, output_dir, ...) {
  to_rmd(teal_card(block), output_dir = output_dir, ...)
}

#' @method .to_rmd teal_card
#' @keywords internal
.to_rmd.teal_card <- function(block, output_dir, global_knitr = getOption("teal.reporter.global_knitr"), ...) {
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  is_powerpoint <- identical(metadata(block)$output, "powerpoint_presentation")
  powerpoint_exception_parsed <- if (is_powerpoint) {
    format_code_block_function <- quote(
      code_block <- function(code_text) {
        df <- data.frame(code_text)
        ft <- flextable::flextable(df)
        ft <- flextable::delete_part(ft, part = "header")
        ft <- flextable::autofit(ft, add_h = 0)
        ft <- flextable::fontsize(ft, size = 7, part = "body")
        ft <- flextable::bg(x = ft, bg = "lightgrey")
        ft <- flextable::border_outer(ft)
        if (flextable::flextable_dim(ft)$widths > 8) {
          ft <- flextable::width(ft, width = 8)
        }
        ft
      }
    )
    deparse1(format_code_block_function, collapse = "\n")
  } else {
    NULL
  }
  global_knitr_parsed <- sprintf(
    "knitr::opts_chunk$set(%s)",
    paste(utils::capture.output(dput(global_knitr)), collapse = "")
  )
  global_knitr_code_chunk <- code_chunk(c(global_knitr_parsed, powerpoint_exception_parsed), include = FALSE)
  global_knitr_rendered <- to_rmd(global_knitr_code_chunk, output_dir = output_dir)

  # we need to prerender global_knitr as code_chunk for powerpoint will wrap it in code_block() call
  blocks_w_global_knitr <- append(
    block,
    if (length(global_knitr) || is_powerpoint) list(global_knitr_rendered),
    after = 0
  )

  m <- metadata(block)
  paste(
    c(
      if (length(m)) sprintf("---\n%s\n---", trimws(yaml::as.yaml(m))),
      unlist(lapply(
        blocks_w_global_knitr,
        function(x) to_rmd(x, output_dir = output_dir, output_format = m$output, ...)
      ))
    ),
    collapse = "\n\n"
  )
}

#' @method .to_rmd code_chunk
#' @keywords internal
.to_rmd.code_chunk <- function(block, output_dir, ..., output_format = NULL) {
  params <- lapply(attr(block, "params"), function(l) if (is.character(l)) shQuote(l) else l)
  block_str <- format(block)
  lang <- attr(block, "lang", exact = TRUE)
  if (identical(output_format, "powerpoint_presentation")) {
    block_content_list <- lapply(
      split_text_block(block, 30),
      function(x, lang) {
        code_block <- sprintf("code_block(\n%s)", shQuote(x, type = "cmd"))
        format(code_chunk(code_block, echo = FALSE, lang = lang))
      },
      lang = lang
    )
    paste(sprintf("\\newpage\n\n---\n\n%s\n", block_content_list), collapse = "\n\n")
  } else {
    format(block)
  }
}

#' @method .to_rmd character
#' @keywords internal
.to_rmd.character <- function(block, output_dir, ...) {
  block
}

#' @method .to_rmd chunk_output
#' @keywords internal
.to_rmd.chunk_output <- function(block, output_dir, ..., include_chunk_output) {
  if (!missing(include_chunk_output) && isTRUE(include_chunk_output)) {
    to_rmd(block[[1]], output_dir = output_dir, ..., include_chunk_output = include_chunk_output)
  }
}

#' @method .to_rmd condition
#' @keywords internal
.to_rmd.condition <- function(block, output_dir, ...) {
  conditionMessage(block)
}

#' @method .to_rmd gg
#' @keywords internal
.to_rmd.gg <- .content_to_rmd

#' @method .to_rmd trellis
#' @keywords internal
.to_rmd.trellis <- .content_to_rmd

#' @method .to_rmd recordedplot
#' @keywords internal
.to_rmd.recordedplot <- .content_to_rmd

#' @method .to_rmd grob
#' @keywords internal
.to_rmd.grob <- .content_to_rmd

#' @method .to_rmd Heatmap
#' @keywords internal
.to_rmd.Heatmap <- .content_to_rmd

#' @method .to_rmd datatables
#' @keywords internal
.to_rmd.datatables <- .content_to_rmd

#' @method .to_rmd summary.lm
#' @keywords internal
.to_rmd.summary.lm <- .content_to_rmd

#' @method .to_rmd rtables
#' @keywords internal
.to_rmd.rtables <- function(block, output_dir, ...) {
  flextable_block <- to_flextable(block)
  attr(flextable_block, "keep") <- attr(block, "keep")
  to_rmd(flextable_block, output_dir, ...)
}

#' @method .to_rmd flextable
#' @keywords internal
.to_rmd.flextable <- .content_to_rmd

#' @method .to_rmd TableTree
#' @keywords internal
.to_rmd.TableTree <- .to_rmd.rtables

#' @method .to_rmd ElementaryTable
#' @keywords internal
.to_rmd.ElementaryTable <- .to_rmd.rtables

#' @method .to_rmd rlisting
#' @keywords internal
.to_rmd.rlisting <- .to_rmd.rtables

#' @method .to_rmd data.frame
#' @keywords internal
.to_rmd.data.frame <- .to_rmd.rtables

#' @method .to_rmd gtsummary
#' @keywords internal
.to_rmd.gtsummary <- function(block, output_dir, ...) {
  to_rmd(gtsummary::as_flex_table(block), output_dir = output_dir, ...)
}
