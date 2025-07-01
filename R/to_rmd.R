.content_to_rmd <- function(block, output_dir, ...) {
  suppressWarnings(hashname <- rlang::hash(block))
  hashname_file <- paste0(hashname, ".rds")
  path <- tempfile(fileext = ".rds")
  suppressWarnings(saveRDS(block, file = path))
  file.copy(path, file.path(output_dir, hashname_file))
  sprintf("```{r echo = FALSE, eval = TRUE}\nreadRDS('%s')\n```", hashname_file)
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
#' @param block (`any`) content which can be represented in `rmarkdown` syntax
#' @param output_dir (`character(1)`) path to the directory where files should be written to. Beware
#' that absolute paths will break a reproducibility of the Rmd document.
#' @return `character(1)` containing a content or `rmarkdown` document
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

#' @method .to_rmd Reporter
#' @keywords internal
.to_rmd.Reporter <- function(block,
                             output_dir,
                             rmd_yaml_args,
                             ...) {
  checkmate::assert_list(rmd_yaml_args, names = "named")
  tc <- as.teal_card(block$get_blocks())
  metadata(tc) <- rmd_yaml_args
  .to_rmd(tc, output_dir = output_dir, ...)
}

#' @method .to_rmd teal_report
#' @keywords internal
.to_rmd.teal_report <- function(block, output_dir, ...) {
  .to_rmd(teal_card(block), output_dir = output_dir, ...)
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

  blocks_w_global_knitr <- append(
    block,
    if (length(global_knitr) || is_powerpoint) list(global_knitr_code_chunk),
    after = 0
  )

  m <- metadata(block)
  paste(
    c(
      if (length(m)) sprintf("---\n%s\n---", trimws(yaml::as.yaml(m))),
      unlist(lapply(
        blocks_w_global_knitr,
        function(x) .to_rmd(x, output_dir = output_dir, output_format = m$output, ...)
      ))
    ),
    collapse = "\n"
  )
}

#' @method .to_rmd TextBlock
#' @keywords internal
.to_rmd.TextBlock <- function(block, output_dir, ...) {
  text_style <- block$get_style()
  block_content <- block$get_content()
  switch(text_style,
    "default" = block_content,
    "verbatim" = sprintf("\n```\n%s\n```\n", block_content),
    "header2" = paste0("## ", block_content),
    "header3" = paste0("### ", block_content),
    block_content
  )
}

#' @method .to_rmd RcodeBlock
#' @keywords internal
.to_rmd.RcodeBlock <- function(block, output_dir, ..., output_format) {
  params <- block$get_params()
  params <- lapply(params, function(l) if (is.character(l)) shQuote(l) else l)
  if (identical(output_format, "powerpoint_presentation")) {
    block_content_list <- split_text_block(block$get_content(), 30)
    paste(
      sprintf(
        "\\newpage\n\n---\n\n```{r, echo=FALSE}\ncode_block(\n%s)\n```\n",
        shQuote(block_content_list, type = "cmd")
      ),
      collapse = "\n\n"
    )
  } else {
    sprintf(
      "\\newpage\n\n--- \n\n```{r, %s}\n%s\n```\n",
      paste(names(params), params, sep = "=", collapse = ", "),
      block$get_content()
    )
  }
}

#' @method .to_rmd code_chunk
#' @keywords internal
.to_rmd.code_chunk <- function(block, output_dir, ..., output_format = NULL) {
  params <- attr(block, "params")
  params <- lapply(params, function(l) if (is.character(l)) shQuote(l) else l)
  if (identical(output_format, "powerpoint_presentation")) {
    block_content_list <- split_text_block(block, 30)
    paste(
      sprintf(
        "\\newpage\n\n---\n\n```{r, echo=FALSE}\ncode_block(\n%s)\n```\n",
        shQuote(block_content_list, type = "cmd")
      ),
      collapse = "\n\n"
    )
  } else {
    sprintf(
      "```{%s}\n%s\n```\n",
      toString(c("r", paste(names(params), params, sep = "="))),
      block
    )
  }
}

#' @method .to_rmd PictureBlock
#' @keywords internal
.to_rmd.PictureBlock <- function(block, output_dir, ...) {
  basename_pic <- basename(block$get_content())
  file.copy(block$get_content(), file.path(output_dir, basename_pic))
  params <- c(
    `out.width` = "'100%'",
    `out.height` = "'100%'"
  )
  title <- block$get_title()
  if (length(title)) params["fig.cap"] <- shQuote(title)
  sprintf(
    "\n```{r, echo = FALSE, %s}\nknitr::include_graphics(path = '%s')\n```\n",
    paste(names(params), params, sep = "=", collapse = ", "),
    basename_pic
  )
}

#' @method .to_rmd TableBlock
#' @keywords internal
.to_rmd.TableBlock <- function(block, output_dir, ...) {
  basename_table <- basename(block$get_content())
  file.copy(block$get_content(), file.path(output_dir, basename_table))
  sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename_table)
}

#' @method .to_rmd NewpageBlock
#' @keywords internal
.to_rmd.NewpageBlock <- function(block, output_dir, ...) {
  block$get_content()
}

#' @method .to_rmd HTMLBlock
#' @keywords internal
.to_rmd.HTMLBlock <- function(block, output_dir, ...) {
  basename <- basename(tempfile(fileext = ".rds"))
  suppressWarnings(saveRDS(block$get_content(), file = file.path(output_dir, basename)))
  sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename)
}

#' @method .to_rmd character
#' @keywords internal
.to_rmd.character <- function(block, output_dir, ...) {
  block
}

#' @method .to_rmd PictureBlock
#' @keywords internal
.to_rmd.chunk_output <- function(block, output_dir, ..., include_chunk_output) {
  if (!missing(include_chunk_output) && isTRUE(include_chunk_output)) {
    NextMethod()
  }
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
  .content_to_rmd(flextable_block, output_dir)
}

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
