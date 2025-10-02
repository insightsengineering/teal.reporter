#' Convert content into a `flextable`
#'
#' Converts supported table formats into a `flextable` for enhanced formatting and presentation.
#'
#' Function merges cells with `colspan` > 1,
#' aligns columns to the center and row names to the left,
#' indents the row names by 10 times indentation.
#'
#' @param content Supported formats: `data.frame`, `rtables`, `TableTree`, `ElementaryTable`, `listing_df`
#'
#' @return `flextable`.
#'
#' @keywords internal
to_flextable <- function(content) {
  if (inherits(content, c("rtables", "TableTree", "ElementaryTable"))) {
    ft <- rtables.officer::tt_to_flextable(content)
  } else if (inherits(content, "listing_df")) {
    mf <- rlistings::matrix_form(content)
    nr_header <- attr(mf, "nrow_header")
    df <- as.data.frame(mf$strings[seq(nr_header + 1, nrow(mf$strings)), , drop = FALSE])
    header_df <- as.data.frame(mf$strings[seq_len(nr_header), , drop = FALSE])

    ft <- rtables::df_to_tt(df)
    if (length(mf$main_title) != 0) {
      rtables::main_title(ft) <- mf$main_title
    }
    rtables::subtitles(ft) <- mf$subtitles
    rtables::main_footer(ft) <- mf$main_footer
    rtables::prov_footer(ft) <- mf$prov_footer
    rtables::header_section_div(ft) <- mf$header_section_div
    ft <- rtables.officer::tt_to_flextable(ft, total_width = c(grDevices::pdf.options()$width - 1))
  } else if (inherits(content, "data.frame")) {
    ft <- if (nrow(content) == 0) {
      flextable::flextable(content)
    } else {
      rtables.officer::tt_to_flextable(
        rtables::df_to_tt(content)
      )
    }
  } else {
    stop(paste0("Unsupported class `(", format(class(content)), ")` when exporting table"))
  }

  ft
}

#' Get the merge index for a single span.
#' This function retrieves the merge index for a single span,
#' which is used in merging cells.
#' @noRd
#' @keywords internal
get_merge_index_single <- function(span) {
  ret <- list()
  j <- 1
  while (j < length(span)) {
    if (span[j] != 1) {
      ret <- c(ret, list(seq(j, j + span[j] - 1)))
    }
    j <- j + span[j]
  }
  ret
}

#' Divide text block into smaller blocks
#'
#' Split a text block into smaller blocks with a specified number of lines.
#'
#' A single character string containing a text block of multiple lines (separated by `\n`)
#' is split into multiple strings with n or less lines each.
#'
#' @param x (`character`) string containing the input block of text
#' @param n (`integer`) number of lines per block
#'
#' @return
#' List of character strings with up to `n` lines in each element.
#'
#' @keywords internal
split_text_block <- function(x, n) {
  checkmate::assert_string(x)
  checkmate::assert_integerish(n, lower = 1L, len = 1L)

  lines <- strsplit(x, "\n")[[1]]

  if (length(lines) <= n) {
    return(list(x))
  }

  nblocks <- ceiling(length(lines) / n)
  ind <- rep(1:nblocks, each = n)[seq_along(lines)]
  unname(lapply(split(lines, ind), paste, collapse = "\n"))
}

#' Retrieve text details for global_knitr options
#' This function returns a character string describing the default settings for the global_knitr options.
#' @noRd
#' @keywords internal
global_knitr_details <- function() {
  paste0(
    c(
      " To access the default values for the `global_knitr` parameter,",
      "  use `getOption('teal.reporter.global_knitr')`. These defaults include:",
      " - `echo = TRUE`",
      " - `tidy.opts = list(width.cutoff = 60)`",
      " - `tidy = TRUE`  if `formatR` package is installed, `FALSE` otherwise"
    ),
    collapse = "\n"
  )
}

#' @export
#' @keywords internal
format.code_chunk <- function(x, ...) {
  language <- attr(x, "lang", exact = TRUE)
  params <- attr(x, "params", exact = TRUE)
  if (language %in% names(knitr::knit_engines$get())) {
    sprintf(
      "```{%s}\n%s\n```",
      toString(c(language, paste(names(params), params, sep = "="))),
      NextMethod()
    )
  } else {
    sprintf("```%s\n%s\n```", language, NextMethod())
  }
}

#' Teal action button that is disabled while busy
#'
#' @inheritParams bslib::input_task_button
#' @param id (`character(1)`) the id of the button.
#' @param label (`character(1)`) the label of the button.
#' @param icon (`character(1)` or `NULL`) the name of the Bootstrap icon to be
#' displayed on the button.
#' @param additional_class (`character(1)` or `NULL`) additional CSS class to be
#' added to the button.
#'
#' @return A `shiny` action button that is disabled while busy.
#' @keywords internal
.action_button_busy <- function(id,
                                label,
                                icon = NULL,
                                type = "primary",
                                outline = FALSE,
                                additional_class = NULL) {
  checkmate::assert_string(type)
  checkmate::assert_string(additional_class, null.ok = TRUE)
  shiny::tagList(
    shinyjs::useShinyjs(),
    .custom_css_dependency("outline_button.css"),
    .custom_js_dependency("busy-disable.js", name = "teal-reporter-busy-disable"),
    shiny::tags$button(
      id = id,
      class = c(
        "teal-reporter action-button teal-reporter-busy-disable",
        sprintf("btn btn-%1$s %1$s", trimws(type)),
        if(isTRUE(outline)) "outline-button",
        additional_class
      ),
      role = "button",
      style = "text-decoration: none;",
      if (!is.null(icon)) {
        margin_style <- ifelse(is.null(label), "margin: 0 10px 0 10px;", "")
        shiny::tags$span(
          style = margin_style,
          bsicons::bs_icon(icon, class = sprintf("text-%s", type))
        )
      },
      label
    )
  )
}

#' @keywords internal
.custom_js_dependency <- function(script, name = sprintf("teal-reporter-%s", script)) {
  htmltools::htmlDependency(
    name = name,
    version = utils::packageVersion("teal.reporter"),
    package = "teal.reporter",
    src = "js",
    script = script
  )
}

#' @keywords internal
.custom_css_dependency <- function(stylesheet = "custom.css", name = sprintf("teal-reporter-%s", stylesheet)) {
  checkmate::assert_string(stylesheet)
  htmltools::htmlDependency(
    name = name,
    version = utils::packageVersion("teal.reporter"),
    package = "teal.reporter",
    src = "css",
    stylesheet = stylesheet
  )
}

#' @keywords internal
.accordion_toggle_js_dependency <- function() { # nolint object_length_linter.
  .custom_js_dependency("accordion-toggle.js", name = "teal-reporter-accordion-toggle")
}

#' @noRd
dummy <- function() {
  R6::R6Class # Used to trick R CMD check for avoiding NOTE about R6
  jsonlite::fromJSON # Used to trick R CMD check for not detecting jsonlite usage
}
