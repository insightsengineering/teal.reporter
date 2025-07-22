#' Panel group widget
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param title (`character`) title of panel
#' @param ... content of panel
#' @param collapsed (`logical`, optional)
#'  whether to initially collapse panel
#' @param input_id (`character`, optional)
#'  name of the panel item element. If supplied, this will register a shiny input variable that
#'  indicates whether the panel item is open or collapsed and is accessed with `input$input_id`.
#'
#' @return `shiny.tag`.
#'
#' @keywords internal
panel_item <- function(title, ..., collapsed = TRUE, input_id = NULL) {
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  div_id <- paste0(input_id, "_div")
  panel_id <- paste0(input_id, "_panel_body_", sample(1:10000, 1))


  shiny::tags$div(.renderHook = function(res_tag) {
    res_tag$children <- list(
      shiny::tags$div(
        class = "card my-2",
        shiny::tags$div(
          class = "card-header",
          shiny::tags$div(
            class = ifelse(collapsed, "collapsed", ""),
            # bs4
            `data-toggle` = "collapse",
            # bs5
            `data-bs-toggle` = "collapse",
            href = paste0("#", panel_id),
            `aria-expanded` = ifelse(collapsed, "false", "true"),
            shiny::icon("angle-down", class = "dropdown-icon"),
            shiny::tags$label(
              class = "card-title inline",
              title,
            )
          )
        ),
        shiny::tags$div(
          id = panel_id,
          class = paste("collapse", ifelse(collapsed, "", "show")),
          shiny::tags$div(
            class = "card-body",
            ...
          )
        )
      )
    )

    shiny::tagList(
      shiny::singleton(
        shiny::tags$head(
          shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter"))
        )
      ),
      res_tag
    )
  })
}

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
    ft <- rtables.officer::tt_to_flextable(
      rtables::df_to_tt(content)
    )
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


#' @keywords internal
.outline_button <- function(id, label, icon = NULL, class = "primary") {
  shiny::tags$a(
    id = id,
    class = sprintf("teal-reporter action-button outline-button %s", class),
    role = "button",
    style = "text-decoration: none;",
    if (!is.null(icon)) {
      bsicons::bs_icon(icon, class = sprintf("text-%s", class))
    },
    label
  )
}
