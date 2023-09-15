#' Get bootstrap current version
#' @note will work properly mainly inside a tag `.renderHook`
#' @keywords internal
get_bs_version <- function() {
  theme <- bslib::bs_current_theme()
  if (bslib::is_bs_theme(theme)) {
    bslib::theme_version(theme)
  } else {
    "3"
  }
}

#' Panel group widget
#' @md
#'
#' @description `r lifecycle::badge("experimental")`
#' @param title (`character`)\cr title of panel
#' @param ... content of panel
#' @param collapsed (`logical`, optional)\cr
#'  whether to initially collapse panel
#' @param input_id (`character`, optional)\cr
#'  name of the panel item element. If supplied, this will register a shiny input variable that
#'  indicates whether the panel item is open or collapsed and is accessed with `input$input_id`.
#'
#' @return (`shiny.tag`)
#'
#' @keywords internal
panel_item <- function(title, ..., collapsed = TRUE, input_id = NULL) {
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  div_id <- paste0(input_id, "_div")
  panel_id <- paste0(input_id, "_panel_body_", sample(1:10000, 1))


  shiny::tags$div(.renderHook = function(res_tag) {
    bs_version <- get_bs_version()

    # alter tag structure
    if (bs_version == "3") {
      res_tag$children <- list(
        shiny::tags$div(
          class = "panel panel-default",
          shiny::tags$div(
            id = div_id,
            class = paste("panel-heading", ifelse(collapsed, "collapsed", "")),
            `data-toggle` = "collapse",
            href = paste0("#", panel_id),
            `aria-expanded` = ifelse(collapsed, "false", "true"),
            shiny::icon("angle-down", class = "dropdown-icon"),
            shiny::tags$label(
              class = "panel-title inline",
              title,
            )
          ),
          shiny::tags$div(
            class = paste("panel-collapse collapse", ifelse(collapsed, "", "in")),
            id = panel_id,
            shiny::tags$div(
              class = "panel-body",
              ...
            )
          )
        )
      )
    } else if (bs_version %in% c("4", "5")) {
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
    } else {
      stop("Bootstrap 3, 4, and 5 are supported.")
    }

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

#' Convert content into a `flextable`, merge cells with `colspan` > 1
#' align columns to the center, and row names to the left
#' Indent the row names by 10 times indentation
#'
#' @param content Supported formats: `data.frame`, `rtables`, `TableTree`, `ElementaryTable`

#' @return (`flextable`)
#'
#' @keywords internal
to_flextable <- function(content) {
  if (inherits(content, c("rtables", "TableTree", "ElementaryTable"))) {
    mf <- rtables::matrix_form(content)
    nr_header <- attr(mf, "nrow_header")
    non_total_coln <- c(TRUE, !grepl("All Patients", names(content)))
    df <- as.data.frame(mf$strings[(nr_header + 1):(nrow(mf$strings)), , drop = FALSE])
    header_df <- as.data.frame(mf$strings[1:nr_header, , drop = FALSE])

    ft <- flextable::flextable(df)
    ft <- flextable::delete_part(ft, part = "header")
    ft <- flextable::add_header(ft, values = header_df)

    merge_index_body <- get_merge_index(mf$spans[seq(nr_header + 1, nrow(mf$spans)), , drop = FALSE])
    merge_index_header <- get_merge_index(mf$spans[seq_len(nr_header), , drop = FALSE])

    ft <- merge_at_indice(ft, lst = merge_index_body, part = "body")
    ft <- merge_at_indice(ft, lst = merge_index_header, part = "header")
    ft <- flextable::align_text_col(ft, align = "center", header = TRUE)
    ft <- flextable::align(ft, i = seq_len(nrow(content)), j = 1, align = "left")
    ft <- padding_lst(ft, mf$row_info$indent)
    ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
    ft <- flextable::autofit(ft, add_h = 0)

    width_vector <- c(
      dim(ft)$widths[1],
      rep(sum(dim(ft)$widths[-1]), length(dim(ft)$widths) - 1) / (ncol(mf$strings) - 1)
    )
    ft <- flextable::width(ft, width = width_vector)
  } else if (inherits(content, "data.frame")) {
    ft <- flextable::flextable(content)
  } else {
    stop(paste0("Unsupported class `(", format(class(content)), ")` when exporting table")
  }

  ft <- custom_theme(ft)
  if (flextable::flextable_dim(ft)$widths > 10) {
    pgwidth <- 10.5
    width_vector <- dim(ft)$widths * pgwidth / flextable::flextable_dim(ft)$widths
    ft <- flextable::width(ft, width = width_vector)
  }

  ft
}

#' @noRd
custom_theme <- function(ft) {
  ft <- flextable::fontsize(ft, size = 8, part = "body")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::hline(ft, border = flextable::fp_border_default(width = 1, color = "grey"))
  ft <- flextable::border_outer(ft)
  ft
}

#' @noRd
get_merge_index_single <- function(span) {
  ret <- list()
  j <- 1
  while (j < length(span)) {
    if (span[j] != 1) {
      ret <- c(ret, list(j:(j + span[j] - 1)))
    }
    j <- j + span[j]
  }
  return(ret)
}

#' @noRd
#'
#' @keywords internal
get_merge_index <- function(spans) {
  ret <- lapply(seq_len(nrow(spans)), function(i) {
    ri <- spans[i, ]
    r <- get_merge_index_single(ri)
    lapply(r, function(s) {
      list(j = s, i = i)
    })
  })
  unlist(ret, recursive = FALSE, use.names = FALSE)
}

#' @noRd
#'
#' @keywords internal
merge_at_indice <- function(ft, lst, part) {
  Reduce(function(ft, ij) {
    flextable::merge_at(ft, i = ij$i, j = ij$j, part = part)
  }, lst, ft)
}

#' @noRd
#'
#' @keywords internal
padding_lst <- function(ft, indents) {
  Reduce(function(ft, s) {
    flextable::padding(ft, s, 1, padding.left = (indents[s] + 1) * 10)
  }, seq_len(length(indents)), ft)
}
