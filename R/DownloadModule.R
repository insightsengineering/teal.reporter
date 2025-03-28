#' Download report button module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Provides a button that triggers downloading a report.
#'
#' For more information, refer to the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name download_report_button
#'
#' @param id (`character(1)`) this `shiny` module's id.
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname download_report_button
#' @export
download_report_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::actionButton(
      ns("download_button"),
      class = "teal-reporter simple_report_button btn-primary",
      title = "Download",
      `data-val` = shiny::restoreInput(id = ns("download_button"), default = NULL),
      shiny::tags$span(
        shiny::icon("download")
      )
    )
  )
}

#' @rdname download_report_button
#' @export
download_report_button_srv <- function(id,
                                       reporter,
                                       global_knitr = getOption("teal.reporter.global_knitr"),
                                       rmd_output = c(
                                         "html" = "html_document", "pdf" = "pdf_document",
                                         "powerpoint" = "powerpoint_presentation", "word" = "word_document"
                                       ),
                                       rmd_yaml_args = list(
                                         author = "NEST", title = "Report",
                                         date = as.character(Sys.Date()), output = "html_document",
                                         toc = FALSE
                                       )) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c(
      "html_document", "pdf_document",
      "powerpoint_presentation", "word_document"
    ),
    empty.ok = FALSE
  )
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_names(
    names(rmd_yaml_args),
    subset.of = c("author", "title", "date", "output", "toc"),
    must.include = "output"
  )
  checkmate::assert_true(rmd_yaml_args[["output"]] %in% rmd_output)

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("download_button"))

    ns <- session$ns

    download_modal <- function() {
      nr_cards <- length(reporter$get_cards())
      downb <- shiny::tags$a(
        id = ns("download_data"),
        class = paste("btn btn-primary shiny-download-link", if (nr_cards) NULL else "disabled"),
        style = if (nr_cards) NULL else "pointer-events: none;",
        href = "",
        target = "_blank",
        download = NA,
        shiny::icon("download"),
        "Download"
      )
      shiny::tags$div(
        class = "teal-widgets reporter-modal",
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Download the Report"),
          shiny::tags$hr(),
          if (length(reporter$get_cards()) == 0) {
            shiny::tags$div(
              class = "mb-4",
              shiny::tags$p(
                class = "text-danger",
                shiny::tags$strong("No Cards Added")
              )
            )
          } else {
            shiny::tags$div(
              class = "mb-4",
              shiny::tags$p(
                class = "text-success",
                shiny::tags$strong(paste("Number of cards: ", nr_cards))
              ),
            )
          },
          reporter_download_inputs(
            rmd_yaml_args = rmd_yaml_args,
            rmd_output = rmd_output,
            showrcode = any_rcode_block(reporter),
            session = session
          ),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            downb
          )
        )
      )
    }

    shiny::observeEvent(input$download_button, {
      shiny::showModal(download_modal())
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "report_",
          if (reporter$get_id() == "") NULL else paste0(reporter$get_id(), "_"),
          format(Sys.time(), "%y%m%d%H%M%S"),
          ".zip"
        )
      },
      content = function(file) {
        shiny::showNotification("Rendering and Downloading the document.")
        shinybusy::block(id = ns("download_data"), text = "", type = "dots")
        input_list <- lapply(names(rmd_yaml_args), function(x) input[[x]])
        names(input_list) <- names(rmd_yaml_args)
        if (is.logical(input$showrcode)) global_knitr[["echo"]] <- input$showrcode
        report_render_and_compress(reporter, input_list, global_knitr, file)
        shinybusy::unblock(id = ns("download_data"))
      },
      contentType = "application/zip"
    )
  })
}

#' Render the report
#'
#' Render the report and zip the created directory.
#'
#' @param reporter (`Reporter`) instance.
#' @param input_list (`list`) like `shiny` input converted to a regular named list.
#' @param global_knitr (`list`) a global `knitr` parameters, like echo.
#' But if local parameter is set it will have priority.
#' @param file (`character(1)`) where to copy the returned directory.
#'
#' @return `file` argument, invisibly.
#'
#' @keywords internal
report_render_and_compress <- function(reporter, input_list, global_knitr, file = tempdir()) {
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_list(input_list, names = "named")
  checkmate::assert_string(file)

  if (
    identical("pdf_document", input_list$output) &&
      inherits(try(system2("pdflatex", "--version", stdout = TRUE), silent = TRUE), "try-error")
  ) {
    shiny::showNotification(
      ui = "pdflatex is not available so the pdf_document could not be rendered. Please use other output type.",
      action = "Please contact app developer",
      type = "error"
    )
    stop("pdflatex is not available so the pdf_document could not be rendered.")
  }

  yaml_header <- as_yaml_auto(input_list)

  tryCatch(
    output_dir <- report_render(reporter$get_blocks(), yaml_header, global_knitr),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Render document warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Render document error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  tryCatch(
    reporter$to_jsondir(output_dir),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Archive document warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Archive document error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  temp_zip_file <- tempfile(fileext = ".zip")
  tryCatch(
    expr = zip::zipr(temp_zip_file, output_dir),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Zipping folder warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Zipping folder error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  tryCatch(
    expr = file.copy(temp_zip_file, file),
    warning = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Copying file warning!",
        action = "Please contact app developer",
        type = "warning"
      )
    },
    error = function(cond) {
      print(cond)
      shiny::showNotification(
        ui = "Copying file error!",
        action = "Please contact app developer",
        type = "error"
      )
    }
  )

  invisible(file)
}

#' Get the custom list of UI inputs
#'
#' @param rmd_output (`character`) vector with `rmarkdown` output types,
#' by default all possible `pdf_document`, `html_document`, `powerpoint_presentation`, and `word_document`.
#' If vector is named then those names will appear in the `UI`.
#' @param rmd_yaml_args (`named list`) with `Rmd` `yaml` header fields and their default values.
#' This `list` will result in the custom subset of UI inputs for the download reporter functionality.
#' Default `list(author = "NEST", title = "Report", date = Sys.Date(), output = "html_document", toc = FALSE)`.
#' The `list` must include at least `"output"` field.
#' The default value for `"output"` has to be in the `rmd_output` argument.
#'
#' @keywords internal
reporter_download_inputs <- function(rmd_yaml_args, rmd_output, showrcode, session) {
  shiny::tagList(
    lapply(names(rmd_yaml_args), function(e) {
      switch(e,
        author = shiny::textInput(session$ns("author"), label = "Author:", value = rmd_yaml_args$author),
        title = shiny::textInput(session$ns("title"), label = "Title:", value = rmd_yaml_args$title),
        date = shiny::dateInput(session$ns("date"), "Date:", value = rmd_yaml_args$date),
        output = shiny::tags$div(
          shinyWidgets::pickerInput(
            inputId = session$ns("output"),
            label = "Choose a document type: ",
            choices = rmd_output,
            selected = rmd_yaml_args$output
          )
        ),
        toc = shiny::checkboxInput(session$ns("toc"), label = "Include Table of Contents", value = rmd_yaml_args$toc)
      )
    }),
    if (showrcode) {
      shiny::checkboxInput(
        session$ns("showrcode"),
        label = "Include R Code",
        value = FALSE
      )
    }
  )
}

#' @noRd
#' @keywords internal
any_rcode_block <- function(reporter) {
  cards <- reporter$get_cards()
  if (all(vapply(cards, inherits, logical(1), "ReportCard"))) {
    any(
      vapply(
        reporter$get_blocks(),
        function(e) inherits(e, "RcodeBlock"),
        logical(1)
      )
    )
  } else {
    FALSE
  }
}



report_render <- function(blocks, yaml_header, global_knitr = getOption("teal.reporter.global_knitr"), ...) {
  tmp_dir <- tempdir()
  output_dir <- file.path(tmp_dir, sprintf("report_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
  dir.create(path = output_dir)

  args <- list(...)

  # Create output file with report, code and outputs
  input_path <- report_to_rmd(blocks, yaml_header, global_knitr, output_dir, include_echo = TRUE)
  args <- append(args, list(
    input = input_path,
    output_dir = output_dir,
    output_format = "all",
    quiet = TRUE
  ))
  args_nams <- unique(names(args))
  args <- lapply(args_nams, function(x) args[[x]])
  names(args) <- args_nams

  do.call(rmarkdown::render, args)
  file.remove(input_path)

  # Create .Rmd file
  report_to_rmd(blocks, yaml_header, global_knitr, output_dir, include_echo = FALSE) #TODO remove eval=FALSE also
  output_dir
}

report_to_rmd <-
  function(blocks, yaml_header, global_knitr = getOption("teal.reporter.global_knitr"), output_dir, include_echo) {
  checkmate::assert_list(
    blocks,
    c("TextBlock", "PictureBlock", "NewpageBlock", "TableBlock", "RcodeBlock", "HTMLBlock", "character",
      "gg", "rtables", "TableTree", "ElementaryTable", "rlisting", "data.frame")
  )
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  if (missing(yaml_header)) {
    yaml_header <- md_header(yaml::as.yaml(list(title = "Report")))
  }

  report_type <- get_yaml_field(yaml_header, "output")

  parsed_global_knitr <- sprintf(
    "\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(%s)\n%s\n```\n",
    capture.output(dput(global_knitr)),
    if (identical(report_type, "powerpoint_presentation")) {
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
      paste(deparse(format_code_block_function), collapse = "\n")
    } else {
      ""
    }
  )

  parsed_blocks <- paste(
    unlist(
      lapply(blocks,
        function(b) block_to_rmd(b, output_dir = output_dir, report_type = report_type, include_echo = include_echo)
      )
    ),
    collapse = "\n\n"
  )

  rmd_text <- paste0(yaml_header, "\n", parsed_global_knitr, "\n", parsed_blocks, "\n")
  tmp <- tempfile(fileext = ".Rmd")
  input_path <- file.path(
    output_dir,
    sprintf("input_%s.Rmd", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")))
  )
  cat(rmd_text, file = input_path)
  input_path
}

#' @keywords internal
block_to_rmd <- function(block, output_dir, ...) {
  UseMethod("block_to_rmd")
}

#' @method block_to_rmd default
#' @keywords internal
block_to_rmd.default <- function(block, output_dir, ...) {
  block
}

#' @method block_to_rmd TextBlock
#' @keywords internal
block_to_rmd.TextBlock <- function(block, output_dir, ...) {
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

#' @method block_to_rmd RcodeBlock
#' @keywords internal
block_to_rmd.RcodeBlock <- function(block, output_dir, ..., report_type) {
  params <- block$get_params()
  params <- lapply(params, function(l) if (is.character(l)) shQuote(l) else l)
  if (identical(report_type, "powerpoint_presentation")) {
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

#' @method block_to_rmd code_chunk
#' @keywords internal
block_to_rmd.code_chunk <- function(block, output_dir, ..., include_echo, report_type, eval = FALSE) {

  if (include_echo || !isFALSE(attr(block, "keep"))) {
    params <- attr(block, "params")
    if (!('eval' %in% names(params))) params <- c(params, eval = eval)
    params <- lapply(params, function(l) if (is.character(l)) shQuote(l) else l)
    if (identical(report_type, "powerpoint_presentation")) {
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
        "```{r, %s}\n%s\n```\n",
        paste(names(params), params, sep = "=", collapse = ", "),
        block
      )
    }
  }
}

#' @method block_to_rmd PictureBlock
#' @keywords internal
block_to_rmd.PictureBlock <- function(block, output_dir, ...) {
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

#' @method block_to_rmd TableBlock
#' @keywords internal
block_to_rmd.TableBlock <- function(block, output_dir, ...) {
  basename_table <- basename(block$get_content())
  file.copy(block$get_content(), file.path(output_dir, basename_table))
  sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename_table)
}

#' @method block_to_rmd NewpageBlock
#' @keywords internal
block_to_rmd.NewpageBlock <- function(block, output_dir, ...) {
  block$get_content()
}

#' @method block_to_rmd HTMLBlock
#' @keywords internal
block_to_rmd.HTMLBlock <- function(block, output_dir, ...) {
  basename <- basename(tempfile(fileext = ".rds"))
  suppressWarnings(saveRDS(block$get_content(), file = file.path(output_dir, basename)))
  sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename)
}

#' @method block_to_rmd character
#' @keywords internal
block_to_rmd.character <- function(block, output_dir, ..., include_echo) {
  if (include_echo || !isFALSE(attr(block, "keep"))) {
    block
  }
}

#' @method block_to_rmd gg
#' @keywords internal
block_to_rmd.gg <- function(block, output_dir, ..., include_echo) {
  content_to_rmd(block, output_dir, include_echo)
}

#' @method block_to_rmd rtables
#' @keywords internal
block_to_rmd.rtables <- function(block, output_dir, ..., include_echo) {
  flextable_block <- to_flextable(block)
  attr(flextable_block, "keep") <- attr(block, "keep")
  content_to_rmd(flextable_block, output_dir, include_echo)
}

#' @method block_to_rmd TableTree
#' @keywords internal
block_to_rmd.TableTree <- block_to_rmd.rtables

#' @method block_to_rmd ElementaryTable
#' @keywords internal
block_to_rmd.ElementaryTable <- block_to_rmd.rtables

#' @method block_to_rmd rlisting
#' @keywords internal
block_to_rmd.rlisting <- block_to_rmd.rtables

#' @method block_to_rmd data.frame
#' @keywords internal
block_to_rmd.data.frame <- block_to_rmd.rtables

content_to_rmd = function(content, output_dir, include_echo) {
  if (include_echo || isTRUE(attr(content, "keep"))) {
    suppressWarnings(hashname <- rlang::hash(content))
    hashname_file <- paste0(hashname, ".rds")
    path <- tempfile(fileext = ".rds")
    suppressWarnings(saveRDS(content, file = path))
    file.copy(path, file.path(output_dir, hashname_file))
    sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", hashname_file)
  }
}
