#' @title `Renderer`
#' @docType class
#' @keywords internal
Renderer <- R6::R6Class( # nolint: object_name_linter.
  classname = "Renderer",
  public = list(
    #' @description Returns a `Renderer` object.
    #'
    #' @details Initialize a `Renderer` object.
    #'
    #' @return Object of class `Renderer`, invisibly.
    #' @examples
    #' Renderer <- getFromNamespace("Renderer", "teal.reporter")
    #' Renderer$new()
    #'
    initialize = function() {
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("report_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      private$output_dir <- output_dir
      invisible(self)
    },
    #' @description Finalizes a `Renderer` object.
    finalize = function() {
      unlink(private$output_dir, recursive = TRUE)
    },
    #' @description getting the `Rmd` text which could be easily rendered later.
    #'
    #' @param blocks (`list`) of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header (`character`) an `rmarkdown` `yaml` header.
    #' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
    #'  for customizing the rendering process.
    #' @details `r global_knitr_details()`
    #'
    #' @return `character` a `Rmd` text (`yaml` header + body), ready to be rendered.
    #' @examples
    #' library(yaml)
    #' library(rtables)
    #' library(ggplot2)
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #' card2$append_rcode("2+2", echo = FALSE)
    #'
    #' Reporter <- getFromNamespace("Reporter", "teal.reporter")
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' yaml_quoted <- getFromNamespace("yaml_quoted", "teal.reporter")
    #' yaml_l <- list(
    #'   author = yaml_quoted("NEST"),
    #'   title = yaml_quoted("Report"),
    #'   date = yaml_quoted("07/04/2019"),
    #'   output = list(html_document = list(toc = FALSE))
    #' )
    #'
    #' md_header <- getFromNamespace("md_header", "teal.reporter")
    #' yaml_header <- md_header(as.yaml(yaml_l))
    #' Renderer <- getFromNamespace("Renderer", "teal.reporter")
    #' result_path <- Renderer$new()$renderRmd(reporter$get_blocks(), yaml_header)
    #'
    renderRmd = function(blocks, yaml_header, global_knitr = getOption("teal.reporter.global_knitr")) {
      checkmate::assert_list(blocks, c("TextBlock", "PictureBlock", "NewpageBlock", "TableBlock", "RcodeBlock"))
      checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))

      if (missing(yaml_header)) {
        yaml_header <- md_header(yaml::as.yaml(list(title = "Report")))
      }

      private$report_type <- get_yaml_field(yaml_header, "output")

      parsed_global_knitr <- sprintf(
        "\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(%s)\n%s\n```\n",
        capture.output(dput(global_knitr)),
        if (identical(private$report_type, "powerpoint_presentation")) {
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
          lapply(blocks, function(b) private$block2md(b))
        ),
        collapse = "\n\n"
      )

      rmd_text <- paste0(yaml_header, "\n", parsed_global_knitr, "\n", parsed_blocks, "\n")
      tmp <- tempfile(fileext = ".Rmd")
      input_path <- file.path(
        private$output_dir,
        sprintf("input_%s.Rmd", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")))
      )
      cat(rmd_text, file = input_path)
      input_path
    },
    #' @description Renders the content of this `Report` to the output file.
    #'
    #' @param blocks (`list`) of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header (`character`) an `rmarkdown` `yaml` header.
    #' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
    #'  for customizing the rendering process.
    #' @param ... `rmarkdown::render` arguments, `input` and `output_dir` should not be updated.
    #' @details `r global_knitr_details()`
    #'
    #' @return `character` path to the output.
    #' @examples
    #' library(yaml)
    #' library(ggplot2)
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #' card2$append_rcode("2+2", echo = FALSE)
    #' Reporter <- getFromNamespace("Reporter", "teal.reporter")$new()
    #' Reporter$append_cards(list(card1, card2))
    #'
    #' yaml_quoted <- getFromNamespace("yaml_quoted", "teal.reporter")
    #' yaml_l <- list(
    #'   author = yaml_quoted("NEST"),
    #'   title = yaml_quoted("Report"),
    #'   date = yaml_quoted("07/04/2019"),
    #'   output = list(html_document = list(toc = FALSE))
    #' )
    #'
    #' md_header <- getFromNamespace("md_header", "teal.reporter")
    #' yaml_header <- md_header(as.yaml(yaml_l))
    #' Renderer <- getFromNamespace("Renderer", "teal.reporter")
    #' result_path <- Renderer$new()$render(Reporter$get_blocks(), yaml_header)
    #'
    render = function(blocks, yaml_header, global_knitr = getOption("teal.reporter.global_knitr"), ...) {
      args <- list(...)
      input_path <- self$renderRmd(blocks, yaml_header, global_knitr)
      args <- append(args, list(
        input = input_path,
        output_dir = private$output_dir,
        output_format = "all",
        quiet = TRUE
      ))
      args_nams <- unique(names(args))
      args <- lapply(args_nams, function(x) args[[x]])
      names(args) <- args_nams
      do.call(rmarkdown::render, args)
    },
    #' @description get `output_dir` field.
    #'
    #' @return `character` a `output_dir` field path.
    #' @examples
    #' Renderer <- getFromNamespace("Renderer", "teal.reporter")$new()
    #' Renderer$get_output_dir()
    #'
    get_output_dir = function() {
      private$output_dir
    }
  ),
  private = list(
    output_dir = character(0),
    report_type = NULL,
    # factory method
    block2md = function(block) {
      if (inherits(block, "TextBlock")) {
        private$textBlock2md(block)
      } else if (inherits(block, "RcodeBlock")) {
        private$rcodeBlock2md(block)
      } else if (inherits(block, "PictureBlock")) {
        private$pictureBlock2md(block)
      } else if (inherits(block, "TableBlock")) {
        private$tableBlock2md(block)
      } else if (inherits(block, "NewpageBlock")) {
        block$get_content()
      } else {
        stop("Unknown block class")
      }
    },
    # card specific methods
    textBlock2md = function(block) {
      text_style <- block$get_style()
      block_content <- block$get_content()
      switch(text_style,
        "default" = block_content,
        "verbatim" = sprintf("\n```\n%s\n```\n", block_content),
        "header2" = paste0("## ", block_content),
        "header3" = paste0("### ", block_content),
        block_content
      )
    },
    rcodeBlock2md = function(block) {
      params <- block$get_params()
      params <- lapply(params, function(l) if (is.character(l)) shQuote(l) else l)
      if (identical(private$report_type, "powerpoint_presentation")) {
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
    },
    pictureBlock2md = function(block) {
      basename_pic <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_pic))
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
    },
    tableBlock2md = function(block) {
      basename_table <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_table))
      sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename_table)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
