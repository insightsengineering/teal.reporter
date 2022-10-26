#' @title `Renderer`
#' @keywords internal
Renderer <- R6::R6Class( # nolint: object_name_linter.
  classname = "Renderer",
  public = list(
    #' @description Returns a `Renderer` object.
    #'
    #' @details Returns a `Renderer` object.
    #'
    #' @return `Renderer` object.
    #' @examples
    #' renderer <- teal.reporter:::Renderer$new()
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
    #' @param blocks `list` of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header `character` a `rmarkdown` `yaml` header.
    #' @return `character` a `Rmd` text (`yaml` header + body), ready to be rendered.
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' yaml_l <- list(
    #'   author = teal.reporter:::yaml_quoted("NEST"),
    #'   title = teal.reporter:::yaml_quoted("Report"),
    #'   date = teal.reporter:::yaml_quoted("07/04/2019"),
    #'   output = list(html_document = list(toc = FALSE))
    #' )
    #'
    #' yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
    #' result_path <- teal.reporter:::Renderer$new()$renderRmd(reporter$get_blocks(), yaml_header)
    renderRmd = function(blocks, yaml_header) {
      checkmate::assert_list(blocks, c("TextBlock", "PictureBlock", "NewpageBlock", "TableBlock"))
      if (missing(yaml_header)) {
        yaml_header <- md_header(yaml::as.yaml(list(title = "Report")))
      }

      parsed_yaml <- yaml_header
      parsed_blocks <- paste(unlist(lapply(blocks, function(b) private$block2md(b))), collapse = "\n\n")

      rmd_text <- paste0(parsed_yaml, "\n", parsed_blocks, "\n")
      tmp <- tempfile(fileext = ".Rmd")
      input_path <- file.path(
        private$output_dir,
        sprintf("input_%s.Rmd", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")))
      )
      cat(rmd_text, file = input_path)
      input_path
    },
    #' @description Renders the content of this `Report` to the output file
    #'
    #' @param blocks `list` of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header `character` an `rmarkdown` `yaml` header.
    #' @param ... `rmarkdown::render` arguments, `input` and `output_dir` should not be updated.
    #' @return `character` path to the output
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' yaml_l <- list(
    #'   author = teal.reporter:::yaml_quoted("NEST"),
    #'   title = teal.reporter:::yaml_quoted("Report"),
    #'   date = teal.reporter:::yaml_quoted("07/04/2019"),
    #'   output = list(html_document = list(toc = FALSE))
    #' )
    #'
    #' yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
    #' result_path <- teal.reporter:::Renderer$new()$render(reporter$get_blocks(), yaml_header)
    render = function(blocks, yaml_header, ...) {
      args <- list(...)
      input_path <- self$renderRmd(blocks, yaml_header)
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
    #' @description get `output_dir` field
    #'
    #' @return `character` a `output_dir` field path.
    #' @examples
    #' renderer <- teal.reporter:::Renderer$new()
    #' renderer$get_output_dir()
    get_output_dir = function() {
      private$output_dir
    }
  ),
  private = list(
    output_dir = character(0),
    textBlock2md = function(block) {
      text_style <- block$get_style()
      block_content <- block$get_content()
      switch(text_style,
        "default" = block_content,
        "verbatim" = sprintf("\n```\n%s\n```\n", block_content),
        "rcode" =
          sprintf(
            "```{r, echo=isTRUE(params$showrcode), eval=FALSE}\n# Show R Code\n\n%s\n```\n",
            block_content
          ),
        "header2" = paste0("## ", block_content),
        "header3" = paste0("### ", block_content),
        block_content
      )
    },
    pictureBlock2md = function(block) {
      basename_pic <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_pic))
      title <- block$get_title()
      sprintf("![%s](%s){width=%s, height=%s}", title, basename_pic, block$get_dim()[1], block$get_dim()[2]) # nolint
    },
    tableBlock2md = function(block) {
      basename_table <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_table))
      sprintf("```{r echo = FALSE}\nreadRDS('%s')\n```", basename_table)
    },
    block2md = function(block) {
      block_class <- class(block)[1]
      switch(block_class,
        TextBlock = private$textBlock2md(block),
        PictureBlock = private$pictureBlock2md(block),
        TableBlock = private$tableBlock2md(block),
        NewpageBlock = block$get_content(),
        ""
      )
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
